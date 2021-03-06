package scalajs_java.compiler

import com.sun.tools.javac.tree.JCTree.Tag
import org.scalajs.core.ir
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.Trees.OptimizerHints
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}

import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.trees._
import scalajs_java.utils._
import scalajs_java.utils.scope.Scope.ClassMapT
import scalajs_java.utils.scope._


/** Main compiler.
  * TODO remove the constructors pass (take that info from `classes`. */
class Compiler(val inits: Map[String, Expr],
               val classes: ClassMapT,
               val constructors: ConstructorsT,
               val errorHanlder: ErrorHandler) {

  var MainObjectFullName: Option[String] = None

  private final def objectClassIdent(implicit pos: Position) =
    irt.Ident(ObjectClass, Some("java.lang.Object"))

  private final val objectClassType = irtpe.ClassType("O")

  var companionObjects: List[irt.ClassDef] = Nil
  
  val opCompiler = new OpCompiler(errorHanlder)

  val mangler = new Mangler

  val typeCompiler = new TypeCompiler(mangler, errorHanlder)
  
  val utils = new Utils(classes, errorHanlder)

  // Compiling constructors

  /** Compiles statement of a constructor body
    *
    * The reason we need this is because Scala(.js) has
    * no explicit `super(...)` call mechanism so we have to
    * compile them separately.
    */
  def compileConstructorStmt(className: irt.Ident, classType: irtpe.ClassType,
      superClassType: irtpe.ClassType, stmt: Statement): irt.Tree = {
    implicit val pos = Utils.getPosition(stmt)

    if (Predicates.isSuperCall(stmt)) {
      stmt match {
        case ExprStatement(MethodInv(_, _, args, _, _)) =>
          val argRefsC = args.map(compileParamRef)
          val argStr = args.map(arg => mangler.mangleType(arg.tp)).mkString("__")
          val constrName = irt.Ident("init___" + argStr)
          irt.ApplyStatically(irt.This()(classType), superClassType, constrName,
            argRefsC)(irtpe.NoType)

        case _ =>
          errorHanlder.fail(pos.line, Some("compileConstructorStmt"),
          "encountered unexpected tree", Normal)
          irt.Null()
      }
    } else {
      compileStatement(stmt)
    }
  }

  /** Compiles a class constructor.
    *
    * We need a separate method because of the special handling of java's
    * `super` calls (see `compileConstructorStmt`).
    * */
  def compileConstructor(className: irt.Ident, classType: irtpe.ClassType,
      superClassType: irtpe.ClassType, methodDecl: MethodDecl): irt.MethodDef = {
    implicit val pos = Utils.getPosition(methodDecl)

    val constrName = mangler.encodeMethod(methodDecl)
    // helper func to capture the names:
    val compConsStmt = (stmt: Statement) =>
      compileConstructorStmt(className, classType, superClassType, stmt)
    val body = methodDecl.body match {
      case Block(statements, _) =>
        irt.Block(statements.map(compConsStmt))

      case _ =>
        throw new Exception(s"Unexpected constructor body: ${methodDecl.body}")
    }

    val retType = methodDecl.retType.map(typeCompiler.compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val defVal = methodDecl.defVal.map(compileExpr(_, exprPos = true))
    val thrown = methodDecl.thrown.map(compileExpr(_, exprPos = true))
    val recvParam = methodDecl.recvParam.map(compileStatement)
    val typeParams = methodDecl.typeParams.map(compileTree)

    irt.MethodDef(static = false, constrName, params, retType, Some(body))(
      irt.OptimizerHints.empty, None)

  }

  // Compiling methods

  def compileParam(param: VarDecl): irt.ParamDef = {
    implicit val pos = Utils.getPosition(param)
    val name = irt.Ident(param.name)   // mangler.encodeLocalSym(param.symbol)
    val ptpe = typeCompiler.compileType(param.varType)

    irt.ParamDef(name, ptpe, mutable = false, rest = false)
  }

  def compileParamRef(paramRef: Expr): irt.Tree = {
    implicit val pos = Utils.getPosition(paramRef)
    paramRef match {
      case Ident(sym, name, tp, _, _) =>
        val ident = irt.Ident(name)
        val tpe = typeCompiler.compileType(tp)

        irt.VarRef(ident)(tpe)

      case expr =>
        compileExpr(expr, exprPos = true)
    }
  }

  def compileMethodDecl(methodDecl: MethodDecl): irt.MethodDef = {
    implicit val pos = Utils.getPosition(methodDecl)
    val name = mangler.encodeMethod(methodDecl) // irt.Ident(mangler.mangleMethodName(methodDecl))
    val retType = methodDecl.retType.map(typeCompiler.compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val body = compileTree(methodDecl.body)
    val defVal = methodDecl.defVal.map(compileExpr(_, exprPos = true))
    // methodDecl.modifiers
    val thrown = methodDecl.thrown.map(compileExpr(_, exprPos = true))
    val recvParam = methodDecl.recvParam.map(compileStatement)
    val typeParams = methodDecl.typeParams.map(compileTree)

    irt.MethodDef(static = false, name, params, retType, Some(body))(
        irt.OptimizerHints.empty, None)
  }

  // Compiling classes

  def compileFieldDef(varDecl: VarDecl): irt.FieldDef = {
    implicit val pos = Utils.getPosition(varDecl)
    val name = mangler.encodeFieldSym(varDecl.symbol)
    val tpe = typeCompiler.compileType(varDecl.varType)
//    val modifiers = varDecl.mods
//    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
//      irtpe.zeroOf(tpe))

    irt.FieldDef(name, tpe, mutable = true)
  }

  def compileExtendsClause(extendsCl: Option[Expr])(
      implicit pos: Position): (irt.Ident, irtpe.ClassType) = extendsCl match {
    case  Some(Ident(sym, _, _, _, _)) =>
      val name = mangler.encodeClassFullNameIdent(sym)
      val tpe = mangler.encodeClassType(sym)

      (name, tpe)

    case Some(_) =>
      errorHanlder.fail(pos.line, Some("compileExtendsClause"),
        "extends clause of unknown form (expected: Identifier)", Fatal)
      (irt.Ident(""), irtpe.ClassType(""))

    case None =>
      (objectClassIdent, objectClassType)
  }

  def compileMember(className: irt.Ident, classType: irtpe.ClassType,
      superClassType: irtpe.ClassType, member: Tree): irt.Tree = member match {
    case member: MethodDecl if Predicates.isConstructor(member) =>
      compileConstructor(className, classType, superClassType, member)

    case member =>
      compileTree(member)
  }

  def compileStaticFieldInitializer(varDecl: VarDecl,
        classType: irtpe.ClassType): Option[irt.Tree] = {
    implicit val pos = Utils.getPosition(varDecl)

    val name = mangler.encodeFieldSym(varDecl.symbol)
    val tpe = typeCompiler.compileType(varDecl.varType)
    val init = inits.get(varDecl.name.str).map(compileExpr(_, exprPos = true))

    init.map(Definitions.staticAssignment(classType, name, _))
  }

  /** Creates a companion object containing
    * all the static methods of `classDecl`. Instead of putting it inside the
    * compiled ast, we store it in a list and join it later. */
  def compileCompanionObject(classDecl: ClassDecl): Unit = {
    implicit val pos = Utils.getPosition(classDecl)

    val oldName = classDecl.name.str
    val className = encodeClassName(oldName) + "$"
    val classIdent = irt.Ident(className, Some(oldName))
    val classType = irtpe.ClassType(className)

    val superClassIdent = objectClassIdent
    val superClassType = objectClassType

    val members = classDecl.members.filter(Predicates.isStatic)
    val memberDefs = members.map(
      compileMember(classIdent, classType, superClassType, _))

    val fields = members.collect { case vd: VarDecl => vd }
    val initializers = fields
        .map(compileStaticFieldInitializer(_, classType))
        .collect { case Some(i) => i }

    val consDef = Definitions.defaultConstructor(classIdent, classType, initializers)

    val mainDefs =
      if (Predicates.isMainClass(classDecl)) {
        val exportedModuleDef = irt.ModuleExportDef(className)
        val exportedMethod = Definitions.exportedDefaultMain(classIdent, classType)
        List(exportedMethod, exportedModuleDef)
      } else {
        Nil
      }

    val allDefs = consDef :: (memberDefs ++ mainDefs)

    val classDef = irt.ClassDef(
      classIdent,
      ir.ClassKind.ModuleClass,
      Some(superClassIdent),
      Nil,                       // TODO compile interfaces
      None,
      allDefs)(
      irt.OptimizerHints.empty)

    val hashed = ir.Hashers.hashClassDef(classDef)

    companionObjects = hashed :: companionObjects
    if (Predicates.isMainClass(classDecl)) MainObjectFullName = Some(className)
  }

  /** Returns both the class and its companion object */
  def compileClassDecl(classDecl: ClassDecl): irt.ClassDef = {
    implicit val pos = Utils.getPosition(classDecl)
//    if (isMainClass(classDecl)) compileMainClass(classDecl)

    val className = encodeClassName(classDecl.name.str)
    val classType = irtpe.ClassType(className)
    val classIdent = irt.Ident(className)

    val extendsCl = compileExtendsClause(classDecl.extendsCl)
    val superClassIdent = extendsCl._1
    val superClassType = extendsCl._2

    val interfaces = classDecl.implementsCl.map(e => compileExtendsClause(Option(e))._1)

    val anonCons = {
      if (Predicates.isAnonymousClass(classDecl)) {
        List(Definitions.defaultConstructor(classIdent, classType,
          Nil, storeMod = false))
      } else {
        Nil
      }
    }

    val members = classDecl.members.partition(Predicates.isStatic)
    val memberDefs = anonCons ++ members._2.map(
      compileMember(classIdent, classType, superClassType, _))

    val staticMembers = members._1
    if (staticMembers.nonEmpty) compileCompanionObject(classDecl)

    val classKind =
      if (classDecl.symbol.isInterface) ir.ClassKind.Interface
      else ir.ClassKind.Class

    val classDef = irt.ClassDef(
      classIdent,
      classKind,
      Some(superClassIdent),
      interfaces,
      None,
      memberDefs)(
      irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  // Compile low-level nodes

  def compileLocalVar(varDecl: VarDecl): irt.VarDef = {
    implicit val pos = Utils.getPosition(varDecl)
    val name = mangler.encodeLocalSym(varDecl.symbol)

    val tpe = typeCompiler.compileType(varDecl.varType)
    val init = varDecl.init.map(compileExpr(_, exprPos = true)).getOrElse(irtpe.zeroOf(tpe))
//    val modifiers = varDecl.mods
//    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
//      irtpe.zeroOf(tpe))

    irt.VarDef(name, tpe, mutable = true, init)
  }

  def compileSelectIdent(expr: Expr): irt.Ident = {
    implicit val pos = Utils.getPosition(expr)
    expr match {
      case Ident(sym, _, _, refVar, _) =>
        refVar match {
          case Some(VarInfo(_, mangledName, _, _)) =>
            mangledName

          case _ =>
            if (sym.isLocal) mangler.encodeLocalSym(sym)
            else mangler.encodeFieldSym(sym)
        }


      case _ =>
        errorHanlder.fail(pos.line, Some("compileSelectIdent"),
          "field access of unknown form (expected: Identifier)", Normal)
        irt.Ident("")
    }
  }

  def compileFieldAccessQualifier(selected: Expr, exprPos: Boolean)(
      implicit pos : Position): irt.Tree = selected match {
    case ident: Ident =>
      val classType = typeCompiler.compileType(selected.tp)
      irt.VarRef(compileSelectIdent(ident))(classType)

    case expr: Expr =>
      compileExpr(expr, exprPos)

    case _ =>
      errorHanlder.fail(pos.line, Some("compileFieldAccessQualifier"),
        s"Unknown selected tree: $selected", Fatal)
      irt.Skip()
  }

  def compileFieldAccess(fieldAcc: FieldAccess, exprPos: Boolean): irt.Tree = {
    implicit val pos = Utils.getPosition(fieldAcc)

    val item = mangler.encodeFieldSym(fieldAcc.symbol)
    val classType = typeCompiler.compileType(fieldAcc.selected.tp)
    val generic = Predicates.isGenericType(fieldAcc.selected.tp)
    val tpe = if (generic) irtpe.AnyType else typeCompiler.compileType(fieldAcc.tp)

    val qualifier =
      if (Predicates.isThisSelect(fieldAcc))
        irt.This()(classType)
      else if (Predicates.isStatic(fieldAcc))
        irt.LoadModule(irtpe.ClassType(classType.show() + "$"))
      else
        compileFieldAccessQualifier(fieldAcc.selected, exprPos)

    if (exprPos && fieldAcc.symbol.owner != null &&
        qualifier.tpe.equals(irtpe.AnyType)) {
      val ownerType = irtpe.ClassType(mangler.encodeClassFullName(fieldAcc.symbol.owner))
      val adaptedQual = irt.AsInstanceOf(qualifier, ownerType)
      irt.Select(adaptedQual, item)(tpe)
    } else if (generic && exprPos && Predicates.isAutoboxedType(fieldAcc.tp)) {
      val select = irt.Select(qualifier, item)(tpe)
      val origType = typeCompiler.compileType(fieldAcc.tp)
      val tTag = Utils.typeTag(origType)
      irt.Unbox(select, tTag)
    } else {
      irt.Select(qualifier, item)(tpe)
    }
  }

  def compileStaticAccess(ident: Ident, varDecl: VarDecl): irt.Tree = {
    implicit val pos = Utils.getPosition(ident)

    val item = mangler.encodeFieldSym(ident.symbol)
    val classType = irtpe.ClassType(encodeClassName(ident.enclClass.get) + "$")
    val tpe = typeCompiler.compileType(ident.tp)
    val qualifier = irt.This()(classType)

    irt.Select(qualifier, item)(tpe)
  }

  def compileIdent(ident: Ident): irt.Tree = {
    implicit val pos = Utils.getPosition(ident)
    val sym = ident.symbol
    val tpe = typeCompiler.compileType(ident.tp)
    ident.refVar match {
      case Some(VarInfo(_, mangledIdent, vd, ClassMember)) if Predicates.isStatic(vd) =>
        compileStaticAccess(ident, vd)

      case Some(VarInfo(_, mangledIdent, vDecl, LocalVar)) =>
        irt.VarRef(mangledIdent)(tpe)

      case Some(VarInfo(_, mangledIdent, vDecl, Param)) =>
        irt.VarRef(mangledIdent)(tpe)

      case _ =>
        val name = irt.Ident(ident.name)
        irt.VarRef(name)(tpe)
    }
  }

  // Compiling higher-level nodes

  def compileExpr(expr: Expr, exprPos: Boolean): irt.Tree = {
    implicit val pos = Utils.getPosition(expr)
    expr match {
      case expr: LetExpr =>
        ???

      case expr: Annotation =>
        ???

      case expr: AnnotatedType =>
        ???

      case expr: Wildcard =>
        ???

      case expr: TypeIntersection =>
        ???

      case expr: TypeUnion =>
        ???

      case expr: TypeApply =>
        irt.Null() // TODO

      case expr: ArrayTypeTree =>
        ???

      case expr: PrimitiveTypeTree =>
        ???

      case expr: AnyTypeTree =>
        ???

      case expr: Literal =>
        compileLiteral(expr)

      case expr: Ident =>
        compileIdent(expr)

      case expr: FieldAccess =>
        compileFieldAccess(expr, exprPos)

      case ArrayAccess(arrRef, indexExpr, tp) =>
        val arrRefC = compileExpr(arrRef, exprPos)
        val indexExprC = compileExpr(indexExpr, exprPos)
//        val tTag = mangler.mangledTypeName(tp)
        val tpC = typeCompiler.compileType(tp)

        irt.ArraySelect(arrRefC, indexExprC)(tpC)

      case expr: InstanceOf =>
        ???

      case expr: TypeCast =>
        ???

      case Binary(op, left, right, tp) =>
        val opC = opCompiler.compileBinopCode(op, left.tp, right.tp)
        val leftC = compileExpr(left, exprPos = true)
        val rightC = compileExpr(right, exprPos = true)

        irt.BinaryOp(opC, leftC, rightC)

      case Unary(op, arg, tp) =>
        val opC = opCompiler.compileBinopCode(op, arg.tp, arg.tp)
        val argC = compileExpr(arg, exprPos = true)
        val binOpC = irt.BinaryOp(opC, argC, irt.IntLiteral(1))
        val assignC = irt.Assign(argC, binOpC)

        op match {
          case Tag.PREINC | Tag.PREDEC =>
            irt.Block(assignC, argC)

          case Tag.POSTINC | Tag.POSTDEC =>
            val tmpType = typeCompiler.compileType(tp)
            val tmpName = irt.Ident("tmp12345")  // TODO
            val tmpVarDef = irt.VarDef(tmpName, tmpType, mutable = true, argC)
            val tmpVarRef = irt.VarRef(tmpName)(tmpType)

            irt.Block(tmpVarDef, assignC, tmpVarRef)

          case _ =>
            errorHanlder.fail(pos.line, Some("compileExpr: Unary"),
              s"Not a know unary operation: $op", Normal)
            irt.Null()
        }

      case expr: AssignOp =>
        ???

      case Assign(lhs, rhs, _) =>
        val lhsC = compileExpr(lhs, exprPos = false)
        val rhsC = compileExpr(rhs, exprPos = true)
        val assignment = irt.Assign(lhsC, rhsC)

        irt.Block(assignment, lhsC)

      case Parens(expr, _) =>
        compileExpr(expr, exprPos)  // ???

      case NewArray(_, _, dims, initializers, elemType, tp) =>
        val initializersC = initializers.map(compileExpr(_, exprPos = true))
        val typeInfo = mangler.arrayTypeInfo(tp)
        val ndims = if (dims.isEmpty) 1 else dims.length

        if (initializers.nonEmpty) {
          val arrType = irtpe.ArrayType(typeInfo._2, ndims)
          irt.ArrayValue(arrType, initializersC)
        } else {
          val dimsC = dims.map(compileExpr(_, exprPos = true))
          irt.NewArray(irtpe.ArrayType(typeInfo._2, ndims), dimsC)
        }

      case expr: PolyExpr =>
        compilePolyExpr(expr)

      case et@ErrorTree(pos) =>
        errorHanlder.fail(pos.line, Some("compileExpr"),
          "Errors found during one of the previous phases.", Fatal)
        irt.Null()(Utils.getPosition(et))
    }
  }

  def compilePolyExpr(polyExpr: PolyExpr): irt.Tree = {
    implicit val pos = Utils.getPosition(polyExpr)
    polyExpr match {
      case polyExpr: MethodInv =>
        compileMethodInv(polyExpr)

      case Conditional(cond, thenp, elsep, tpe) =>
        val condC = compileExpr(cond, exprPos = true)
        val thenpC = compileExpr(thenp, exprPos = true)
        val elsepC = compileExpr(elsep, exprPos = true)
        val tpeC = typeCompiler.compileType(tpe)

        irt.If(condC, thenpC, elsepC)(tpeC)

      case nc@NewClass(ident, tArgs, args, clsBody, enclExpr, tp) =>
        val clsC = typeCompiler.compileClassType(ident)
        val ctorOpt = utils.getMatchingConstructor(nc, constructors)
        val argsC = args.map(compileExpr(_, exprPos = true))
        val ctorIdent = ctorOpt match {
          case Some(m) => mangler.encodeMethod(m)
          case None    => Definitions.defaultConstructorIdent
        }

        irt.New(clsC, ctorIdent, argsC)

      case polyExpr: FuncExpr => polyExpr match {
        case funcExpr: MemberRef =>
          ???

        case funcExpr: Lambda =>
          ???
      }
    }
  }

  def compileMethodSelect(methodSel: Expr, args: List[Tree],
    refDecl: Option[ScopeElem], tp: Type): irt.Tree = {
    implicit val pos = Utils.getPosition(methodSel)

    refDecl match {
      case Some(methodInfo) =>
        val methodName = mangler.encodeMethod(
          methodInfo.decl.asInstanceOf[MethodDecl]) // TODO
        val isStatic = Predicates.isStatic(methodInfo.decl)

        methodSel match {
          case fa@FieldAccess(name, sym, selected, _) =>
            val classType = typeCompiler.compileType(selected.tp)
            val tpC = typeCompiler.compileType(tp)
            val argsC = args.map(compileTree)
            val qualifier =
              if (Predicates.isThisSelect(fa))
                irt.This()(classType)
              else if (isStatic)
                  irt.LoadModule(irtpe.ClassType(classType.show() + "$"))
              else
                compileFieldAccessQualifier(fa.selected, exprPos = true)

            irt.Apply(qualifier, methodName, argsC)(tpC)

          case id@Ident(sym, name, tpe, refVar, enclClass) =>
            val className = encodeClassName(enclClass.get)
            val classType =
              irtpe.ClassType(className + (if (isStatic) "$" else ""))
            val tpC = typeCompiler.compileType(tp)
            val argsC = args.map(compileTree)
            val qualifier =
              if (isStatic) {
                irt.This()(classType)
              } else {
                val ident = compileSelectIdent(id)
                irt.VarRef(ident)(classType)
              }

            irt.Apply(qualifier, methodName, argsC)(tpC)

          case other =>
            errorHanlder.fail(pos.line, Some("compileMethodSelect"),
              "method call of unknown form (expected: Field Access)", Normal)
            irt.Null()(Utils.getPosition(other))
        }

      case other =>
        errorHanlder.fail(pos.line, Some("compileMethodSelect"),
          s"failed to determine which method does the identifier ($methodSel) refer to ($refDecl).",
          Normal)
        irt.Null()

    }
  }

  def compileMethodInv(methodInv: MethodInv): irt.Tree = {
    implicit val pos = Utils.getPosition(methodInv)

    if (Predicates.isPrintMethodInv(methodInv)) {
      val body = compileTree(methodInv.args.head)
      Definitions.printMethod(body)
    } else {
      compileMethodSelect(methodInv.methodSel, methodInv.args,
        methodInv.refDecl, methodInv.tp)
    }
  }


  def compileLiteral(lit: Literal): irt.Literal = {
    implicit val pos = Utils.getPosition(lit)
    lit match {
      case BooleanLiteral(value, _) =>
        irt.BooleanLiteral(value)

      case CharLiteral(value, _) =>
        irt.IntLiteral(value)

      case IntLiteral(value, _) =>
        irt.IntLiteral(value)

      case LongLiteral(value, _) =>
        irt.LongLiteral(value)

      case FloatLiteral(value, _) =>
        irt.FloatLiteral(value)

      case DoubleLiteral(value, _) =>
        irt.DoubleLiteral(value)

      case NullLiteral() =>
        irt.Null()

      case ClassLiteral(value, tp) =>
        if (Predicates.isStringType(tp))
          irt.StringLiteral(value.asInstanceOf[String])
        else
          ???
    }
  }

  def compileStatement(stmt: Statement): irt.Tree = {
    implicit val pos = Utils.getPosition(stmt)
    stmt match {
      case stmt: VarDecl =>
        stmt.kind match {
          case ClassMember =>
            compileFieldDef(stmt)

          case Param =>
            compileParam(stmt)

          case LocalVar =>
            compileLocalVar(stmt)

          case Method =>
            errorHanlder.fail(pos.line, Some("compileStatement: VarDecl"),
              "Expected: Method declaration, got: Variable Declaration", Fatal)
            irt.Null()

          case Class =>
            errorHanlder.fail(pos.line, Some("compileStatement: VarDecl"),
              "Expected: Method declaration, got: Class Declaration", Fatal)
            irt.Null()
        }

      case stmt: ClassDecl =>
        // if (isMainClass(stmt)) compileMainClass(stmt)
        compileClassDecl(stmt)

      case stmt: Assert =>
        ??? // TODO

      case stmt: Throw =>
        val expr = compileExpr(stmt.expr, exprPos = true)

        irt.Throw(expr)

      case stmt: Return =>
        val expr = stmt.expr.map(compileExpr(_, exprPos = true)).getOrElse(irt.Undefined())
        irt.Return(expr)

      case stmt: Continue =>
        val lab = stmt.label.map(name => irt.Ident(name))

        irt.Continue(lab)

      case stmt: Break =>
        ??? // TODO

      case ExprStatement(expr) =>
        compileExpr(expr, exprPos = false)

      case If(cond, thenp, elsep) =>
        val condC = compileExpr(cond, exprPos = true)
        val thenpC = compileStatement(thenp)
        val elsepC = elsep.map(compileStatement).getOrElse(irt.Undefined())
        val tpe = irtpe.NoType  // in Java if won't ever be in expression position

        irt.If(condC, thenpC, elsepC)(tpe)

      case Block(statements, _) =>
        val statementsC = statements.map(compileStatement)
        irt.Block(statementsC)

      case stmt: TryStmt =>
        ??? // TODO

      case stmt: Skip =>
        irt.Skip()

      case stmt: Case =>
        ??? // TODO

      case stmt: Switch =>
        ???

      case stmt: Synchronized =>
        ???

      case stmt: LabeledStmt =>
        ???

      case stmt: EnhancedForLoop =>
        ???

      case stmt: ForLoop =>
        ???

      case WhileLoop(cond, body) =>
        val condC = compileExpr(cond, exprPos = true)
        val bodyC = compileStatement(body)

        irt.While(condC, bodyC)

      case DoWhileLoop(cond, body) =>
        val condC = compileExpr(cond, exprPos = true)
        val bodyC = compileStatement(body)

        irt.DoWhile(bodyC, condC)

      case et@ErrorTree(pos) =>
        errorHanlder.fail(pos.line, Some("compileExpr"),
          "Errors found during one of the previous phases.", Fatal)
        irt.Null()(Utils.getPosition(et))
    }
  }

  def compileImport(imp: Import): irt.Tree = {
    ??? // TODO
  }

  /** Compile an expression tree into an IR `Tree`. */
  def compileTree(tree: Tree): irt.Tree = {
    implicit val pos = Utils.getPosition(tree)
    tree match {
      case tree: Import =>
        compileImport(tree)

      case tree: MethodDecl =>
        compileMethodDecl(tree)

      case tree: TypeParam =>
        ???

      case tree: CatchTree =>
        ???

      case tree: Expr =>
        compileExpr(tree, exprPos = true)

      case tree: Statement =>
        compileStatement(tree)

      case EmptyTree() =>
        irt.Null()

      case tree: CompilationUnit =>
        errorHanlder.fail(pos.line, Some("compileTree"),
          "Cannot have nested compilation units", Fatal)
        irt.Null()

      case _ =>
        errorHanlder.fail(pos.line, Some("compileTree"),
          s"Found unknown tree: $tree", Fatal)
        irt.Null()
    }
  }

  def compile(compilationUnit: CompilationUnit): (List[irt.ClassDef], Option[String]) = {
    implicit val pos = Utils.getPosition(compilationUnit)

    companionObjects = Nil

    val imports = compilationUnit.imports.map(compileImport) // TODO
    val decls = compilationUnit.typeDecls.map({
      case c: ClassDecl => compileClassDecl(c)
      case _            =>
        errorHanlder.fail(pos.line, Some("compile"), "only class declarations" +
            "allowed at top-level", Fatal)
        irt.ClassDef(irt.Ident(""), ir.ClassKind.Class, None, Nil, None, Nil)(
          OptimizerHints.empty)
    })

    (decls ++ companionObjects, MainObjectFullName)
  }
}