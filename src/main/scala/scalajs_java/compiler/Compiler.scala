package scalajs_java.compiler

import com.sun.tools.javac.tree.JCTree.Tag
import org.scalajs.core.ir
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.Trees.OptimizerHints
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}

import scalajs_java.Config
import scalajs_java.trees._
import scalajs_java.utils._


/** Main compiler. */
class Compiler(val inits: Map[String, Expr], val errorHanlder: ErrorHandler) {
  var MainObjectFullName: Option[String] = None

  private final def objectClassIdent(implicit pos: Position) =
    irt.Ident(ObjectClass, Some("java.lang.Object"))

  private final val objectClassType = irtpe.ClassType("O")

  var companionObjects: List[irt.ClassDef] = Nil

  def getPosition(tree: Tree): Position =  tree.pos match {
    case scalajs_java.trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 0)
  }
  
  val opCompiler = new OpCompiler(errorHanlder)

  val typeCompiler = new TypeCompiler(errorHanlder)

  val compiledInits = inits.mapValues(compileExpr)
  

  // Compiling constructors

  /** Compiles statement of a constructor body
    *
    * The reason we need this is because Scala(.js) has
    * no explicit `super(...)` call mechanism so we have to
    * compile them separately.
    */
  def compileConstructorStmt(className: irt.Ident, classType: irtpe.ClassType,
      superClassType: irtpe.ClassType, stmt: Statement): irt.Tree = {
    implicit val pos = getPosition(stmt)

    if (Predicates.isSuperCall(stmt)) {
      stmt match {
        case ExprStatement(MethodInv(_, _, args, _, _)) =>
          val argRefsC = args.map(compileParamRef)
          val argStr = args.map(arg => Mangler.mangleType(arg.tp)).mkString("__")
          val constrName = irt.Ident("init___" + argStr)
          irt.ApplyStatically(irt.This()(classType), superClassType, constrName,
            argRefsC)(irtpe.NoType)

        case _ =>
          errorHanlder.fail(pos.line, Some("compileConstructorStmt"),
          "encountered unexpected tree", Normal)
          irt.EmptyTree
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
    implicit val pos = getPosition(methodDecl)

    val constrName = Mangler.encodeMethod(methodDecl)
    // helper func to capture the names:
    val compConsStmt = (stmt: Statement) =>
      compileConstructorStmt(className, classType, superClassType, stmt)
    val body = irt.Block(methodDecl.body.statements.map(compConsStmt))

    val retType = methodDecl.retType.map(typeCompiler.compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val defVal = methodDecl.defVal.map(compileExpr)
    val thrown = methodDecl.thrown.map(compileExpr)
    val recvParam = methodDecl.recvParam.map(compileStatement)
    val typeParams = methodDecl.typeParams.map(compileTree)

    irt.MethodDef(static = false, constrName, params, retType, body)(
      irt.OptimizerHints.empty, None)

  }

  // Compiling methods

  def compileParam(param: VarDecl): irt.ParamDef = {
    implicit val pos = getPosition(param)
    val name = irt.Ident(param.name)   // Mangler.encodeLocalSym(param.symbol)
    val ptpe = typeCompiler.compileType(param.varType)

    irt.ParamDef(name, ptpe, mutable = false, rest = false)
  }

  def compileParamRef(paramRef: Expr): irt.Tree = {
    implicit val pos = getPosition(paramRef)
    paramRef match {
      case Ident(sym, name, tp, _, _) =>
        val ident = irt.Ident(name)
        val tpe = typeCompiler.compileType(tp)

        irt.VarRef(ident)(tpe)

      case _ =>
        errorHanlder.fail(pos.line, Some("compileParam"),
          "Parameter reference of unknown form", Normal)
        irt.EmptyTree
    }
  }

  def compileMethodDecl(methodDecl: MethodDecl): irt.MethodDef = {
    implicit val pos = getPosition(methodDecl)
    val name = Mangler.encodeMethod(methodDecl) // irt.Ident(Mangler.mangleMethodName(methodDecl))
    val retType = methodDecl.retType.map(typeCompiler.compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val body = compileTree(methodDecl.body)
    val defVal = methodDecl.defVal.map(compileExpr)
    // methodDecl.modifiers
    val thrown = methodDecl.thrown.map(compileExpr)
    val recvParam = methodDecl.recvParam.map(compileStatement)
    val typeParams = methodDecl.typeParams.map(compileTree)

    irt.MethodDef(static = false, name, params, retType, body)(
        irt.OptimizerHints.empty, None)
  }

  // Compiling classes

  def compileFieldDef(varDecl: VarDecl): irt.FieldDef = {
    implicit val pos = getPosition(varDecl)
    val name = Mangler.encodeFieldSym(varDecl.symbol)
    val tpe = typeCompiler.compileType(varDecl.varType)
//    val modifiers = varDecl.mods
//    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
//      irtpe.zeroOf(tpe))

    irt.FieldDef(name, tpe, mutable = true)
  }

  def compileExtendsClause(extendsCl: Option[Expr])(
      implicit pos: Position): (irt.Ident, irtpe.ClassType) = extendsCl match {
    case  Some(Ident(sym, _, _, _, _)) =>
      val name = Mangler.encodeClassFullNameIdent(sym)
      val tpe = Mangler.encodeClassType(sym)

      (name, tpe)

    case Some(_) =>
      errorHanlder.fail(pos.line, Some("compileExtendsClause"),
        "extends clause of unknown form (expected: Identifier)", Normal)
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
    implicit val pos = getPosition(varDecl)

    val name = Mangler.encodeFieldSym(varDecl.symbol)
    val tpe = typeCompiler.compileType(varDecl.varType)
    val init = inits.get(varDecl.name.str).map(compileExpr)
    val fieldDef = irt.FieldDef(name, tpe, mutable = true)

    init.map(Definitions.staticAssignment(classType, name, _))
  }

  /** Creates a companion object containing
    * all the static methods of `classDecl`. Instead of putting it inside the
    * compiled ast, we store it in a list and join it later. */
  def compileCompanionObject(classDecl: ClassDecl): Unit = {
    implicit val pos = getPosition(classDecl)

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
    implicit val pos = getPosition(classDecl)

//    if (isMainClass(classDecl)) compileMainClass(classDecl)

    val className = encodeClassName(classDecl.name.str)
    val classType = irtpe.ClassType(className)
    val classIdent = irt.Ident(className)

    val extendsCl = compileExtendsClause(classDecl.extendsCl)
    val superClassIdent = extendsCl._1
    val superClassType = extendsCl._2

    val members = classDecl.members.partition(Predicates.isStatic)
    val memberDefs = members._2.map(
      compileMember(classIdent, classType, superClassType, _))

    val staticMembers = members._1
    if (staticMembers.nonEmpty) compileCompanionObject(classDecl)

    val classDef = irt.ClassDef(
      classIdent,
      ir.ClassKind.Class,
      Some(superClassIdent),
      Nil,                       // TODO compile interfaces
      None,
      memberDefs)(
      irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  // Compile low-level nodes

  def compileLocalVar(varDecl: VarDecl): irt.VarDef = {
    implicit val pos = getPosition(varDecl)
    val name = Mangler.encodeLocalSym(varDecl.symbol)

    val tpe = typeCompiler.compileType(varDecl.varType)
    val init = varDecl.init.map(compileExpr).getOrElse(irtpe.zeroOf(tpe))
    val modifiers = varDecl.mods
    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
      irtpe.zeroOf(tpe))

    irt.VarDef(name, tpe, mutable = true, init)
  }

  def compileSelectIdent(expr: Expr): irt.Ident = {
    implicit val pos = getPosition(expr)
    expr match {
      case Ident(sym, _, _, _, _) =>
        if (sym.isLocal) Mangler.encodeLocalSym(sym)
        else Mangler.encodeFieldSym(sym) // TODO

      case _ =>
        errorHanlder.fail(pos.line, Some("compileSelectIdent"),
          "field access of unknown form (expected: Identifier)", Normal)
        irt.Ident("")
    }
  }

  def compileFieldAccess(fieldAcc: FieldAccess): irt.Select = {
    implicit val pos = getPosition(fieldAcc)

    val item = Mangler.encodeFieldSym(fieldAcc.symbol)
    val classType = typeCompiler.compileType(fieldAcc.selected.tp)
    val tpe = typeCompiler.compileType(fieldAcc.tp)
    val qualifier =
      if (Predicates.isThisSelect(fieldAcc)) {
        irt.This()(classType)
      } else if (Predicates.isStatic(fieldAcc)) {
        irt.LoadModule(irtpe.ClassType(classType.show() + "$"))
      } else {
        val ident = compileSelectIdent(fieldAcc.selected)
        irt.VarRef(ident)(classType)
      }

    irt.Select(qualifier, item)(tpe)
  }

  def compileStaticAccess(ident: Ident, varDecl: VarDecl): irt.Tree = {
    implicit val pos = getPosition(ident)

    val item = Mangler.encodeFieldSym(ident.symbol)
    val classType = irtpe.ClassType(encodeClassName(ident.enclClass.get) + "$")
    val tpe = typeCompiler.compileType(ident.tp)
    val qualifier = irt.This()(classType)

    irt.Select(qualifier, item)(tpe)
  }

  def compileIdent(ident: Ident): irt.Tree = {
    implicit val pos = getPosition(ident)
    val sym = ident.symbol
    val tpe = typeCompiler.compileType(ident.tp)
    ident.refVar match {
      case Some(VarInfo(_, vd, ClassMember)) if Predicates.isStatic(vd) =>
        compileStaticAccess(ident, vd)

      case Some(VarInfo(_, _, LocalVar)) =>
        val name = Mangler.encodeLocalSym(sym)
        irt.VarRef(name)(tpe)

      case Some(VarInfo(_, _, Param)) =>
        val name = Mangler.encodeParamIdent(sym)
        irt.VarRef(name)(tpe)

      case _ =>
        val name = irt.Ident(ident.name)
        irt.VarRef(name)(tpe)
    }
  }

  // Compiling higher-level nodes

  def compileExpr(expr: Expr): irt.Tree = {
    implicit val pos = getPosition(expr)
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
        ???

      case expr: ArrayTypeTree =>
        ???

      case expr: PrimitiveTypeTree =>
        ???

      case expr: Literal =>
        compileLiteral(expr)

      case expr: Ident =>
        compileIdent(expr)

      case expr: FieldAccess =>
        compileFieldAccess(expr)

      case ArrayAccess(arrRef, indexExpr, tp) =>
        val arrRefC = compileExpr(arrRef)
        val indexExprC = compileExpr(indexExpr)
//        val tTag = Mangler.mangledTypeName(tp)
        val tpC = typeCompiler.compileType(tp)

        irt.ArraySelect(arrRefC, indexExprC)(tpC)

      case expr: InstanceOf =>
        ???

      case expr: TypeCast =>
        ???

      case Binary(op, left, right, tp) =>
        val opC = opCompiler.compileBinopCode(op, left.tp)
        val leftC = compileExpr(left)
        val rightC = compileExpr(right)

        irt.BinaryOp(opC, leftC, rightC)

      case Unary(op, arg, tp) =>
        val opC = opCompiler.compileBinopCode(op, arg.tp)
        val argC = compileExpr(arg)
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
            irt.EmptyTree
        }

      case expr: AssignOp =>
        ???

      case Assign(lhs, rhs, _) =>
        val lhsC = compileExpr(lhs)
        val rhsC = compileExpr(rhs)
        val assignment = irt.Assign(lhsC, rhsC)

        irt.Block(assignment, lhsC)

      case Parens(expr, _) =>
        compileExpr(expr)  // ???

      case NewArray(_, _, dims, initializers, elemType, tp) =>
        val initializersC = initializers.map(compileExpr)
        val typeInfo = Mangler.arrayTypeInfo(tp)
        val ndims = if (dims.isEmpty) 1 else dims.length

        if (initializers.nonEmpty) {
          val arrType = irtpe.ArrayType(typeInfo._2, ndims)
          irt.ArrayValue(arrType, initializersC)
        } else {
          val dimsC = dims.map(compileExpr)
          irt.NewArray(irtpe.ArrayType(typeInfo._2, ndims), dimsC)
        }

      case expr: PolyExpr =>
        compilePolyExpr(expr)

      case ErrorTree(pos) =>
        errorHanlder.fail(pos.line, Some("compileExpr"),
          "Errors found during one of the previous phases.", Fatal)
        irt.EmptyTree
    }
  }

  def compilePolyExpr(polyExpr: PolyExpr): irt.Tree = {
    implicit val pos = getPosition(polyExpr)
    polyExpr match {
      case polyExpr: MethodInv =>
        compileMethodInv(polyExpr)

      case Conditional(cond, thenp, elsep, tpe) =>
        val condC = compileExpr(cond)
        val thenpC = compileExpr(thenp)
        val elsepC = compileExpr(elsep)
        val tpeC = typeCompiler.compileType(tpe)

        irt.If(condC, thenpC, elsepC)(tpeC)

      case NewClass(ident, tArgs, args, clsBody, enclExpr, tp) =>
        val clsC = typeCompiler.compileClassType(ident)
        val mangledArgs = args.map(arg => Mangler.mangleType(arg.tp)).mkString("__")
        val ctorC = irt.Ident("init___" + mangledArgs, Some("<init>__" + mangledArgs))
        val argsC = args.map(compileExpr)

        irt.New(clsC, ctorC, argsC)

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
    implicit val pos = getPosition(methodSel)

    refDecl match {
      case Some(methodInfo@MethodInfo(_, _, _)) =>
        val methodName = Mangler.encodeMethod(methodInfo.decl)
        val isStatic = Predicates.isStatic(methodInfo.decl)

        methodSel match {
          case fa@FieldAccess(name, sym, selected, _) =>
            val classType = typeCompiler.compileType(selected.tp)
            val tpC = typeCompiler.compileType(tp)
            val argsC = args.map(compileTree)
            val qualifier =
              if (Predicates.isThisSelect(fa)) {
                irt.This()(classType)
              } else if (isStatic) {
                  irt.LoadModule(irtpe.ClassType(classType.show() + "$"))
              } else {
                val ident = compileSelectIdent(selected)
                irt.VarRef(ident)(classType)
              }

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

          case _ =>
            errorHanlder.fail(pos.line, Some("compileMethodSelect"),
              "method call of unknown form (expected: Field Access)", Normal)
            irt.EmptyTree
        }

      case _ =>
        errorHanlder.fail(pos.line, Some("compileMethodSelect"),
          s"failed to determine which method does the identifier ($methodSel) refer to.",
          Normal)
        irt.EmptyTree

    }
  }

  def compileMethodInv(methodInv: MethodInv): irt.Tree = {
    implicit val pos = getPosition(methodInv)

    if (Predicates.isPrintMethodInv(methodInv)) {
      val body = compileTree(methodInv.args.head)
      Definitions.printMethod(body)
    } else {
      compileMethodSelect(methodInv.methodSel, methodInv.args,
        methodInv.refDecl, methodInv.tp)
    }
  }


  def compileLiteral(lit: Literal): irt.Literal = {
    implicit val pos = getPosition(lit)
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

      case ClassLiteral(value, tp) =>
        if (Predicates.isStringType(tp))
          irt.StringLiteral(value.asInstanceOf[String])
        else
          ???
    }
  }

  def compileStatement(stmt: Statement): irt.Tree = {
    implicit val pos = getPosition(stmt)
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
              "Expected: Method declaration, got: Variable Declaration", Normal)
            irt.EmptyTree
        }

      case stmt: ClassDecl =>
        // if (isMainClass(stmt)) compileMainClass(stmt)
        compileClassDecl(stmt)

      case stmt: Assert =>
        ??? // TODO

      case stmt: Throw =>
        val expr = compileExpr(stmt.expr)

        irt.Throw(expr)

      case stmt: Return =>
        val expr = stmt.expr.map(compileExpr).getOrElse(irt.Undefined())
        irt.Return(expr)

      case stmt: Continue =>
        val lab = stmt.label.map(name => irt.Ident(name))

        irt.Continue(lab)

      case stmt: Break =>
        ??? // TODO

      case ExprStatement(expr) =>
        compileExpr(expr)

      case If(cond, thenp, elsep) =>
        val condC = compileExpr(cond)
        val thenpC = compileStatement(thenp)
        val elsepC = elsep.map(compileStatement).getOrElse(irt.EmptyTree)
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
        val condC = compileExpr(cond)
        val bodyC = compileStatement(body)

        irt.While(condC, bodyC)

      case DoWhileLoop(cond, body) =>
        val condC = compileExpr(cond)
        val bodyC = compileStatement(body)

        irt.DoWhile(bodyC, condC)

      case ErrorTree(pos) =>
        errorHanlder.fail(pos.line, Some("compileExpr"),
          "Errors found during one of the previous phases.", Fatal)
        irt.EmptyTree
    }
  }

  def compileImport(imp: Import): irt.Tree = {
    ??? // TODO
  }

  /** Compile an expression tree into an IR `Tree`. */
  def compileTree(tree: Tree): irt.Tree = {
    implicit val pos = getPosition(tree)
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
        compileExpr(tree)

      case tree: Statement =>
        compileStatement(tree)

      case tree: CompilationUnit =>
        errorHanlder.fail(pos.line, Some("compileTree"),
          "Cannot have nested compilation units", Fatal)
        irt.EmptyTree

      case _ =>
        errorHanlder.fail(pos.line, Some("compileTree"),
          s"Found unknown tree: $tree", Fatal)
        irt.EmptyTree
    }
  }

  def compile(compilationUnit: CompilationUnit): (List[irt.ClassDef], Option[String]) = {
    implicit val pos = getPosition(compilationUnit)

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