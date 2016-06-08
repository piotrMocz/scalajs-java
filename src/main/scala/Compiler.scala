
import com.sun.tools.javac.code.Symbol.VarSymbol
import com.sun.tools.javac.code.{TypeTag, Type => JType}
import org.scalajs.core.ir
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.Types.ClassType
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}
import trees._
import utils.Mangler

/** Main compiler.
  */
object Compiler {
  final val MainObjectFullName = "main.Main"

  private final val MainClassFullName = MainObjectFullName + "$"

  private final def objectClassIdent(implicit pos: Position) =
    irt.Ident(ObjectClass, Some("java.lang.Object"))

  private final val objectClassType = irtpe.ClassType("O")

  def getPosition(tree: Tree): Position =  tree.pos match {
    case trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 1)
  }

  def enhanceMainClassConstr(classType: irtpe.ClassType,
      member: irt.Tree): irt.Tree = {
    implicit val pos = member.pos
    member match {
      case m: irt.MethodDef if m.name.name.startsWith("init___") =>
        val storeModule = irt.StoreModule(classType, irt.This()(classType))
        val newBody: irt.Tree = irt.Block(m.body, storeModule)
        m.copy(body = newBody)(m.optimizerHints, m.hash)

      case m =>
        m
    }


  }

  /**
    * Compile an expression tree into a full `ClassDef`.
    */
  def compileMainClass(classDecl: ClassDecl): irt.ClassDef = {
    implicit val pos = getPosition(classDecl)

    val mainObjectFullName = classDecl.name.str
    val mainClassFullName = mainObjectFullName + "$"

    val mainClassName = encodeClassName(mainClassFullName)
    val mainClassType = irtpe.ClassType(mainClassName)
    val mainClassIdent = irt.Ident(mainClassName)

    val superClassType = objectClassType

    val ctorDef = irt.MethodDef(static = false,
      irt.Ident("init___", Some("<init>")), Nil, irtpe.NoType,
      irt.Block(List(
        irt.ApplyStatically(irt.This()(mainClassType),
          irtpe.ClassType(ObjectClass),
          irt.Ident("init___", Some("<init>")),
          Nil)(
          irtpe.NoType),
        irt.StoreModule(mainClassType, irt.This()(mainClassType)))))(
      irt.OptimizerHints.empty, None)

//    val body = compileTree(classDecl)
//    val methodDef = irt.MethodDef(static = false,
//      irt.Ident("main__D", Some("main")), Nil, irtpe.DoubleType, body)(
//      irt.OptimizerHints.empty, None)
    val memberDefs = classDecl.members.map { m =>
      compileMember(mainClassIdent, mainClassType, superClassType, m)
    }

    val mainMemberDefs = memberDefs map { m =>
      enhanceMainClassConstr(mainClassType, m)
    }

    val exportedMethodDef = irt.MethodDef(static = false,
      irt.StringLiteral("main"), Nil, irtpe.AnyType,
      irt.Apply(irt.This()(mainClassType), irt.Ident("main__V", Some("main")),
        Nil)(irtpe.DoubleType))(
      irt.OptimizerHints.empty, None)

    val exportedModuleDef = irt.ModuleExportDef(mainObjectFullName)

    val allDefs = List(ctorDef, exportedMethodDef, exportedModuleDef) ++
        mainMemberDefs

    val classDef = irt.ClassDef(
      irt.Ident(mainClassName),
      ir.ClassKind.ModuleClass,
      Some(irt.Ident(ObjectClass)),
      Nil,
      None,
      allDefs)(
      irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  // Compiling types

  def compilePrimitiveType(tTag: TypeTag): irtpe.Type = tTag match {
    case TypeTag.BOOLEAN => irtpe.BooleanType
    case TypeTag.BYTE    => irtpe.IntType
    case TypeTag.CHAR    => irtpe.IntType
    case TypeTag.DOUBLE  => irtpe.DoubleType
    case TypeTag.FLOAT   => irtpe.FloatType
    case TypeTag.INT     => irtpe.IntType
    case TypeTag.LONG    => irtpe.LongType
    case TypeTag.SHORT   => irtpe.IntType
    case TypeTag.VOID    => irtpe.NoType
    case _               => throw new Exception(
      "[compilePrimitiveType] Not a primitive type")
  }

  def compileObjectType(tpe: JType): irtpe.Type = ???  // TODO

  def compileJavaType(tpe: JExprType): irtpe.Type = {
    if (tpe.jtype.isPrimitive) compilePrimitiveType(tpe.jtype.getTag)
    else compileObjectType(tpe.jtype)
  }

  /** Compile a type encoded as an AST attribute */
  def compileType(tpe: Type): irtpe.Type = tpe match {
    case tp: JExprType => compileJavaType(tp)
    case StatementType => irtpe.NoType
  }

  /** Compile a type encoded as an AST node */
  def compileType(tpe: Tree): irtpe.Type = tpe match {
    case PrimitiveTypeTree(_, tTag, _) =>
      compilePrimitiveType(tTag)

    case ArrayTypeTree(elemType, _) =>
      val tname = Mangler.mangleType(elemType)
      irtpe.ArrayType(tname, 1)

    case Ident(sym, _, _) =>
      if (sym.toString == "java.lang.String") irtpe.StringType
      else throw new Exception("[compileType] Cannot yet compile this type.") // TODO compile object types

    case _ =>
      ???
  }

  // Utilty methods

  def isConstructor(tree: Tree): Boolean = tree match {
    case tree: MethodDecl => tree.symbol.isConstructor
    case _                => false
  }

  def isSuperCall(stmt: Statement): Boolean = stmt match {
    case ExprStatement(MethodInv(Ident(_, name, _), _, _, _)) =>
      name.str == "super"

    case _ =>
      false
  }

  def isThisSelect(fieldAcc: FieldAccess): Boolean = fieldAcc.selected match {
    case Ident(_, name, tp) => name.str == "this"
    case _                  => false
  }

  def isMainMethod(tree: Tree): Boolean = tree match {
    case m: MethodDecl => m.name.str == "main"
    case _             => false
  }

  def isMainClass(classDecl: ClassDecl): Boolean =
    classDecl.members.exists(isMainMethod)

  // Compiling constructors

  /** Compiles statement of a constructor body
    *
    * The reason we need this is because Scala(.js) has
    * no explicit `super(...)` call mechanism so we have to
    * compile them separately.
    */
  def compileConstructorStmt(className: irt.Ident, classType: irtpe.ClassType,
      constrName: irt.Ident, superClassType: irtpe.ClassType,
      stmt: Statement): irt.Tree = {
    implicit val pos = getPosition(stmt)

    if (isSuperCall(stmt)) {
      val superArgs = stmt match {
        case ExprStatement(MethodInv(_, _, args, _)) =>
          args.map(compileParamRef)
      }
      // TODO constrName is actually a constructor of the super class
      irt.ApplyStatically(irt.This()(classType), superClassType, constrName,
        superArgs)(irtpe.NoType)
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
    val constrArgs = methodDecl.params.map(compileParam)
    val tp = irtpe.NoType
    // helper func to capture the names:
    val compConsStmt = (stmt: Statement) =>
      compileConstructorStmt(className, classType, constrName, superClassType, stmt)
    val body = irt.Block(methodDecl.body.statements.map(compConsStmt))

    val retType = methodDecl.retType.map(compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val defVal = methodDecl.defVal.map(compileExpr)
    val thrown = methodDecl.thrown.map(compileExpr)
    val recvParam = methodDecl.recvParam.map(compileVarDecl)
    val typeParams = methodDecl.typeParams.map(compileTypeParam)

    irt.MethodDef(static = false, constrName, params, retType, body)(
      irt.OptimizerHints.empty, None)

  }

  // Compiling methods

  def compileParam(param: VarDecl): irt.ParamDef = {
    implicit val pos = getPosition(param)
    val name = irt.Ident(param.name)   // Mangler.encodeLocalSym(param.symbol)
    val ptpe = compileType(param.varType)

    irt.ParamDef(name, ptpe, mutable = false, rest = false)
  }

  def compileParamRef(paramRef: Expr): irt.VarRef = {
    implicit val pos = getPosition(paramRef)
    paramRef match {
      case Ident(sym, name, tp) =>
        val ident = irt.Ident(name)
        val tpe = compileType(tp)

        irt.VarRef(ident)(tpe)

      case _ =>
        throw new Exception(
          "[compileParamRef] Parameter references have to be idents")
    }
  }

  def compileMethodDecl(methodDecl: MethodDecl): irt.MethodDef = {
    implicit val pos = getPosition(methodDecl)
    val name = Mangler.encodeMethod(methodDecl) // irt.Ident(Mangler.mangleMethodName(methodDecl))
    val retType = methodDecl.retType.map(compileType).getOrElse(irtpe.NoType)
    val params = methodDecl.params.map(compileParam)
    val body = compileTree(methodDecl.body)
    val defVal = methodDecl.defVal.map(compileExpr)
    // methodDecl.modifiers
    val thrown = methodDecl.thrown.map(compileExpr)
    val recvParam = methodDecl.recvParam.map(compileVarDecl)
    val typeParams = methodDecl.typeParams.map(compileTypeParam)

    irt.MethodDef(static = false, name, params, retType, body)(
        irt.OptimizerHints.empty, None)
  }

  // Compiling classes

  def compileFieldDef(varDecl: VarDecl): irt.FieldDef = {
    implicit val pos = getPosition(varDecl)
    val name = Mangler.encodeFieldSym(varDecl.symbol)
    val tpe = compileType(varDecl.varType)
    val init = varDecl.init.map(compileExpr)
    val modifiers = varDecl.mods
    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
      irtpe.zeroOf(tpe))

    // TODO what happens with the initializer?

    irt.FieldDef(name, tpe, mutable = false)
  }

  def compileExtendsClause(extendsCl: Option[Expr])(
      implicit pos: Position): (irt.Ident, irtpe.ClassType) = extendsCl match {
    case  Some(Ident(sym, _, _)) =>
      val name = Mangler.encodeClassFullNameIdent(sym)
      val tpe = Mangler.encodeClassType(sym)

      (name, tpe)

    case Some(_) =>
      throw new Exception(
        "[compileExtendsClause] expected Ident as the extends clause.")

    case None =>
      (objectClassIdent, objectClassType)
  }

  def compileMember(className: irt.Ident, classType: irtpe.ClassType,
      superClassType: irtpe.ClassType, member: Tree): irt.Tree = member match {
    case member: MethodDecl if isConstructor(member) =>
      compileConstructor(className, classType, superClassType, member)

    case member =>
      compileTree(member)
  }

  def compileClassDecl(classDecl: ClassDecl): irt.ClassDef = {
    implicit val pos = getPosition(classDecl)

    if (isMainClass(classDecl)) compileMainClass(classDecl)

    val className = Mangler.encodeClassFullNameIdent(classDecl.symbol)
    val classType = Mangler.encodeClassType(classDecl.symbol)
        .asInstanceOf[ClassType]

    val extendsCl = compileExtendsClause(classDecl.extendsCl)
    val superClassIdent = extendsCl._1
    val superClassType = extendsCl._2

    val memberDefs = classDecl.members
        .map(compileMember(className, classType, superClassType, _))

    val classDef = irt.ClassDef(
      className,
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

    val tpe = compileType(varDecl.varType)
    val init = varDecl.init.map(compileExpr)
    val modifiers = varDecl.mods
    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
      irtpe.zeroOf(tpe))

    irt.VarDef(name, tpe, mutable = false, nameExpr)
  }

  def compileBlock(block: Block): irt.Tree = {
    implicit val pos = getPosition(block)
    val statements = block.statements.map(compileStatement)

    irt.Block(statements)
  }

  def compileIf(ifStmt: If): irt.If = {
    implicit val pos = getPosition(ifStmt)
    val cond = compileExpr(ifStmt.cond)
    val thenp = compileStatement(ifStmt.thenStmt)
    val elsep = ifStmt.elseStmt.map(compileStatement).getOrElse(irt.EmptyTree)
    val tpe = irtpe.NoType  // in Java if won't ever be in expression position

    irt.If(cond, thenp, elsep)(tpe)
  }

  def compileSelectIdent(expr: Expr): irt.Ident = expr match {
    case Ident(sym, _, _) =>
      implicit val pos = getPosition(expr)
      if (sym.isLocal) Mangler.encodeLocalSym(sym)
      else Mangler.encodeFieldSym(sym.asInstanceOf[VarSymbol]) // TODO

    case _ =>
      throw new Exception(
        "[compileSelectIdent] Expression has to be an ident.")
  }

  def compileFieldAccess(fieldAcc: FieldAccess): irt.Select = {
    implicit val pos = getPosition(fieldAcc)
    val item = Mangler.encodeFieldSym(fieldAcc.symbol)
    val tpe = compileType(fieldAcc.tp)
    val qualifier =
    if (isThisSelect(fieldAcc)) {
        irt.This()(irtpe.NoType)
      } else {
        val ident = compileSelectIdent(fieldAcc.selected)
        irt.VarRef(ident)(tpe)
      }

    irt.Select(qualifier, item)(tpe)
  }

  def compileAssign(assign: Assign): irt.Assign = {
    implicit val pos = getPosition(assign)
    val lhs = compileExpr(assign.variable)
    val rhs = compileExpr(assign.expr)
    irt.Assign(lhs, rhs)
  }

  def compileIdent(ident: Ident): irt.VarRef = {
    implicit val pos = getPosition(ident)
    val sym = ident.symbol
    val tpe = compileType(ident.tp)
    val name =
      if (sym.isLocal) Mangler.encodeLocalSym(sym)
      else Mangler.encodeFieldSym(sym.asInstanceOf[VarSymbol])

    irt.VarRef(name)(tpe)
  }

  // Compiling higher-level nodes

  def compileExpr(expr: Expr): irt.Tree = expr match {
    case expr: LetExpr =>
      ???

    case expr: Annotation =>
      ???

    case expr: Erroneous =>
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

    case expr: ArrayAccess =>
      ???

    case expr: InstanceOf =>
      ???

    case expr: TypeCast =>
      ???

    case expr: Binary =>
      ???

    case expr: Unary =>
      ???

    case expr: AssignOp =>
      ???

    case expr: Assign =>
      compileAssign(expr)

    case expr: Parens =>
      ???

    case expr: NewArray =>
      ???

    case expr: PolyExpr =>
      compilePolyExpr(expr)
  }

  def compilePolyExpr(polyExpr: PolyExpr): irt.Tree = polyExpr match {
    case polyExpr: MethodInv =>
      compileMethodInv(polyExpr)

    case polyExpr: Conditional =>
      ???

    case polyExpr: NewClass =>
      ???

    case polyExpr: FuncExpr =>
      compileFuncExpr(polyExpr)
  }

  def compileFuncExpr(funcExpr: FuncExpr): irt.Tree = funcExpr match {
    case funcExpr: MemberRef =>
      ???

    case funcExpr: Lambda =>
      ???
  }

  def compileMethodInv(methodInv: MethodInv): irt.Tree = {
    // we need to translate a super(...) call explicitly
    methodInv.args
    methodInv.methodSel
    val typeArgs = methodInv.typeArgs.map(compileType)

    ???
  }

  def compileTypeParam(tParam: TypeParam): irt.Tree = {
    ??? // TODO
  }

  def compileVarDecl(varDecl: VarDecl): irt.Tree = varDecl.kind match {
    case ClassMember =>
      compileFieldDef(varDecl)

    case Param =>
      compileParam(varDecl)

    case LocalVar =>
      compileLocalVar(varDecl)
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

      case ClassLiteral(value, _) =>
        ???
    }
  }

  def compileStatement(stmt: Statement): irt.Tree = {
    implicit val pos = getPosition(stmt)
    stmt match {
      case stmt: VarDecl =>
        compileVarDecl(stmt)

      case stmt: ClassDecl =>
        if (isMainClass(stmt)) compileMainClass(stmt)
        else compileClassDecl(stmt)

      case stmt: Assert =>
        ??? // TODO

      case stmt: Throw =>
        val expr = compileExpr(stmt.expr)

        irt.Throw(expr)

      case stmt: Return =>
        val expr = stmt.expr.map(compileExpr).getOrElse(irt.EmptyTree)
        irt.Return(expr)

      case stmt: Continue =>
        val lab = stmt.label.map(name => irt.Ident(name))

        irt.Continue(lab)

      case stmt: Break =>
        ??? // TODO

      case stmt: ExprStatement =>
        compileExpr(stmt.expr)

      case stmt: If =>
        compileIf(stmt)

      case stmt: Block =>
        compileBlock(stmt)

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

      case stmt: EnhancedForLoop =>
        ???

      case stmt: ForLoop =>
        ???

      case stmt: WhileLoop =>
        ???

      case stmt: DoWhileLoop =>
        ???
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
        compileTypeParam(tree)

      case tree: CatchTree =>
        ???

      case tree: Expr =>
        compileExpr(tree)

      case tree: Statement =>
        compileStatement(tree)

      case tree: CompilationUnit =>
        throw new Exception(
          "Cannot have nested compilation units")

      case _ =>
        throw new Exception(
          s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }

  def compile(compilationUnit: CompilationUnit): List[irt.Tree] = {
    implicit val pos = getPosition(compilationUnit)
    val imports = compilationUnit.imports.map(compileImport)
    val decls = compilationUnit.typeDecls.map(compileTree)

    decls // TODO
  }
}