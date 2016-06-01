
import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.code.{Type => JType}
import org.scalajs.core.ir
import ir.{Position, Trees => irt, Types => irtpe}
import ir.Definitions._
import trees._
import utils.Mangler

/** Main compiler.
  */
object Compiler {
  final val MainObjectFullName = "main.Main"

  private final val MainClassFullName = MainObjectFullName + "$"

  def getPosition(tree: Tree): Position =  tree.pos match {
    case trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 1)
  }

  /**
    * Compile an expression tree into a full `ClassDef`.
    */
  def compileMainClass(tree: Tree): irt.ClassDef = {
    implicit val pos = getPosition(tree)

    val className = encodeClassName(MainClassFullName)
    val classType = irtpe.ClassType(className)

    val ctorDef = irt.MethodDef(static = false,
      irt.Ident("init___", Some("<init>")), Nil, irtpe.NoType,
      irt.Block(List(
        irt.ApplyStatically(irt.This()(classType),
          irtpe.ClassType(ObjectClass),
          irt.Ident("init___", Some("<init>")),
          Nil)(
          irtpe.NoType),
        irt.StoreModule(classType, irt.This()(classType)))))(
      irt.OptimizerHints.empty, None)

    val body = compileTree(tree)
    val methodDef = irt.MethodDef(static = false,
      irt.Ident("main__D", Some("main")), Nil, irtpe.DoubleType, body)(
      irt.OptimizerHints.empty, None)

    val exportedMethodDef = irt.MethodDef(static = false,
      irt.StringLiteral("main"), Nil, irtpe.AnyType,
      irt.Apply(irt.This()(classType), irt.Ident("main__D", Some("main")),
        Nil)(irtpe.DoubleType))(
      irt.OptimizerHints.empty, None)

    val exportedModuleDef = irt.ModuleExportDef(MainObjectFullName)

    val allDefs = List(ctorDef, methodDef, exportedMethodDef, exportedModuleDef)

    val classDef = irt.ClassDef(
      irt.Ident(className),
      ir.ClassKind.ModuleClass,
      Some(irt.Ident(ObjectClass)),
      Nil,
      None,
      allDefs)(
      irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  def isConstructor(tree: Tree): Boolean = tree match {
    case tree: MethodDecl
      if tree.name.str.equals("<init>") => true

    case _ =>
      false
  }

  // TODO
  def compileConstructors(classDecl: ClassDecl): List[irt.MethodDef] = ???

  // for now we'll only handle default constructor
  def compileDefaultConstructor(classDecl: ClassDecl): irt.MethodDef = {
    implicit val pos = getPosition(classDecl)
    val ident = irt.Ident("init__", Some("<init>__"))
    val tp = irtpe.NoType
    val className = encodeClassName(classDecl.name)
    val classType = irtpe.ClassType(className)

    irt.MethodDef(static = false,
      irt.Ident("init___", Some("<init>")), Nil, irtpe.NoType,
      irt.Block(List(
        irt.ApplyStatically(irt.This()(classType),
          irtpe.ClassType(ObjectClass),
          irt.Ident("init___", Some("<init>")),
          Nil)(
          irtpe.NoType))))(
      irt.OptimizerHints.empty, None)
  }

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

  def compileType(tpe: Tree): irtpe.Type = tpe match {
    case PrimitiveTypeTree(_, tTag, _) =>
      compilePrimitiveType(tTag)

    case Ident(sym, _, _) =>
      ???    /* TODO compile object types */

    case _ =>
      ???
  }

  def compileParam(param: VarDecl): irt.ParamDef = {
    implicit val pos = getPosition(param)
    val name = irt.Ident(param.name) // TODO mangle
    val ptpe = compileType(param.varType)

    irt.ParamDef(name, ptpe, mutable = true, rest = false)
  }

  def compileMethodDecl(methodDecl: MethodDecl): irt.MethodDef = {
    implicit val pos = getPosition(methodDecl)
    val name = irt.Ident(Mangler.mangleMethodName(methodDecl))
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

  def compileClassDecl(classDecl: ClassDecl): irt.ClassDef = {
    implicit val pos = getPosition(classDecl)

    val className = encodeClassName(classDecl.name)
    val classType = irtpe.ClassType(className)

    // TODO only default constructor is compiled right now
    val ctorDef = compileDefaultConstructor(classDecl)
    val memberDefs = classDecl.members
        .filter(!isConstructor(_)).map(compileTree)

    val allDefs = ctorDef :: memberDefs

    val classDef = irt.ClassDef(
      irt.Ident(className),
      ir.ClassKind.Class,
      Some(irt.Ident(ObjectClass)),  // TODO compile extends clause
      Nil,                           // TODO compile interfaces
      None,
      allDefs)(
      irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

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
      ???

    case expr: FieldAccess =>
      ???

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
      ???

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

    methodInv.args
    methodInv.methodSel
    val typeArgs = methodInv.typeArgs.map(compileType)

    ???
  }

  def compileTypeParam(tParam: TypeParam): irt.Tree = {
    ??? // TODO
  }

  def compileVarDecl(varDecl: VarDecl): irt.VarDef = {
    implicit val pos = getPosition(varDecl)
    val name = irt.Ident(varDecl.name)
    val tpe = compileType(varDecl.varType)
    val init = varDecl.init.map(compileExpr)
    val modifiers = varDecl.mods
    val nameExpr = varDecl.nameExpr.map(compileExpr).getOrElse(
      irtpe.zeroOf(tpe))

    irt.VarDef(name, tpe, mutable = true, nameExpr)
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
    }
  }

  def compileStatement(stmt: Statement): irt.Tree = {
    implicit val pos = getPosition(stmt)
    stmt match {
      case stmt: VarDecl =>
        compileVarDecl(stmt)

      case stmt: ClassDecl =>
        compileClassDecl(stmt)

      case stmt: Assert =>
        ??? // TODO

      case stmt: Throw =>
        val expr = compileExpr(stmt.expr)

        irt.Throw(expr)

      case stmt: Return =>
        val expr = compileExpr(stmt.expr)

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