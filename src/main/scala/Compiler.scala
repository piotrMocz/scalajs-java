
import com.sun.tools.javac.code.TypeTag
import org.scalajs.core.ir
import ir.{Position, Trees => irt, Types => irtpe}
import ir.Definitions._
import trees._

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

    val body = compileExpr(tree)
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

  /** Compile an expression tree into an IR `Tree`, which is an expression
    *  that evaluates to the result of the tree.
    */
  def compileExpr(tree: Tree): irt.Tree = {
    implicit val pos = getPosition(tree)

    tree match {
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

      case _ =>
        throw new Exception(
          s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}