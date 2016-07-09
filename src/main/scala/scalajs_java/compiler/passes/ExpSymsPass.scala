package scalajs_java.compiler.passes
import scala.collection.mutable.{Map => MMap}
import scalajs_java.traversals.ExportedSymbolsTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.Scope.ScopeT
import scalajs_java.utils.{CompilerPhase, ErrorHandler}


class ExpSymsPass(verb: Boolean)
    extends Pass[CompilationUnit, CompilationUnit] {

  override val verbose: Boolean = verb

  override val name: String = "Exposed symbols"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase(name))

  var scope: ScopeT = MMap.empty

  val expSymTraverse = new ExportedSymbolsTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit = {
    val res = expSymTraverse.traverse(tree)
    scope = expSymTraverse.scope
    res
  }
}
