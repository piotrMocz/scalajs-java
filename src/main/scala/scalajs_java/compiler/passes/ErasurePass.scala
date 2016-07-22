package scalajs_java.compiler.passes

import scalajs_java.traversals.ErasureTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class ErasurePass(verb: Boolean) extends Pass[CompilationUnit, CompilationUnit] {
  override val verbose: Boolean = verb

  override val name: String = "Type erasure"

  override val errorHandler: ErrorHandler = new ErrorHandler(CompilerPhase(name))

  override def runPass(tree: CompilationUnit): CompilationUnit =
    new ErasureTraverse(errorHandler).traverse(tree)
}
