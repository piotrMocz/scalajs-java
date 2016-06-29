package scalajs_java.compiler.passes

import scalajs_java.traversals.OperationsTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHandler}


class OpTraversePass(override val verbose: Boolean=false) extends Pass[CompilationUnit, CompilationUnit] {

  override val name = "Operation transforming"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase("Operation transforming"))

  private val opTraverse = new OperationsTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit =
    opTraverse.traverse(tree)
}
