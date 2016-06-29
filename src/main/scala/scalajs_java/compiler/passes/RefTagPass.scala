package scalajs_java.compiler.passes

import scalajs_java.traversals.RefTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class RefTagPass(override val verbose: Boolean=false) extends Pass[CompilationUnit, CompilationUnit] {

  override val name = "Variable reference tagging"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase("Variable ref tagging"))

  private val refTagger = new RefTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit =
    refTagger.traverse(tree)

}
