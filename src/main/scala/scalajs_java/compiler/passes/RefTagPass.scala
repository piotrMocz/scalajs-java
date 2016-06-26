package scalajs_java.compiler.passes

import scalajs_java.traversals.ScopedTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHanlder}

class RefTagPass(override val verbose: Boolean=false) extends Pass[CompilationUnit, CompilationUnit] {

  override val name = "Variable reference tagging"

  override val errorHandler: ErrorHanlder =
    new ErrorHanlder(CompilerPhase("Variable ref tagging"))

  private val refTagger = new ScopedTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit =
    refTagger.traverse(tree)

}
