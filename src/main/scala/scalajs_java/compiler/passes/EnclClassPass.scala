package scalajs_java.compiler.passes

import scalajs_java.traversals.EnclClassTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class EnclClassPass(override val verbose: Boolean=false) extends Pass[CompilationUnit, CompilationUnit]{
  override val name: String = "Enclosing class tagging"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase("Enclosing class tagging"))

  private val enclClassTraverse = new EnclClassTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit =
    enclClassTraverse.traverse(tree)

}
