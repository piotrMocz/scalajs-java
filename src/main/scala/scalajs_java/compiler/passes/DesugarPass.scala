package scalajs_java.compiler.passes

import scalajs_java.traversals.DesugarTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.scope.Scope.ClassMapT
import scalajs_java.utils.{CompilerPhase, ErrorHandler}


class DesugarPass(override val verbose: Boolean=false,
                  classes: ClassMapT) extends Pass[CompilationUnit, CompilationUnit] {

  override val name = "Desugaring"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase(name))

  private val desugarTraverse = new DesugarTraverse(errorHandler, classes)

  override def runPass(tree: CompilationUnit): CompilationUnit =
    desugarTraverse.traverse(tree)
}
