package scalajs_java.compiler.passes

import scalajs_java.traversals.StaticInitsTraverse
import scalajs_java.trees.{CompilationUnit, Expr}
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

/**
  * Created by moczur on 09.07.16.
  */
class StaticInitsPass(verb: Boolean=false)
    extends Pass[CompilationUnit, CompilationUnit] {

  override val verbose: Boolean = verb

  override val name: String = "Static initializers"

  override val errorHandler: ErrorHandler = new ErrorHandler(CompilerPhase(name))

  var inits: Map[String, Expr] = Map.empty

  val staticInitsTraverse = new StaticInitsTraverse(errorHandler)

  override def runPass(tree: CompilationUnit): CompilationUnit = {
    val res = staticInitsTraverse.traverse(tree)
    inits = staticInitsTraverse.inits
    res
  }
}
