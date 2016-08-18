package scalajs_java.compiler.passes

import scalajs_java.traversals.ConstructorsTraverse
import scalajs_java.trees.{CompilationUnit, MethodDecl}
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class ConstructorPass(verb: Boolean=false) extends Pass[CompilationUnit, CompilationUnit] {
  import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT

  override val verbose: Boolean = verb

  var constructors: ConstructorsT = Map.empty

  private val constructorsTraverse = new ConstructorsTraverse()

  override def runPass(tree: CompilationUnit): CompilationUnit = {
    val res = constructorsTraverse.traverseMain(tree)
    constructors = constructorsTraverse.constructors
    res
  }

  override val name: String = "Constructors"

  override val errorHandler: ErrorHandler = new ErrorHandler(CompilerPhase(name))
}

object ConstructorPass {
  type ConstructorsT = Map[String, List[MethodDecl]]

  def mkConstructors(constructors: List[ConstructorsT]): Map[String, List[MethodDecl]] =
    constructors reduce {_ ++ _}
}