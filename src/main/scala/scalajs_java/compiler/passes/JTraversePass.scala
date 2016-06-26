package scalajs_java.compiler.passes

import com.sun.tools.javac.tree.JCTree
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit

import scalajs_java.traversals.JTreeTraverse
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHanlder}

class JTraversePass(override val verbose: Boolean=false) extends Pass[JCTree.JCCompilationUnit, CompilationUnit] {

  override val name = "Java AST Traversal"

  override val errorHandler: ErrorHanlder =
    new ErrorHanlder(CompilerPhase("Java AST -> Scala AST"))

  private val jtraverse = new JTreeTraverse(errorHandler)

  override def runPass(tree: JCCompilationUnit): CompilationUnit =
    jtraverse.traverse(tree)
}
