package scalajs_java.compiler.passes

import org.scalajs.core.ir.{Trees => irt}
import org.scalajs.core.ir.Trees.ClassDef

import scalajs_java.compiler.Compiler
import scalajs_java.trees.{CompilationUnit, Expr}
import scalajs_java.utils.{CompilerPhase, ErrorHandler}


class CompilerPass(inits: Map[String, Expr],
                   override val verbose: Boolean=false)
    extends Pass[CompilationUnit, (List[irt.ClassDef], Option[String])] {

  override val name = "Compiler Pass"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase("Compilation (AST -> IR)"))

  private val compiler = new Compiler(inits, errorHandler)

  override def runPass(tree: CompilationUnit): (List[ClassDef], Option[String]) =
    compiler.compile(tree)
}