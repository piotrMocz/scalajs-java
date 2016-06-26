package scalajs_java.compiler.passes

import org.scalajs.core.ir.{Trees => irt}
import org.scalajs.core.ir.Trees.ClassDef

import scalajs_java.compiler.Compiler
import scalajs_java.trees.CompilationUnit
import scalajs_java.utils.{CompilerPhase, ErrorHanlder}


class CompilerPass extends Pass[CompilationUnit, (List[irt.ClassDef], String)] {

  override val errorHandler: ErrorHanlder =
    new ErrorHanlder(CompilerPhase("Compilation (AST -> IR)"))

  private val compiler = new Compiler(errorHandler)

  override def runPass(tree: CompilationUnit): (List[ClassDef], String) =
    compiler.compile(tree)
}