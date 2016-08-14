package scalajs_java.compiler.passes

import org.scalajs.core.ir.Trees.ClassDef
import org.scalajs.core.ir.{Trees => irt}

import scalajs_java.compiler.Compiler
import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.trees.{CompilationUnit, Expr}
import scalajs_java.utils.scope.Scope.ClassMapT
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class CompilerPass(inits: Map[String, Expr],
                   classes: ClassMapT,
                   constructors: ConstructorsT,
                   override val verbose: Boolean=false)
    extends Pass[CompilationUnit, (List[irt.ClassDef], Option[String])] {

  override val name = "Compiler Pass"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase("Compilation (AST -> IR)"))

  private val compiler = new Compiler(inits, classes, constructors, errorHandler)

  override def runPass(tree: CompilationUnit): (List[ClassDef], Option[String]) =
    compiler.compile(tree)
}