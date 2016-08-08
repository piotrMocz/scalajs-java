package scalajs_java.compiler.passes

import org.scalajs.core.ir.Trees.ClassDef

import scalajs_java.compiler.Adapt
import scalajs_java.utils.{CompilerPhase, ErrorHandler}

class AdaptPass(verb: Boolean) extends Pass[ClassDef, ClassDef] {
  override val verbose: Boolean = verb

  override def runPass(tree: ClassDef): ClassDef =
    (new Adapt).adaptClassDef(tree)

  override val name: String = "Adapt types"

  override val errorHandler: ErrorHandler =
    new ErrorHandler(CompilerPhase(name))
}
