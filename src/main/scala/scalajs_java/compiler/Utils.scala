package scalajs_java.compiler

import org.scalajs.core.ir.Position

import scalajs_java.Config
import scalajs_java.compiler.passes.ConstructorPass.ConstructorsT
import scalajs_java.trees._
import scalajs_java.utils.{ErrorHandler, Fatal, Predicates}

class Utils(val typeCompiler: TypeCompiler, val errorHandler: ErrorHandler) {
  def getPosition(tree: Tree): Position =  tree.pos match {
    case scalajs_java.trees.Position(line) => Position(Position.SourceFile(Config.testFilePath), line, 0)
  }

  def getClassNameFromExpr(expr: Expr)(
      implicit pos: Position): String = expr match {
    case ident: Ident =>
      ident.symbol.toString

    case _ =>
      errorHandler.fail(pos.line, Some("[getClassNameFromExpr]"),
        "Failed to determine class name.", Fatal)
      ""
  }

  // TODO this has to be WAY more sophisticated
  def argsMatch(callArg: Expr, defArg: VarDecl): Boolean = {
    implicit val position = getPosition(callArg)
    val callArgType = typeCompiler.compileType(callArg.tp)
    val defArgType = typeCompiler.compileType(defArg.varType)
    callArgType == defArgType || Predicates.isNull(callArg)
  }

  def argListsMatch(callArgs: List[Expr], defArgs: List[VarDecl]): Boolean = {

    if (callArgs.length != defArgs.length) return false

    (callArgs zip defArgs).map(p => argsMatch(p._1, p._2)).reduce {_ && _}
  }

  /** Chooses a matching constructor for the NewClass call */
  def getMatchingConstructor(newClass: NewClass,
      constructors: ConstructorsT): MethodDecl = {
    implicit val pos = getPosition(newClass)

    val clsName = getClassNameFromExpr(newClass.ident)

    val ctorCandidates = constructors.getOrElse(clsName, throw new Exception("No constructor found")) // TODO
    // find the first constructor, for which the argument list is ok:
    val ctor = ctorCandidates.find(md => argListsMatch(newClass.args, md.params))

    ctor.getOrElse(throw new Exception("No constructor found"))
  }
}
