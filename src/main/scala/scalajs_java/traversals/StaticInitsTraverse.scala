package scalajs_java.traversals

import scalajs_java.trees.{Expr, VarDecl}
import scalajs_java.utils.ErrorHandler

/** Collects all the assignments like:
  * static int x = 42;
  */
class StaticInitsTraverse(val errorHandler: ErrorHandler) extends Traverse {

  var inits: Map[String, Expr] = Map.empty

  override def traverse(varDecl: VarDecl): VarDecl = {
    if (varDecl.symbol.isStatic && varDecl.init.isDefined)
      inits += (varDecl.name.str -> varDecl.init.get)

    super.traverse(varDecl)
  }

}
