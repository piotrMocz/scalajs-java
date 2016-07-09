package scalajs_java.traversals

import scalajs_java.trees.{Ident, ClassDecl}
import scalajs_java.utils.{ClassScope, ErrorHandler}

/** Traverses the tree constructing the scope and tagging
  * `Ident` nodes with the tree nodes they reference. */
class EnclClassTraverse(errHandler: ErrorHandler) extends Traverse with ClassScope { self =>

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    withEnclClass[ClassDecl, ClassDecl](classDecl, classDecl)(super.traverse)
  }

  override def traverse(ident: Ident): Ident =
    ident.copy(enclClass = getEnclClass)(ident.pos)

  val errorHandler: ErrorHandler = errHandler
}
