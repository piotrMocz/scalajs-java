package scalajs_java.utils

import scalajs_java.trees.ClassDecl

/** Used to keep track of the enclosing class */
trait ClassScope {

  type ClassName = String

  var classes: List[ClassName] = Nil

  def withEnclClass[T, S](tree: T, enclClass: ClassDecl)(f: T => S): S = {
    classes = enclClass.name.str :: classes
    val res = f(tree)
    classes = if (classes.nonEmpty) classes.tail else Nil
    res
  }

  def getEnclClass: Option[ClassName] = classes.headOption

}
