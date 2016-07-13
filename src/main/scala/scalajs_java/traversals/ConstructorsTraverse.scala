package scalajs_java.traversals

import scalajs_java.trees.{ClassDecl, MethodDecl, Tree}
import scalajs_java.utils.ErrorHandler

class ConstructorsTraverse extends Traverse {

  var constructors: Map[String, List[MethodDecl]] = Map.empty

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    val conss = classDecl.members.collect {
      case md: MethodDecl if md.symbol.isConstructor =>
        md
    }
    constructors += (classDecl.symbol.toString -> conss)
    classDecl // we don't need to know about inner classes just now
  }
}
