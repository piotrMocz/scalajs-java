package scalajs_java.traversals

import scalajs_java.trees._
import scalajs_java.utils.Scope

/** Traverses the tree constructing the scope and tagging
  * `Ident` nodes with the tree nodes they reference. */
class ScopedTraverse extends Traverse with Scope { self =>

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    withScope[ClassDecl](classDecl.members, classDecl)(super.traverse)
  }

  override def traverse(methodDecl: MethodDecl): MethodDecl = {
    withScope[MethodDecl](methodDecl.params, methodDecl)(super.traverse)
  }

  override def traverse(block: Block) = {
    withScope[Block](block.statements, block)(super.traverse)
  }

  override def traverse(forLoop: ForLoop) = {
    withScope[ForLoop](forLoop.init, forLoop)(super.traverse)
  }

  override def traverse(enhancedForLoop: EnhancedForLoop): EnhancedForLoop = {
    withScope[EnhancedForLoop](enhancedForLoop.variable, enhancedForLoop)(
      super.traverse)
  }

  override def traverse(ident: Ident): Ident = {
    val symbol = ident.name.str
    val referredTree = getFromScope(symbol)

    ident.copy(refVar = referredTree)(ident.pos)
  }
}
