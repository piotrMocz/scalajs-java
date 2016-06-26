package scalajs_java.traversals

import scalajs_java.trees._
import scalajs_java.utils.{ErrorHanlder, Scope}

/** Traverses the tree constructing the scope and tagging
  * `Ident` nodes with the tree nodes they reference. */
class ScopedTraverse(errHanlder: ErrorHanlder) extends Traverse with Scope { self =>

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    withScope[ClassDecl, ClassDecl](classDecl.members, classDecl)(super.traverse)
  }

  override def traverse(methodDecl: MethodDecl): MethodDecl = {
    withScope[MethodDecl, MethodDecl](methodDecl.params, methodDecl)(super.traverse)
  }

  override def traverse(block: Block) = {
    withScope[Block, Block](block.statements, block)(super.traverse)
  }

  override def traverse(forLoop: ForLoop): Statement = {
    withScope[ForLoop, Statement](forLoop.init, forLoop)(super.traverse)
  }

  override def traverse(enhancedForLoop: EnhancedForLoop): Statement = {
    withScope[EnhancedForLoop, Statement](enhancedForLoop.variable, enhancedForLoop)(
      super.traverse)
  }

  override def traverse(ident: Ident): Ident = {
    val symbol = ident.name.str
    val referredTree = getFromScope(symbol)

    ident.copy(refVar = referredTree)(ident.pos)
  }

  override def traverse(methodInv: MethodInv): MethodInv = {
    implicit val pos = methodInv.pos

    val refTree = methodInv.methodSel match {
      case FieldAccess(name, _, _, _) => getFromScope(name)
      case _                          => None
    }

    MethodInv(
      traverse(methodInv.methodSel),
      methodInv.typeArgs.map(traverse),
      methodInv.args.map(traverse),
      methodInv.tp,
      refTree)
  }

  override val errorHanlder: ErrorHanlder = errHanlder
}
