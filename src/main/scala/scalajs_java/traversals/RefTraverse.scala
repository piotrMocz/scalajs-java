package scalajs_java.traversals

import scalajs_java.trees._
import scalajs_java.utils.scope.{MethodElem, Scope, ScopeState}
import scalajs_java.utils.ErrorHandler

/** Traverses the tree constructing the scope and tagging
  * `Ident` nodes with the tree nodes they reference. */
class RefTraverse(errHandler: ErrorHandler, initScope: ScopeState) extends Traverse with Scope { self =>

  override val errorHanlder: ErrorHandler = errHandler

  scopeState = initScope

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
    val referredTree = scopeState.getElem(symbol)

    ident.copy(refVar = referredTree)(ident.pos)
  }

  override def traverse(methodInv: MethodInv): MethodInv = {
    implicit val pos = methodInv.pos

    val refTree: Option[MethodElem] = methodInv.methodSel match {
      case FieldAccess(name, _, _, _) =>
        scopeState.getMethod(name)

      case Ident(_, name, _, rv, _) =>
        if (rv.isDefined) rv.flatMap {
          case mElem: MethodElem => Some(mElem)
          case _                 => None
        } else scopeState.getMethod(name)

      case _ =>
        None
    }

    MethodInv(
      traverse(methodInv.methodSel),
      methodInv.typeArgs.map(traverse),
      methodInv.args.map(traverse),
      methodInv.tp,
      refDecl = refTree)
  }
}
