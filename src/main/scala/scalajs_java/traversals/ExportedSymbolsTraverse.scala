package scalajs_java.traversals

import scalajs_java.trees._
import scalajs_java.utils._

/** Prepares an exported module scope with all
  * the public methods and fields.
  */
class ExportedSymbolsTraverse(errHandler: ErrorHandler) extends Traverse with Scope {

  override val errorHanlder: ErrorHandler = errHandler

  private def getMemberInfos(members: List[Tree]): List[ScopeElem] = {
    members.collect {
      case md: MethodDecl if !Predicates.isPrivate(md) =>
        MethodInfo(md.name.str, md, kind = Method)

      case vd: VarDecl if Predicates.isField(vd) && Predicates.isPrivate(vd) =>
        VarInfo(vd.name.str, vd, kind = ClassMember)
    }
  }

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    implicit val pos = classDecl.pos

    if (!Predicates.isPrivate(classDecl)) {
      val expMethods = getMemberInfos(classDecl.members)
      expMethods.foreach(addToScope)
    }

    classDecl.copy()
  }
}
