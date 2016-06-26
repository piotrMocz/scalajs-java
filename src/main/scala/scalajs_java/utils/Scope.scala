package scalajs_java.utils

import scala.collection.mutable.{Map => MMap}
import scalajs_java.trees._

sealed trait ScopeElem {
  val name: String
  val decl: Tree
  val kind: VarKind
}
case class VarInfo(name: String, decl: VarDecl, kind: VarKind) extends ScopeElem
case class MethodInfo(name: String, decl: MethodDecl, kind: VarKind=Method) extends ScopeElem
case class LibraryMethod(name: String) extends ScopeElem {
  override val decl: Tree = Skip()(Position.noPosition)
  override val kind: VarKind = Method
}

trait Scope {

  val errorHanlder: ErrorHanlder

  type ScopeT = MMap[String, List[ScopeElem]]

  val scope: ScopeT = MMap.empty

  def addToScope(scopeElem: ScopeElem): Unit = {
    val sym = scopeElem.name
    if (!scope.contains(sym)) scope(sym) = Nil

    scope(sym) = scopeElem :: scope(sym)
  }

  def remFromScope(symbol: String): Unit = {
    if (!scope.contains(symbol)) {
      errorHanlder.fail(0, Some("remFromScope"),
        s"Not in scope: $symbol", Normal)
    } else {
      if (scope(symbol).nonEmpty)
        scope(symbol) = scope(symbol).tail

      if (scope(symbol).isEmpty)
        scope.remove(symbol)
    }
  }

  def getFromScope(symbol: String): Option[ScopeElem] = {
    scope.get(symbol).flatMap {
      case head :: _ => Some(head)
      case _         => Scope.libraryMethods.get(symbol)
    }
  }

  def getScopeElems(members: List[Tree]): List[ScopeElem] = {
    members.collect {
      case vd: VarDecl => VarInfo(vd.name.str, vd, vd.kind)
      case md: MethodDecl => MethodInfo(md.name.str, md, Method)
    }
  }

  /** The key method of this module, used to perform
    * computations with a temporary scope update */
  def withScope[T, S](scope: List[Tree], tree: T)(f: T => S): S = {
    val scopeElems = getScopeElems(scope)
    scopeElems.foreach(addToScope)

    val res = f(tree)

    scopeElems.foreach(vi => remFromScope(vi.name))
    res
  }

  def withScope[T, S](scopeElem: Tree, tree: T)(f: T => S): S =
    withScope[T, S](List(scopeElem), tree)(f)
}

object Scope {
  /* TODO make only `System.out.println` a library method,
   * not every `println` */
  val libraryMethods = Map[String, ScopeElem](
    "println" -> LibraryMethod("println"),
    "print" -> LibraryMethod("print")
  )
}
