package scalajs_java.utils

import scala.collection.mutable.{Map => MMap}
import scalajs_java.trees.{Tree, VarDecl, VarKind}

trait Scope {

  type ScopeT = MMap[String, List[(Tree, VarKind)]]
  type VarInfo = (String, VarDecl, VarKind)

  val scope: ScopeT = MMap.empty

  def addToScope(symbol: String, tree: Tree, varKind: VarKind): Unit = {
    if (!scope.contains(symbol)) scope(symbol) = Nil

    scope(symbol) = (tree, varKind) :: scope(symbol)
  }

  def remFromScope(symbol: String): Unit = {
    if (!scope.contains(symbol))
      throw new Exception("[remFromScope] Symbol not in scope.")

    if (scope(symbol).nonEmpty)
      scope(symbol) = scope(symbol).tail

    if (scope(symbol).isEmpty)
      scope.remove(symbol)
  }

  def getFromScope(symbol: String): Option[(Tree, VarKind)] = {
    scope.get(symbol).flatMap {
      case head :: _ => Some(head)
      case _ => None
    }
  }

  def getVarInfos(members: List[Tree]): List[VarInfo] =
    members.collect { case vd: VarDecl => (vd.name.str, vd, vd.kind) }

  /** The key method of this module, used to perform
    * computations with a temporary scope update */
  def withScope[T](scopeElems: List[Tree], tree: T)(f: T => T): T = {
    val varInfos = getVarInfos(scopeElems)
    varInfos.foreach(vi => addToScope(vi._1, vi._2, vi._3))

    val res = f(tree)
    varInfos.foreach(vi => remFromScope(vi._1))
    res
  }

  def withScope[T](scopeElem: Tree, tree: T)(f: T => T): T =
    withScope[T](List(scopeElem), tree)(f)
}
