package scalajs_java.utils.scope

import org.scalajs.core.ir.{Trees => irt}

import scalajs_java.compiler.Utils
import scalajs_java.trees._
import scalajs_java.utils._

sealed trait ScopeElem {
  val name: String
  val decl: Tree
  val kind: VarKind
}

sealed trait MethodElem extends ScopeElem {
  override val kind: VarKind = Method
}

case class VarInfo(name: String, mangled: irt.Ident, decl: VarDecl, kind: VarKind) extends ScopeElem

case class MethodInfo(name: String, decl: MethodDecl) extends MethodElem

case class ClassInfo(name: String, decl: ClassDecl, kind: VarKind=Class) extends ScopeElem

case class LibraryMethod(name: String) extends MethodElem {
  override val decl: Tree = Skip()(Position.noPosition)
}

trait Scope {

  val errorHanlder: ErrorHandler

  var scopeState: ScopeState = ScopeState.empty

  val mangler = new Mangler

  def addToScope(scopeElem: ScopeElem): Unit = scopeState.addElem(scopeElem)

  def remFromScope(symbol: String): Unit = scopeState.remElem(symbol)

  def getScopeElems(members: List[Tree]): List[ScopeElem] = {
    members.collect {
      case vd: VarDecl =>
        val pos = Utils.getPosition(vd)
        val mangledName = vd.kind match {
          case LocalVar    => mangler.encodeLocalSym(vd.symbol)(pos)
          case Param       => mangler.encodeParamIdent(vd.symbol)(pos)
          case ClassMember => mangler.encodeFieldSym(vd.symbol)(pos)
          case _           => irt.Ident(vd.symbol.toString)(pos)
        }
        VarInfo(vd.name.str, mangledName, vd, vd.kind)

      case md: MethodDecl =>
        MethodInfo(md.name.str, md)
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
  type ClassMapT = Map[String, ClassDecl]

  def empty: ScopeState = ScopeState.empty

  def mkScope(scopes: List[ScopeState]): ScopeState =
    scopes.reduce {_ ++ _}

  def getClasses(scope: ScopeState): ClassMapT = {
    scope.classes
        .filter(entry => entry._2.nonEmpty)
        .map(entry => (entry._1, entry._2.head.decl))
        .toMap
  }

  /* TODO make only `System.out.println` a library method,
   * not every `println` */
  val libraryMethods = Map[String, MethodElem](
    "println" -> LibraryMethod("println"),
    "print" -> LibraryMethod("print")
  )
}