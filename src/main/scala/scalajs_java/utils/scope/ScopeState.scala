package scalajs_java.utils.scope

import scala.collection.mutable.{Map => MMap}

class ScopeState(val vars: MMap[String, List[VarInfo]],
                 val methods: MMap[String, List[MethodElem]],
                 val classes: MMap[String, List[ClassInfo]]) {

  def ++(that: ScopeState): ScopeState = {
    new ScopeState(
      this.vars ++ that.vars,
      this.methods ++ that.methods,
      this.classes ++ that.classes)
  }

  def addElem(scopeElem: ScopeElem): Unit = {
    val sym = scopeElem.name
    scopeElem match {
      case vi: VarInfo =>
        if (!vars.contains(sym)) vars(sym) = Nil

        vars(sym) = vi :: vars(sym)

      case mi: MethodInfo =>
        if (!methods.contains(sym)) methods(sym) = Nil

        methods(sym) = mi :: methods(sym)

      case ci: ClassInfo =>
        if (!classes.contains(sym)) classes(sym) = Nil

        classes(sym) = ci :: classes(sym)

      case _: LibraryMethod =>
        throw new Exception("Cannot compile library methods")
    }
  }

  def remElem(sym: String): Unit = {
    if (vars.contains(sym)) {
      if (vars(sym).nonEmpty)
        vars(sym) = vars(sym).tail

      if (vars(sym).isEmpty)
        vars.remove(sym)
    } else if (methods.contains(sym)) {
      if (methods(sym).nonEmpty)
        methods(sym) = methods(sym).tail

      if (methods(sym).isEmpty)
        methods.remove(sym)
    } else if (classes.contains(sym)) {
      if (classes(sym).nonEmpty)
        classes(sym) = classes(sym).tail

      if (classes(sym).isEmpty)
        classes.remove(sym)
    } else {
      throw new Exception(s"[ScopeT -- remElem]" +
          s"Cannot remove element ($sym) -- not in scope.")
    }
  }

  def getMethod(sym: String): Option[MethodElem] = {
    methods.get(sym).flatMap {
      case head :: _ => Some(head)
      case _         => Scope.libraryMethods.get(sym)
    }
  }

  def getVar(sym: String): Option[VarInfo] = {
    vars.get(sym).flatMap {
      case head :: _ => Some(head)
      case _         => None
    }
  }

  def getClass(sym: String): Option[ClassInfo] = {
    classes.get(sym).flatMap {
      case head :: _ => Some(head)
      case _         => None
    }
  }

  def getElem(sym: String): Option[ScopeElem] = {
    if (vars.contains(sym)) getVar(sym)
    else if (methods.contains(sym)) getMethod(sym)
    else if (classes.contains(sym)) getClass(sym)
    else None
  }
}

object ScopeState {
  def empty: ScopeState =
    new ScopeState(MMap.empty, MMap.empty, MMap.empty)
}
