package scalajs_java.compiler

import javax.lang.model.element.Modifier

import scalajs_java.trees.{Block, JExprType, Type, _}

/** Contains a set of methods for checking whether a given AST node
  * is a main method, constructor, etc.
  */
object Predicates {

  def isConstructor(tree: Tree): Boolean = tree match {
    case tree: MethodDecl => tree.symbol.isConstructor
    case _                => false
  }

  def isSuperCall(stmt: Statement): Boolean = stmt match {
    case ExprStatement(MethodInv(Ident(_, name, _, _), _, _, _)) =>
      name.str == "super"

    case _ =>
      false
  }

  def isThisSelect(fieldAcc: FieldAccess): Boolean = fieldAcc.selected match {
    case Ident(_, name, tp, _) => name.str == "this"
    case _                     => false
  }

  def isMainMethod(tree: Tree): Boolean = tree match {
    case m: MethodDecl => m.name.str == "main"
    case _             => false
  }

  def isPrintMethodInv(methodInv: MethodInv): Boolean = {
    methodInv.methodSel match {
      case FieldAccess(Name(mName), _, FieldAccess(Name("out"), _,
      Ident(_, Name("System"), _, _), _), _)
        if mName == "println" || mName == "print" =>
        true

      case _ =>
        false
    }
  }

  def isMainClass(classDecl: ClassDecl): Boolean =
    classDecl.members.exists(isMainMethod)

  def isStatic(member: Tree): Boolean = member match {
    case member: MethodDecl =>
      member.modifiers.flags.contains(Modifier.STATIC)

    case member: VarDecl =>
      member.mods.flags.contains(Modifier.STATIC)

    case member: Block =>
      member.isStatic

    case _ =>
      false
  }

  def isStringType(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) => jtype.toString == "java.lang.String"
    case _                => false
  }

}
