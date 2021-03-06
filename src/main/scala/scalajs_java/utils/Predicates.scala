package scalajs_java.utils

import javax.lang.model.element.Modifier

import scalajs_java.trees._

/** Contains a set of methods for checking whether a given AST node
  * is a main method, constructor, etc.
  */
object Predicates {

  def isConstructor(tree: Tree): Boolean = tree match {
    case tree: MethodDecl => tree.symbol.isConstructor
    case _                => false
  }

  def isSuperCall(stmt: Statement): Boolean = stmt match {
    case ExprStatement(MethodInv(Ident(_, name, _, _, _), _, _, _, _)) =>
      name.str == "super"

    case _ =>
      false
  }

  def isThisSelect(fieldAcc: FieldAccess): Boolean = fieldAcc.selected match {
    case Ident(_, name, tp, _, _) => name.str == "this"
    case _                     => false
  }

  def isMainMethod(tree: Tree): Boolean = tree match {
    case m: MethodDecl => m.name.str == "main"
    case _             => false
  }

  def isPrintMethodInv(methodInv: MethodInv): Boolean = {
    methodInv.methodSel match {
      case FieldAccess(Name(mName), _, FieldAccess(Name("out"), _,
      Ident(_, Name("System"), _, _, _), _), _)
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

    case fa: FieldAccess =>
      fa.symbol.isStatic

    case _ =>
      false
  }

  def isPrivate(tree: Tree): Boolean = tree match {
    case cDecl: ClassDecl =>
      cDecl.symbol.isPrivate

    case mDecl: MethodDecl =>
      mDecl.symbol.isPrivate

    case fDecl: VarDecl if fDecl.kind == ClassMember =>
      fDecl.symbol.isPrivate

    case _ =>
      false
  }

  def isAutoboxedType(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) => Type.autoboxedTypes.contains(jtype.tsym.toString)
    case _                => false
  }

  def isPrimitiveType(tpe: Type): Boolean = {
    if (isAutoboxedType(tpe)) true
    else tpe match {
      case JExprType(jtype) => jtype.isPrimitive
      case _                => false
    }
  }

  def isStringType(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) => jtype.toString == "java.lang.String"
    case _                => false
  }

  def isClassType(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) => !jtype.isPrimitive
    case NullType         => true
    case _                => false
  }

  def isNullType(tpe: Type): Boolean = tpe match {
    case NullType => true
    case _        => false
  }

  def isMethod(tree: Tree) = tree match {
    case _: MethodDecl => true
    case _             => false
  }

  def isField(tree: Tree) = tree match {
    case vd: VarDecl if vd.kind == ClassMember => true
    case _                                     => false
  }

  def isClassMember(tree: Tree) =
    isMethod(tree) || isField(tree)

  def isNull(tree: Tree) = tree match {
    case NullLiteral() => true
    case _             => false
  }

  def isTypeParameter(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) =>
      if (jtype.tsym.owner == null) false
      else jtype.tsym.owner.`type`.isParameterized

    case _ =>
      false
  }

  def isErasedParameter(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) =>
      jtype.tsym.toString.isEmpty || jtype.tsym.toString.equals("<any>")

    case _ =>
      false
  }

  def isGenericType(tpe: Type): Boolean = tpe match {
    case JExprType(jtype) => jtype.isParameterized
    case _                => false
  }

  def isAnonymousClass(classDecl: ClassDecl): Boolean =
    classDecl.name.str.contains("$$anon$")

}
