package scalajs_java.trees

import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.{TypeTag, Type => JType}

import scalajs_java.utils.Predicates

sealed trait Type

object Type {
  def getTypeFromTree(tree: Tree): Type = tree match {
    case NullLiteral() => NullType
    case expr: Expr    => expr.tp
    case _             => StatementType
  }

  def eq(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
    case (t1, t2) if Predicates.isTypeParameter(t1) ||
      Predicates.isTypeParameter(t2) =>
      true

    case (StatementType, StatementType) =>
      true

    case (AnyType, _) =>
      true

    case (_, AnyType) =>
      true

    case (JExprType(jtype1), JExprType(jtype2)) =>
      jtype1.tsym.toString == jtype2.tsym.toString

    case (NullType, JExprType(jtype)) if jtype.isNullOrReference =>
      true

    case (JExprType(jtype), NullType) if jtype.isNullOrReference =>
      true

    case _ =>
      false
  }

  def eq(tp1: Tree, tp2: Tree): Boolean = (tp1, tp2) match {
    case (AnyTypeTree(), _) =>
      true

    case (_, AnyTypeTree()) =>
      true

    case (PrimitiveTypeTree(_, tag1, _), PrimitiveTypeTree(_, tag2, _)) =>
      tag1 == tag2

    case (ArrayTypeTree(el1, _), ArrayTypeTree(el2, _)) =>
      eq(el1, el2)

    case (Ident(sym1, _, _, _, _), Ident(sym2, _, _, _, _)) =>
      sym1.toString == sym2.toString

    case (TypeApply(tpe1, args1, _), TypeApply(tpe2, args2, _)) =>
      eq(tpe1, tpe2) &&
        args1.zip(args2).map(p => eq(p._1, p._2)).reduce(_ && _) // all pairs satisfy `eq`

    // TODO more cases

    case _ =>
      false
  }
}

// Blank type for statements

case object StatementType extends Type

case class JExprType(jtype: JType) extends Type

// used to indicate an erased type

case object AnyType extends Type

case object NullType extends Type

case object JExprType {
  def booleanType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.BOOLEAN, null))

  def charType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.CHAR, null))

  def intType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.INT, null))

  def longType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.LONG, null))

  def floatType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.FLOAT, null))

  def doubleType(): JExprType =
    JExprType(new JCPrimitiveType(TypeTag.DOUBLE, null))
}

trait TypedTree {
  def tp: Type
}

trait StatementTree extends TypedTree {
  override def tp: Type = StatementType
}

trait ExpressionTree extends TypedTree
