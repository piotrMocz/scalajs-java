package scalajs_java.trees

import com.sun.tools.javac.code.Type.JCPrimitiveType
import com.sun.tools.javac.code.{TypeTag, Type => JType}

sealed trait Type

object Type {
  def getTypeFromTree(tree: Tree): Type = tree match {
    case NullLiteral() => NullType
    case expr: Expr    => expr.tp
    case _             => StatementType
  }

  def getTypeName(tp: Type): String = tp match {
    case StatementType => ""
  }

  def eq(tp1: Type, tp2: Type) = (tp1, tp2) match {
    case (StatementType, StatementType) =>
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
}

/*
 *   Blank type for statements
 */
case object StatementType extends Type

case class JExprType(jtype: JType) extends Type

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
