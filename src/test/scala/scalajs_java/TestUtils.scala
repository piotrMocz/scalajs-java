package scalajs_java

import com.sun.tools.javac.util.{Name => JName}

import scalajs_java.trees._

object TestUtils {
  def booleanLiteral(value: Boolean)(implicit pos: Position): BooleanLiteral =
    BooleanLiteral(value, JExprType.booleanType())

  def charLiteral(value: Char)(implicit pos: Position) =
    CharLiteral(value, JExprType.charType())

  def intLiteral(value: Int)(implicit pos: Position): IntLiteral =
    IntLiteral(value, JExprType.intType())

  def longLiteral(value: Long)(implicit pos: Position): LongLiteral =
    LongLiteral(value, JExprType.longType())

  def floatLiteral(value: Float)(implicit pos: Position): FloatLiteral =
    FloatLiteral(value, JExprType.floatType())

  def doubleLiteral(value: Double)(implicit pos: Position): DoubleLiteral =
    DoubleLiteral(value, JExprType.doubleType())

  def makeName(name: String): JName = {
    new JName(null) {

      val internalName: Array[Byte] = name.getBytes()

      override def getByteArray: Array[Byte] = internalName

      override def getIndex: Int = 0

      override def getByteLength: Int = internalName.length

      override def getByteAt(i: Int): Byte = internalName(i)

      override def getByteOffset: Int = 0
    }
  }
}
