package scalajs_java.compiler

import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.tree.JCTree.Tag
import org.scalajs.core.ir.Trees.{BinaryOp, UnaryOp}

import scalajs_java.trees.{AnyType, JExprType, Type}
import scalajs_java.utils.{ErrorHandler, Fatal, Normal, Predicates}

/** Compiles/translates the operations like +, -, etc.*/
class OpCompiler(errorHanlder: ErrorHandler) {

  private def classOp(tLeft: Type, tRight: Type): Boolean =
    Predicates.isClassType(tLeft) && Predicates.isClassType(tRight)

  private def stringOp(tLeft: Type, tRight: Type): Boolean = {
    classOp(tLeft, tRight) &&
        (Predicates.isStringType(tLeft) || Predicates.isStringType(tRight))
  }

  def primitiveOp(tLeft: Type, tRight: Type): Boolean = {
    val leftPrimitiveLike =
      Predicates.isPrimitiveType(tLeft) ||
      Predicates.isTypeParameter(tLeft) ||
      Predicates.isErasedParameter(tLeft)

    val rightPrimitiveLike =
      Predicates.isPrimitiveType(tRight) ||
      Predicates.isTypeParameter(tRight) ||
      Predicates.isErasedParameter(tRight)

    leftPrimitiveLike && rightPrimitiveLike
  }

  private def primTypeOp(op: Tag, tTag: TypeTag): Int = {
    def fail: Int = {
      errorHanlder.fail(0, Some("primTypeOpTag"),
        s"Not a valid binop tag: $op, $tTag", Fatal)
      0
    }

    tTag match {
      case TypeTag.BOOLEAN => op match {
        case Tag.EQ => BinaryOp.Boolean_==
        case Tag.NE => BinaryOp.Boolean_!=
        case Tag.AND => BinaryOp.Boolean_&
        case Tag.OR => BinaryOp.Boolean_|
        case _ => fail
      }

      case TypeTag.INT | TypeTag.CHAR | TypeTag.SHORT | TypeTag.BYTE =>
        op match {
          case Tag.PLUS => BinaryOp.Int_+
          case Tag.MINUS => BinaryOp.Int_-
          case Tag.MUL => BinaryOp.Int_*
          case Tag.DIV => BinaryOp.Int_/
          case Tag.MOD => BinaryOp.Int_%
          case Tag.BITAND => BinaryOp.Int_&
          case Tag.BITOR => BinaryOp.Int_|
          case Tag.SL => BinaryOp.Int_<<
          case Tag.SR => BinaryOp.Int_>>
          case Tag.USR => BinaryOp.Int_>>>
          case Tag.EQ => BinaryOp.Num_==
          case Tag.NE => BinaryOp.Num_!=
          case Tag.LT => BinaryOp.Num_<
          case Tag.LE => BinaryOp.Num_<=
          case Tag.GT => BinaryOp.Num_>
          case Tag.GE => BinaryOp.Num_>=
          case Tag.PREDEC | Tag.POSTDEC => BinaryOp.Int_-
          case Tag.PREINC | Tag.POSTINC => BinaryOp.Int_+
          case _ => fail
        }

      case TypeTag.LONG => op match {
        case Tag.PLUS => BinaryOp.Long_+
        case Tag.MINUS => BinaryOp.Long_-
        case Tag.MUL => BinaryOp.Long_*
        case Tag.DIV => BinaryOp.Long_/
        case Tag.MOD => BinaryOp.Long_%
        case Tag.BITAND => BinaryOp.Long_&
        case Tag.BITOR => BinaryOp.Long_|
        case Tag.SL => BinaryOp.Long_<<
        case Tag.SR => BinaryOp.Long_>>
        case Tag.USR => BinaryOp.Long_>>>
        case Tag.EQ => BinaryOp.Long_==
        case Tag.NE => BinaryOp.Long_!=
        case Tag.LT => BinaryOp.Long_<
        case Tag.LE => BinaryOp.Long_<=
        case Tag.GT => BinaryOp.Long_>
        case Tag.GE => BinaryOp.Long_>=
        case Tag.PREDEC | Tag.POSTDEC => BinaryOp.Long_-
        case Tag.PREINC | Tag.POSTINC => BinaryOp.Long_+
        case _ => fail
      }

      case TypeTag.FLOAT => op match {
        case Tag.PLUS => BinaryOp.Float_+
        case Tag.MINUS => BinaryOp.Float_-
        case Tag.MUL => BinaryOp.Float_*
        case Tag.DIV => BinaryOp.Float_/
        case Tag.MOD => BinaryOp.Float_%
        case Tag.EQ => BinaryOp.Num_==
        case Tag.NE => BinaryOp.Num_!=
        case Tag.LT => BinaryOp.Num_<
        case Tag.LE => BinaryOp.Num_<=
        case Tag.GT => BinaryOp.Num_>
        case Tag.GE => BinaryOp.Num_>=
        case Tag.PREDEC | Tag.POSTDEC => BinaryOp.Float_-
        case Tag.PREINC | Tag.POSTINC => BinaryOp.Float_+
        case _ => fail
      }

      case TypeTag.DOUBLE => op match {
        case Tag.PLUS => BinaryOp.Double_+
        case Tag.MINUS => BinaryOp.Double_-
        case Tag.MUL => BinaryOp.Double_*
        case Tag.DIV => BinaryOp.Double_/
        case Tag.MOD => BinaryOp.Double_%
        case Tag.EQ => BinaryOp.Num_==
        case Tag.NE => BinaryOp.Num_!=
        case Tag.LT => BinaryOp.Num_<
        case Tag.LE => BinaryOp.Num_<=
        case Tag.GT => BinaryOp.Num_>
        case Tag.GE => BinaryOp.Num_>=
        case Tag.PREDEC | Tag.POSTDEC => BinaryOp.Double_-
        case Tag.PREINC | Tag.POSTINC => BinaryOp.Double_+
        case _ => fail
      }

      case _ =>
        errorHanlder.fail(0, Some("compileBinopCode"),
          s"Not a primitive type: $tTag", Fatal)
        0
    }
  }

  def compileBinopCode(op: Tag, tpeLeft: Type, tpeRight: Type): BinaryOp.Code = {
    (tpeLeft, tpeRight) match {

      case (tLeft, tRight) if primitiveOp(tLeft, tRight) =>
        val tpe =
          if (Predicates.isPrimitiveType(tLeft)) tLeft
          else tRight

        primTypeOp(op, Utils.getJavaTypeTag(tpe))

      case (typeLeft, typeRight) if stringOp(typeLeft, typeRight) &&
          op == Tag.PLUS =>
        BinaryOp.String_+

      case (typeLeft, typeRight) if classOp(typeLeft, typeRight) &&
          op == Tag.EQ =>
        BinaryOp.===

      case (typeLeft, typeRight) if classOp(typeLeft, typeRight) &&
          op == Tag.NE =>
        BinaryOp.!==

      case _ =>
        throw new Exception(s"Cannot yet handle op: $op")
    }
  }

  def compileUnopCode(op: Tag, tpe: Type): UnaryOp.Code = op match {
    case Tag.NEG => UnaryOp.Boolean_!
    case _ => throw new Exception(s"Cannot yet handle op: $op")
  }
}