package scalajs_java.compiler

import com.sun.tools.javac.code.TypeTag
import com.sun.tools.javac.tree.JCTree.Tag
import org.scalajs.core.ir.Trees.{BinaryOp, UnaryOp}

import scalajs_java.trees.{JExprType, Type}
import scalajs_java.utils.{ErrorHandler, Fatal, Normal, Predicates}

/** Compiles/translates the operations like +, -, etc.*/
class OpCompiler(errorHanlder: ErrorHandler) {

  private def classOp(tLeft: Type, tRight: Type): Boolean =
    Predicates.isClassType(tLeft) && Predicates.isClassType(tRight)

  private def stringOp(tLeft: Type, tRight: Type): Boolean = {
    classOp(tLeft, tRight) &&
      (Predicates.isStringType(tLeft) || Predicates.isStringType(tRight))
  }

  def compileBinopCode(op: Tag, tpeLeft: Type, tpeRight: Type): BinaryOp.Code = {
    def fail(tag: TypeTag): Int = {
      errorHanlder.fail(0, Some("compileBinopCode"),
        s"Not a valid binop tag: $tag", Fatal)
      0
    }

    (tpeLeft, tpeRight) match {

      case (JExprType(jtypeLeft), JExprType(jtypeRight))
        if jtypeLeft.isPrimitive && jtypeLeft.isPrimitive => jtypeLeft.getTag match {
        case TypeTag.BOOLEAN => op match {
          case Tag.EQ => BinaryOp.Boolean_==
          case Tag.NE => BinaryOp.Boolean_!=
          case Tag.AND => BinaryOp.Boolean_&
          case Tag.OR => BinaryOp.Boolean_|
          case _ => fail(jtypeLeft.getTag)
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
            case _ => fail(jtypeLeft.getTag)
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
          case _ => fail(jtypeLeft.getTag)
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
          case _ => fail(jtypeLeft.getTag)
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
          case _ => fail(jtypeLeft.getTag)
        }

        case _ =>
          errorHanlder.fail(0, Some("compileBinopCode"),
            s"Not a primitive type: ${jtypeLeft.getTag}", Fatal)
          0
      }

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
    case _       => throw new Exception(s"Cannot yet handle op: $op")
  }

}
