package scalajs_java.compiler

import org.scalajs.core.ir.{Types => irtpe}
import org.scalajs.core.ir.Trees._

class Adapt {

  private def typeCode(tpe: irtpe.Type): Char = tpe match {
    case irtpe.BooleanType => 'Z'
    case irtpe.DoubleType  => 'D'
    case irtpe.FloatType   => 'F'
    case irtpe.IntType     => 'I'
    case irtpe.LongType    => 'J'
    case irtpe.StringType  => 'T'
    case _                 => throw new Exception(
      s"[typeTag] unknown tag for type: $tpe")
  }

  def adapt(tree: Tree, targetType: irtpe.Type=irtpe.NoType): Tree = {
    implicit val pos = tree.pos

    def convert(tree: Tree, targetType: irtpe.Type): Tree = {
      implicit val pos = tree.pos

      if (tree.tpe.equals(targetType)) adapt(tree, targetType)
      else targetType match {
        case cls: irtpe.ReferenceType =>
          AsInstanceOf(tree, cls)
        case irtpe.AnyType | irtpe.UndefType | irtpe.NoType |
             irtpe.NothingType | irtpe.StringType =>
          tree
        case tp =>
          Unbox(tree, typeCode(tp))
      }
    }

    tree match {
      case VarDef(ident, vtpe, mutable, rhs) =>
        VarDef(ident, vtpe, mutable, convert(rhs, vtpe))

      // Control flow constructs

      case Block(stats :+ expr) =>
        Block(stats.map(adapt(_)) :+ convert(expr, targetType))

      case Labeled(label, tpe, body) =>
        Labeled(label, tpe, convert(body, tpe))

      case Assign(lhs, rhs) =>
        // TODO lhs
        Assign(lhs, convert(rhs, lhs.tpe))

      case Return(expr, label) =>
        Return(adapt(expr), label)

      case If(cond, thenp, elsep) =>
        If(adapt(cond), adapt(thenp, tree.tpe),
          adapt(elsep, tree.tpe))(tree.tpe)

      case While(cond, body, label) =>
        While(adapt(cond), adapt(body), label)

      case DoWhile(body, cond, label) =>
        DoWhile(adapt(body), adapt(cond), label)

      case TryCatch(block, errVar, handler) =>
        TryCatch(adapt(block), errVar, adapt(handler))(tree.tpe)

      case TryFinally(block, finalizer) =>
        TryFinally(adapt(block), adapt(finalizer))

      case Throw(expr) =>
        Throw(adapt(expr))

      case Match(selector, cases, default) =>
        Match(adapt(selector),
          cases map (c => (c._1, adapt(c._2))),
          adapt(default))(tree.tpe)

      // Scala expressions

      case New(cls, ctor, args) =>
        New(cls, ctor, args.map(adapt(_)))

      case Select(qualifier, item) =>
        Select(adapt(qualifier), item)(tree.tpe)

      case Apply(receiver, method, args) =>
        Apply(adapt(receiver), method,
          args.map(adapt(_)))(tree.tpe)

      case ApplyStatically(receiver, cls, method, args) =>
        ApplyStatically(adapt(receiver), cls, method,
          args.map(adapt(_)))(tree.tpe)

      case ApplyStatic(cls, method, args) =>
        ApplyStatic(cls, method, args.map(adapt(_)))(tree.tpe)

      case UnaryOp(op, lhs) =>
        UnaryOp(op, convert(lhs, targetType))

      case BinaryOp(op, lhs, rhs) =>
        val newLeft =
          if (lhs.tpe.equals(irtpe.AnyType)) convert(lhs, targetType)
          else adapt(lhs)

        val newRight =
          if (rhs.tpe.equals(irtpe.AnyType)) convert(rhs, targetType)
          else adapt(rhs)

        BinaryOp(op, newLeft, newRight)

      case NewArray(tpe, lengths) =>
        NewArray(tpe, lengths.map(adapt(_)))

      case ArrayValue(tpe, elems) =>
        ArrayValue(tpe, elems.map(adapt(_)))

      case ArrayLength(array) =>
        ArrayLength(adapt(array))

      case ArraySelect(array, index) =>
        ArraySelect(adapt(array), adapt(index))(tree.tpe)

      case RecordValue(tpe, elems) =>
        RecordValue(tpe, elems.map(adapt(_)))

      case IsInstanceOf(expr, cls) =>
        IsInstanceOf(adapt(expr), cls)

      case AsInstanceOf(expr, cls) =>
        AsInstanceOf(adapt(expr), cls)

      case Unbox(expr, charCode) =>
        Unbox(adapt(expr), charCode)

      case GetClass(expr) =>
        GetClass(adapt(expr))

      // Defs

      case md@MethodDef(static, name, args, resType, body) =>
        MethodDef(static, name, args, resType, body.map(convert(_, resType)))(
          md.optimizerHints, md.hash)

      case cd: ClassDef =>
        adaptClassDef(cd)

      // Atomic expressions

//      case Closure(captureParams, params, body, captureValues) =>
//        Closure(captureParams, params, transformExpr(body),
//          captureValues.map(transformExpr))

      // Trees that need not be transformed

      case _ =>
        tree
    }
  }

  def adaptClassDef(classDef: ClassDef): ClassDef = {
    implicit val pos = classDef.pos
    val adaptedDefs = classDef.defs.map(adapt(_))

    classDef.copy(defs = adaptedDefs)(classDef.optimizerHints)
  }
}
