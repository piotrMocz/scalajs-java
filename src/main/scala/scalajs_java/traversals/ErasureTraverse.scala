package scalajs_java.traversals

import com.sun.tools.javac.code.TypeTag

import scala.collection.mutable.{Map => MMap}
import scalajs_java.trees._
import scalajs_java.utils.ErrorHandler

class ErasureTraverse(val errorHandler: ErrorHandler) extends Traverse {

  val typeParams: MMap[String, List[TypeParam]] = MMap.empty

  def addTypeParams(tParamsList: List[TypeParam]): Unit = {
    for (tpar <- tParamsList) {
      val sym = tpar.name.str
      typeParams(sym) =
        if (typeParams.contains(sym)) tpar :: typeParams(sym)
        else List(tpar)
    }
  }

  def removeTypeParams(tParamsList: List[TypeParam]): Unit = {
    for (tpar <- tParamsList) {
      val sym = tpar.name.str
      typeParams.get(sym) match {
        case Some(p :: ps) => typeParams(sym) = ps
        case Some(Nil)     => typeParams.remove(sym)
        case None          => ()
      }
    }
  }

  def isTypeParam(ident: Ident): Boolean =
    typeParams.contains(ident.name.str)

  // TODO more cases
  def eraseTypeTree(tpe: Tree): Expr = tpe match {
    case ta: TypeApply =>
      eraseTypeTree(ta.tpe)

    case ident: Ident if isTypeParam(ident) =>
      AnyTypeTree()(tpe.pos)

    case tp: Expr =>
      tp

    case _ =>
      throw new Exception(s"[ErasureTraverse -- eraseTypeTree]" +
          s"type has to be Expr, found: $tpe")
  }

  def eraseType(tpe: Type): Type = tpe match {
    case JExprType(jtype) if jtype.isParameterized =>
      // TODO
      // the only sensible solution seems to be this:
      // create our own type hierarchy
      JExprType(jtype)

    case tp =>
      tp
  }

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    addTypeParams(classDecl.typeParams)

    val res = super.traverse(classDecl)

    removeTypeParams(classDecl.typeParams)

    res
  }

  override def traverse(varDecl: VarDecl): VarDecl = varDecl.init match {
    // <ArrayType> arr = { elems... };
    case Some(NewArray(anns, dimAnns, dims, inits, elemType, tp))
      if elemType.isEmpty =>
      implicit val pos = varDecl.pos
      val newAnns = anns.map(traverse)
      val newDimAnns = dimAnns.map(l => l.map(traverse))
      val newDims = dims.map(traverse)
      val newInits = inits.map(traverse)
      val elemTypeTree = varDecl.varType.asInstanceOf[ArrayTypeTree]
      val newElemType = eraseTypeTree(elemTypeTree.elemType)
      val newArray = NewArray(newAnns, newDimAnns, newDims, newInits, Some(newElemType), tp)

      VarDecl(varDecl.mods, varDecl.name, varDecl.nameExpr.map(traverse),
        varDecl.symbol, eraseTypeTree(varDecl.varType), Some(newArray),
        varDecl.kind)

    case _ =>
      val erVarType = eraseTypeTree(varDecl.varType)
      super.traverse(varDecl.copy(varType = erVarType)(varDecl.pos))
  }

  override def traverse(newClass: NewClass): NewClass = {
    val erTypeTree = eraseTypeTree(newClass.ident)
    val erType = eraseType(newClass.tp)
    erTypeTree match {
      case tpe: Expr =>
        super.traverse(newClass.copy(
          ident = tpe, tp = erType)(newClass.pos))

      case _ =>
        throw new Exception(
          s"[TypeParamsTraverse -- NewClass] Unexpected type: $erTypeTree")
    }
  }

  override def traverse(methodDecl: MethodDecl): MethodDecl = {
    val retTpe = methodDecl.retType.map(eraseTypeTree)

    super.traverse(methodDecl.copy(retType = retTpe)(methodDecl.pos))
  }

  // TODO Annotation

  override def traverse(newArray: NewArray): NewArray = {
    implicit val pos: Position = newArray.pos
    val elTypeOpt1 = newArray.elemType.map(et => eraseTypeTree(et))
    val elType = elTypeOpt1.getOrElse(throw new Exception(
      "[ErasureTraverse -- NewArray] failed to determine elem type."))

    val erasedType = eraseType(newArray.tp)

    super.traverse(newArray.copy(
      elemType = Some(elType), tp = erasedType)(newArray.pos))
  }

  override def traverse(ident: Ident): Ident = {
    val erType = eraseType(ident.tp)
    ident.copy(tp=erType)(ident.pos)
  }

  override def traverse(fieldAccess: FieldAccess): FieldAccess = {
    val erType = eraseType(fieldAccess.tp)
    fieldAccess.copy(tp=erType)(fieldAccess.pos)
  }

  override def traverse(arrayAccess: ArrayAccess): ArrayAccess = {
    val erType = eraseType(arrayAccess.tp)
    arrayAccess.copy(tp=erType)(arrayAccess.pos)
  }

  override def traverse(instanceOf: InstanceOf): InstanceOf = {
    val erType = eraseType(instanceOf.tp)
    instanceOf.copy(tp=erType)(instanceOf.pos)
  }

  override def traverse(typeCast: TypeCast): TypeCast = {
    val erType = eraseType(typeCast.tp)
    typeCast.copy(tp=erType)(typeCast.pos)
  }

  override def traverse(binary: Binary): Binary = {
    val erType = eraseType(binary.tp)
    binary.copy(tp=erType)(binary.pos)
  }

  override def traverse(unary: Unary): Unary = {
    val erType = eraseType(unary.tp)
    unary.copy(tp=erType)(unary.pos)
  }

  override def traverse(assignOp: AssignOp): AssignOp = {
    val erType = eraseType(assignOp.tp)
    assignOp.copy(tp=erType)(assignOp.pos)
  }

  override def traverse(assign: Assign): Assign = {
    val erType = eraseType(assign.tp)
    assign.copy(tp=erType)(assign.pos)
  }

  override def traverse(parens: Parens): Parens = {
    val erType = eraseType(parens.tp)
    parens.copy(tp=erType)(parens.pos)
  }

  override def traverse(methodInv: MethodInv): MethodInv = {
    val erType = eraseType(methodInv.tp)
    methodInv.copy(tp=erType)(methodInv.pos)
  }

  override def traverse(conditional: Conditional): Conditional = {
    val erType = eraseType(conditional.tp)
    conditional.copy(tp=erType)(conditional.pos)
  }

  override def traverse(classLiteral: ClassLiteral): ClassLiteral = {
    val erType = eraseType(classLiteral.tp)
    classLiteral.copy(tp=erType)(classLiteral.pos)
  }

  // TODO MemberRef, Lambda

}
