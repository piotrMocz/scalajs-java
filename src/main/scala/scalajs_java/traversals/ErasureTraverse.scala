package scalajs_java.traversals

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
  def eraseTypeTree(tpe: Tree): Tree = tpe match {
    case ta: TypeApply =>
      eraseTypeTree(ta.tpe)

    case ident: Ident if isTypeParam(ident) =>
      AnyTypeTree()(tpe.pos)

    case tp =>
      tp
  }

  override def traverse(classDecl: ClassDecl): ClassDecl = {
    addTypeParams(classDecl.typeParams)

    val res = super.traverse(classDecl)

    removeTypeParams(classDecl.typeParams)

    res
  }

  override def traverse(varDecl: VarDecl): VarDecl = {
    val tpe = eraseTypeTree(varDecl.varType)

    super.traverse(varDecl.copy(varType = tpe)(varDecl.pos))
  }

  override def traverse(newClass: NewClass): NewClass = {
    val tpe = eraseTypeTree(newClass.ident)
    tpe match {
      case tpe: Expr => super.traverse(newClass.copy(ident = tpe)(newClass.pos))
      case _         => throw new Exception(
        s"[TypeParamsTraverse -- NewClass] Unexpected type: $tpe")
    }
  }

  override def traverse(methodDecl: MethodDecl): MethodDecl = {
    val retTpe = methodDecl.retType.map(eraseTypeTree)

    super.traverse(methodDecl.copy(retType = retTpe)(methodDecl.pos))
  }

  // TODO Annotation

  override def traverse(newArray: NewArray): NewArray = {
    val elTypeOpt = newArray.elemType.map(eraseTypeTree)

    elTypeOpt match {
      case Some(tpExpr) => tpExpr match {
        case tpExpr: Expr =>
          super.traverse(newArray.copy(elemType = Some(tpExpr)) (newArray.pos) )

        case _ =>
          throw new Exception(
            s"[TypeParamsTraverse -- NewClass] Unexpected type: $elTypeOpt")
      }

      case _ =>
        throw new Exception(
          s"[TypeParamsTraverse -- NewClass] Unexpected type: $elTypeOpt")
    }
  }

}
