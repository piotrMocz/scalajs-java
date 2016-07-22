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
  def eraseType(tpe: Tree): Tree = tpe match {
    case ta: TypeApply =>
      eraseType(ta.tpe)

    case ident: Ident if isTypeParam(ident) =>
      ident

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
    val tpe = eraseType(varDecl.varType)

    super.traverse(varDecl.copy(varType = tpe)(varDecl.pos))
  }

  override def traverse(newClass: NewClass): NewClass = {
    val tpe = eraseType(newClass.ident)
    tpe match {
      case tpe: Expr => super.traverse(newClass.copy(ident = tpe)(newClass.pos))
      case _         => throw new Exception(
        s"[TypeParamsTraverse -- NewClass] Unexpected type: $tpe")
    }
  }

  override def traverse(methodDecl: MethodDecl): MethodDecl = {
    val retTpe = methodDecl.retType.map(eraseType)

    super.traverse(methodDecl.copy(retType = retTpe)(methodDecl.pos))
  }

  // TODO Annotation

  override def traverse(newArray: NewArray): NewArray = {
    val elTypeOpt = newArray.elemType.map(eraseType)

    elTypeOpt match {
      case elTpe: Option[Expr] => super.traverse(newArray.copy(elemType = elTpe)(newArray.pos))
      case _                    => throw new Exception(
        s"[TypeParamsTraverse -- NewClass] Unexpected type: $elTypeOpt")
    }
  }

}
