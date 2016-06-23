package scalajs_java.compiler

import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}

/** Contains pre-defined IR pieces for common use cases.
  *
  * This is only temporary (until we can compile everything with
  * no (few) special cases.
  */
object Definitions {

  /** This is the default (no-arg) constructor for a companion object
    * that we have to include. */
  def defaultConstructor(classIdent: irt.Ident, classType: irtpe.ClassType)(
    implicit pos: Position): irt.MethodDef = {

    val constrIdent = irt.Ident("init___", Some("<init>__"))

    val superCall = irt.ApplyStatically(
      irt.This()(classType), irtpe.ClassType("O"),
      constrIdent, Nil)(irtpe.NoType)

    val storeModule = irt.StoreModule(classType, irt.This()(classType))

    irt.MethodDef(
      static = false, constrIdent, Nil, irtpe.NoType,
      irt.Block(superCall, storeModule)
    )(irt.OptimizerHints.empty, None)
  }

  /** This is a very ad-hoc solution to produce a method call like:
    * method(Array()), where method :: Array[String] -> Unit */
  def emptyArrayAST(implicit pos: Position): irt.Tree = {
    irt.AsInstanceOf(
      irt.Apply(
        irt.LoadModule(
          irtpe.ClassType("s_Array$")),
        irt.Ident("apply__sc_Seq__s_reflect_ClassTag__O",
          Some("apply__sc_Seq__s_reflect_ClassTag__O")),
        List(
          irt.LoadModule(
            irtpe.ClassType("sci_Nil$")),
          irt.Apply(
            irt.LoadModule(
              irtpe.ClassType("s_reflect_ClassTag$")),
            irt.Ident("apply__jl_Class__s_reflect_ClassTag",
              Some("apply__jl_Class__s_reflect_ClassTag")),
            List(
              irt.ClassOf(irtpe.ClassType("T"))))(
            irtpe.ClassType("s_reflect_ClassTag"))))(irtpe.AnyType),
      irtpe.ArrayType("T", 1))
  }

  /** Print method is a special one, because we use `Predef.println`
    * instead of java's `System.out.println`. */
  def printMethod(printed: irt.Tree)(implicit pos: Position): irt.Tree = {
    irt.Apply(
      irt.LoadModule(irtpe.ClassType("s_Predef$")),
      irt.Ident("println__O__V", Some("println__O__V")),
      List(printed))(irtpe.NoType)
  }

  def exportedDefaultMain(classIdent: irt.Ident, classType: irtpe.ClassType)(
    implicit pos: Position): irt.MethodDef = {
    val emptyArr = emptyArrayAST
    val body = irt.Block(List(
      irt.Apply(irt.This()(classType), irt.Ident("main__AT__V", Some("main")),
        List(emptyArr))(irtpe.NoType),
      irt.IntLiteral(0)
    ))

    irt.MethodDef(static = false,
      irt.StringLiteral("main"), Nil, irtpe.AnyType, body)(
      irt.OptimizerHints.empty, None)
  }

  def newArray(initializer: List[irt.Tree], typeName: String, typeTag: String,
               ndims: Int)(
                implicit pos: Position): irt.Tree = {

    irt.AsInstanceOf(
      irt.Apply(
        irt.LoadModule(
          irtpe.ClassType("s_Array$")),
        irt.Ident("apply__sc_Seq__s_reflect_ClassTag__O", Some("apply__sc_Seq__s_reflect_ClassTag__O")),
        List(
          irt.New(
            irtpe.ClassType("sjs_js_WrappedArray"),
            irt.Ident("init___sjs_js_Array", Some("<init>__sjs_js_Array")),
            List(irt.JSArrayConstr(initializer))),
          irt.Apply(
            irt.LoadModule(
              irtpe.ClassType("s_reflect_ClassTag$")),
            irt.Ident(typeName + "__s_reflect_ClassTag",
              Some(typeName + "__s_reflect_ClassTag")),
            List())(irtpe.ClassType("s_reflect_ClassTag"))))(irtpe.AnyType),
      irtpe.ArrayType(typeTag, ndims))
  }

  def newArrayOfDim(dimSizes: List[irt.Tree], typeName: String, typeTag: String)(
    implicit pos: Position): irt.Tree = {
    val ndims = dimSizes.length
    val paramSig = "__I" * ndims
    val retSig = "A" * (ndims - 1) + "O"
    val applyType =
      if (ndims == 1) irtpe.AnyType
      else irtpe.ArrayType("O", ndims-1)

    irt.AsInstanceOf(
      irt.Apply(
        irt.LoadModule(
          irtpe.ClassType("s_Array$")),
        irt.Ident(s"ofDim${paramSig}__s_reflect_ClassTag__" + retSig,
          Some(s"ofDim${paramSig}__s_reflect_ClassTag__" + retSig)),
        dimSizes ++ List(
          irt.Apply(
            irt.LoadModule(
              irtpe.ClassType("s_reflect_ClassTag$")),
            irt.Ident(typeName + "__s_reflect_ClassTag", Some(typeName + "__s_reflect_ClassTag")),
            List())(irtpe.ClassType("s_reflect_ClassTag"))))(applyType),
      irtpe.ArrayType(typeTag, ndims))
  }
}