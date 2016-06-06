package utils

import scala.collection.mutable.{Set => MSet}

import javax.lang.model.element.Modifier

import com.sun.tools.javac.code.{TypeTag, Type => JType}

import trees._

/*
 * Name mangling
 */
object Mangler {

  private val usedLocalNames: MSet[String] = MSet.empty[String]

  private def mangledTypeName(tp: Type): String = tp match {
    case StatementType =>
      ""

    case JExprType(jtype) if jtype.isPrimitive =>
      manglePrimitiveType(jtype)

    case JExprType(jtype) =>
      mangleObjectType(jtype)
  }

  private def manglePrimitiveType(jtype: JType): String = jtype.getTag match {
    case TypeTag.BOOLEAN => "Z"
    case TypeTag.BYTE    => "B"
    case TypeTag.CHAR    => "C"
    case TypeTag.DOUBLE  => "D"
    case TypeTag.FLOAT   => "F"
    case TypeTag.INT     => "I"
    case TypeTag.LONG    => "J"
    case TypeTag.SHORT   => "S"
    case TypeTag.VOID    => "V"
    case _               => throw new Exception(
      s"[manglePrimitiveType] type ${jtype.tsym} is not primitive")
  }

  // TODO
  private def mangleObjectType(jtype: JType): String = ???

  // TODO this TypedTree class hierarchy is not very good, rethink
  private def mangleType(tree: Tree): String = tree match {
    case t: TypedTree => mangledTypeName(t.tp)
    case _            => throw new Exception("Cannot mangle names without types")
  }

  private def encodeMemberNameInternal(sym: Symbol): String =
    sym.name.toString.replace("_", "$und")

  /*
   * TODO in the next three methods need to encode the names
   * possibly using encodeFieldSym and freshLocalIdent
   */
  private def mangleField(varDecl: VarDecl): String = ???

  private def mangleParam(varDecl: VarDecl): String = ???

  private def mangleLocalVar(varDecl: VarDecl): String = ???

  private def mangleModifier(modifier: Modifier): String = modifier match {
    case Modifier.PRIVATE => "__p1"
    case _                => ""  // TODO
  }

  private def mangleModifiers(modifiers: Modifiers): String =
    modifiers.flags.map(mangleModifier).mkString("")

  def mangleMethodName(methodDecl: MethodDecl): String = {
    val name = methodDecl.name.str
    val retType = methodDecl.retType.map(mangleType).getOrElse("")
    val modifier = mangleModifiers(methodDecl.modifiers)
    val params = methodDecl.params.map(mangleType).mkString("__")

    val modifierStr = if (modifier.isEmpty) "" else "__" + modifier
    val paramsStr = if (params.isEmpty) "" else "__" + params
    val retTypeStr = if (retType.isEmpty) "" else "__" + retType

    val allStrs = List(name, modifierStr, paramsStr, retTypeStr)

    allStrs.mkString("")
  }

  def mangleConstructorName(classDecl: ClassDecl): String = {
    ???
  }

}
      