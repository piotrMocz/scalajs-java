package utils

/*
 * This file copies most of its contents from:
 * https://github.com/scala-js/scala-js/blob/master/compiler/src/main/scala/org/scalajs/core/compiler/JSEncoding.scala
 *
 * Original header:
 * ================
 * Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.collection.JavaConversions._
import javax.lang.model.element.Modifier

import com.sun.tools.javac.code.Symbol.{MethodSymbol, TypeSymbol, VarSymbol}
import com.sun.tools.javac.code.{Symbol, TypeTag, Type => JType}
import org.scalajs.core.ir.Definitions
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}

import trees._

/*
 * Name mangling
 */
object Mangler {

//  private val usedLocalNames: MSet[String] = MSet.empty[String]

  /** Outer separator string (between parameter types) */
  final val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  final val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  final val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  final val dceExportName = "<exported>"

  // Fresh local name generator ----------------------------------------------

  private val usedLocalNames = MSet.empty[String]
  private val localSymbolNames = MMap.empty[Symbol, String]
  private val isReserved =
    Set("arguments", "eval", ScalaJSEnvironmentName)

  private def freshName(base: String = "x"): String = {
    var suffix = 1
    var longName = base
    while (usedLocalNames(longName) || isReserved(longName)) {
      suffix += 1
      longName = base+"$"+suffix
    }
    usedLocalNames += longName
    mangleJSName(longName)
  }

  def freshLocalIdent()(implicit pos: Position): irt.Ident =
    irt.Ident(freshName(), None)

  def freshLocalIdent(base: String)(implicit pos: Position): irt.Ident =
    irt.Ident(freshName(base), Some(base))

  private def localSymbolName(sym: Symbol): String =
    localSymbolNames.getOrElseUpdate(sym, freshName(sym.name.toString))

//  def encodeLabelSym(sym: Symbol)(implicit pos: Position): irt.Ident = {
//    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
//    js.Ident(localSymbolName(sym), Some(sym.unexpandedName.decoded))
//  }

  // TODO
  private lazy val allRefClasses: Set[Symbol] = Set.empty[Symbol]

  def encodeFieldSym(sym: VarSymbol)(implicit pos: Position): irt.Ident = {
    val name0 = encodeMemberNameInternal(sym)
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    /* We have to special-case fields of Ref types (IntRef, ObjectRef, etc.)
     * because they are emitted as private by our .scala source files, but
     * they are considered public at use site since their symbols come from
     * Java-emitted .class files.
     */
    val idSuffix =
      if (sym.isPrivate || allRefClasses.contains(sym.owner))
        // sym.owner.ancestors.count(!_.isTraitOrInterface).toString
        "1"  // TODO
      else
        "f"

    val encodedName = name + "$" + idSuffix
    irt.Ident(mangleJSName(encodedName), Some(sym.flatName().toString))
  }

  def encodeMethodSym(sym: MethodSymbol, reflProxy: Boolean = false)
      (implicit pos: Position): irt.Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    irt.Ident(encodedName + paramsString,
      Some(sym.flatName().toString + paramsString))
  }

  def encodeMethodName(sym: MethodSymbol, reflProxy: Boolean = false): String = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    encodedName + paramsString
  }

  private def encodeMethodNameInternal(sym: MethodSymbol,
      reflProxy: Boolean = false,
      inRTClass: Boolean = false): (String, String) = {

    def name = encodeMemberNameInternal(sym)

    def privateSuffix(owner: Symbol): String =
      if (owner.isInterface) encodeClassFullName(owner)
      else owner.toString // TODO // .ancestors.count(!_.isTraitOrInterface).toString

    val encodedName = {
      if (sym.isConstructor)
        "init" + InnerSep
      else if (sym.isPrivate)
        mangleJSName(name) + OuterSep + "p" + privateSuffix(sym.owner)
      else
        mangleJSName(name)
    }

    val paramsString = makeParamsString(sym, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): irt.Ident = {
    require(sym.isStatic,
      "encodeStaticMemberSym called with non-static symbol: " + sym)
    irt.Ident(
      mangleJSName(encodeMemberNameInternal(sym)) +
          // makeParamsString(List(internalName(sym.`type`))),
      Some(sym.flatName().toString))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): irt.Ident = {
    require(sym.isLocal, "encodeLocalSym called with non-local symbol: " + sym)
    irt.Ident(localSymbolName(sym), Some(sym.flatName().toString))
  }

  // TODO
//  def foreignIsImplClass(sym: Symbol): Boolean =
//    sym.isModuleClass && nme.isImplClassName(sym.name)

  def encodeClassType(sym: Symbol): irtpe.Type =
    irtpe.ClassType(encodeClassFullName(sym))

  def encodeClassFullNameIdent(sym: Symbol)(implicit pos: Position): irt.Ident = {
    irt.Ident(encodeClassFullName(sym), Some(sym.flatName().toString))
  }

  def encodeClassFullName(sym: Symbol): String = {
    Definitions.encodeClassName(
      sym.flatName() + (if (needsModuleClassSuffix(sym)) "$" else ""))
  }

  // TODO
  def needsModuleClassSuffix(sym: Symbol): Boolean = false

  private def encodeMemberNameInternal(sym: Symbol): String =
    sym.name.toString.replace("_", "$und")


  private def makeParamsString(sym: MethodSymbol, reflProxy: Boolean,
      inRTClass: Boolean): String =  {
    val paramTypeNames0 = sym.getParameters.map(p => internalName(p.`type`)).toList

    val hasExplicitThisParameter = inRTClass
    val paramTypeNames = paramTypeNames0 // TODO
      // if (!hasExplicitThisParameter) paramTypeNames0
      // else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0

    val paramAndResultTypeNames = {
      if (sym.isConstructor)
        paramTypeNames
      else if (reflProxy)
        paramTypeNames :+ ""
      else
        paramTypeNames :+ internalName(sym.getReturnType)
    }
    makeParamsString(paramAndResultTypeNames)
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type): String = mangledTypeName(tpe)

  private def internalName(jtpe: JType): String = mangleJType(jtpe)

  // private def internalName(tps: TypeSymbol): String =

  /** mangles names that are illegal in JavaScript by prepending a $
    *  also mangles names that would collide with these mangled names
    */
  private def mangleJSName(name: String) =
    if (irt.isKeyword(name) || name(0).isDigit || name(0) == '$')
      "$" + name
    else name

  //////////////////////////////////////////////////////////////

  private def mangledTypeName(tp: Type): String = tp match {
    case StatementType => ""
    case tp: JExprType => mangleJType(tp.jtype)
  }

  private def mangleJType(jtype: JType): String =
    if (jtype.isPrimitive) manglePrimitiveType(jtype)
    else mangleObjectType(jtype)

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

}
      