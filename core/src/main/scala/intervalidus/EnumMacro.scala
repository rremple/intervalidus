package intervalidus

import scala.quoted.*

/**
  * Lifted from
  * https://github.com/rallyhealth/weePickle/blob/v1/weepickle-implicits/src/main/scala-3/com/rallyhealth/weepickle
  * /v1/implicits/macros.scala#L158
  *
  * Finds the valueOf method (a DefDef) in the companion of the sealed trait for this enum which is used to decode a
  * string as an enum value.
  */
object EnumMacro:

  inline def enumValueOf[T <: scala.reflect.Enum]: String => T = ${ enumValueOfImpl[T] }

  private def enumValueOfImpl[T](using Quotes, Type[T]): Expr[String => T] =
    import quotes.reflect._
    // val errorPrefix = "derivation not supported"
    //
    // println(s"enumValueOf isNoSymbol = ${TypeTree.of[T].symbol.isNoSymbol}")
    // if TypeTree.of[T].symbol.isNoSymbol then report.errorAndAbort(s"$errorPrefix: type is not a symbol")

    val sym = TypeTree.of[T].symbol
    // println(s"enumValueOf sym = $sym, isClassDef = ${sym.isClassDef}, isNoSymbol = ${TypeTree.of[T].symbol
    // .isNoSymbol}")
    // if !sym.isClassDef then report.errorAndAbort(s"$errorPrefix: type is not a class definition")

    val companion = sym.companionClass.tree.asInstanceOf[ClassDef]
    // println(s"enumValueOf companion.symbol = ${companion.symbol}")
    val valueOfMethods: List[DefDef] = companion.body.collect { case dd @ DefDef("valueOf", _, _, _) => dd }
    // println(s"enumValueOf valueOfMethods = valueOfMethods")
    // if valueOfMethods.size != 1 then report.errorAndAbort(s"$errorPrefix: companion valueOf method not found")

    val methodSymbol = valueOfMethods.head.symbol
    // println(s"enumValueOf methodSymbol = $methodSymbol, methodSymbol.owner = ${methodSymbol.owner}")
    Ref(methodSymbol).etaExpand(methodSymbol.owner).asExpr.asInstanceOf[Expr[String => T]]
