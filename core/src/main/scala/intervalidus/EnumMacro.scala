package intervalidus

import scala.quoted.*

/**
  * Gets the `values` field from the companion of the sealed trait for this enum to retrieve all enums values.
  */
object EnumMacro:

  inline def enumValues[E <: scala.reflect.Enum]: Array[E] = ${ enumValuesImpl[E] }

  private def enumValuesImpl[E](using Quotes, Type[E]): Expr[Array[E]] =
    import quotes.reflect.*
    val companion = TypeTree.of[E].symbol.companionModule
    Select.unique(Ref(companion), "values").asExprOf[Array[E]]
