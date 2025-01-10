package intervalidus.tinyrule

import scala.compiletime.erasedValue
import scala.quoted.*

/**
  * This macro supports the stringy type matching in Fact to and from.
  */
object Macro:
  /**
    * Gets a list of type names from a tuple of types (e.g., Mirror.ProductOf[_].MirroredElemTypes).
    * @tparam T
    *   type of the tuple of types
    * @return
    *   a list of type names
    */
  inline def summonTypeNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => Macro.summonTypeName[t] :: summonTypeNames[ts]

  private inline def summonTypeName[A]: String = ${ summonTypeNameImpl[A] }

  private def summonTypeNameImpl[T: Type](using quotes: Quotes): Expr[String] = Expr(Type.show[T])
