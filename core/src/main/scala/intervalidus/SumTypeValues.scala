package intervalidus

import scala.deriving.Mirror.{ProductOf, SumOf}
import scala.compiletime.summonInline

/**
  * Retrieves all the values of a sum type: an enum, sealed trait, or sealed abstract class.
  */
object SumTypeValues:

  inline def sumTypeValues[E](using m: SumOf[E]): List[E] =
    valueList[m.MirroredElemTypes, E]

  private inline def valueList[T <: Tuple, E]: List[E] =
    inline compiletime.erasedValue[T] match
      case _: EmptyTuple           => Nil
      case _: ((head & E) *: tail) => summonInline[ProductOf[head & E]].fromProduct(EmptyTuple) :: valueList[tail, E]
    /*
     * Why are we passing EmptyTuple to a Product Mirror?
     *
     * Every sum type case automatically gets a compiler-synthesized `Mirror.ProductOf[Case.type]`. Because a sum
     * type case (without parameters) has zero constructor fields, its structural definition is effectively an empty
     * product. Calling `.fromProduct(EmptyTuple)` tells the mirror to construct an instance using zero parameters.
     * The compiler is smart enough to realize that, given an empty constructor for a singleton case object, it can
     * just return the existing runtime singleton instance (in this case a value with type head & E <: E).
     */
