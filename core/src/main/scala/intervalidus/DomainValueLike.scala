package intervalidus

/**
  * A domain value is at least
  *   1. bounded, with both a lower bound ([[minValue]]) and an upper bound ([[maxValue]])
  *   1. totally ordered (extends [[Ordering]], requiring a [[compare]] method)
  *   1. mappable to a weakly monotonic double value (requires an [[orderedHashOf]] method that may have "collisions")
  *
  * See [[https://en.wikipedia.org/wiki/Bounded_set]], [[https://en.wikipedia.org/wiki/Maximum_and_minimum]],
  * [[https://en.wikipedia.org/wiki/Total_order]], and [[https://en.wikipedia.org/wiki/Monotonic_function]].
  *
  * @tparam T
  *   a type with continuous or discrete value behavior (e.g., `Double`)
  */
trait DomainValueLike[T] extends Ordering[T]:
  /**
    * Punctuation used when representing interval in brace notation, e.g., "[a..b]" for discrete and "[a, b]" for
    * continuous.
    */
  def bracePunctuation: String

  /**
    * Maximum value (upper bound). See [[https://en.wikipedia.org/wiki/Maximum_and_minimum]].
    */
  def maxValue: T

  /**
    * Minimum value (lower bound). See [[https://en.wikipedia.org/wiki/Maximum_and_minimum]].
    */
  def minValue: T

  /**
    * A totally ordered hash of a value used for mapping intervals to box search trees in double space. If `x1 < x2`,
    * then `orderedHashOf(x1) ≤ orderedHashOf(x2)`, i.e., it is "weakly monotonic". See
    * [[https://en.wikipedia.org/wiki/Monotonic_function]].
    * @note
    *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
    *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
    */
  def orderedHashOf(x: T): Double

  extension (lhs: T)
    /**
      * A totally ordered hash of a value used for mapping intervals to box search trees in double space. If `x1 < x2`,
      * then `orderedHashOf(x1) ≤ orderedHashOf(x2)`, i.e., it is "weakly monotonic" in that it is non-decreasing. See
      * [[https://en.wikipedia.org/wiki/Monotonic_function]].
      * @note
      *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
      *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
      */
    def orderedHashValue: Double = orderedHashOf(lhs)
