package intervalidus

/**
  * A domain value is at least
  *   1. finite, with a max and min value (think `Double` with its `MaxValue` and `MinValue` methods)
  *   1. totally ordered (this type class extends the [[Ordering]] type class, requiring a compare method).
  *   1. mappable to a similarly-ordered double space (potentially with collisions)
  *
  * @tparam T
  *   a value that has continuous or discrete value behavior (e.g., `Double`)
  */
trait DomainValueLike[T] extends Ordering[T]:
  /**
    * Punctuation used when representing interval in brace notation, e.g., "[a..b]" for discrete and "[a, b]" for
    * continuous.
    */
  def bracePunctuation: String

  /**
    * Maximum value. See [[https://en.wikipedia.org/wiki/Maximum_and_minimum]].
    */
  def maxValue: T

  /**
    * Minimum value. See [[https://en.wikipedia.org/wiki/Maximum_and_minimum]].
    */
  def minValue: T

  /**
    * A totally-ordered hash of a value used for mapping intervals to box search trees in double space. If `x1 < x2`,
    * then `orderedHashOf(x1) ≤ orderedHashOf(x2)`, i.e., it is "weakly monotonic". See
    * [[https://en.wikipedia.org/wiki/Monotonic_function]].
    * @note
    *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
    *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
    */
  def orderedHashOf(x: T): Double

  extension (lhs: T)
    /**
      * A totally-ordered hash of a value used for mapping intervals to box search trees in double space. If `x1 < x2`,
      * then `orderedHashOf(x1) ≤ orderedHashOf(x2)`, i.e., it is "weakly monotonic". See
      * [[https://en.wikipedia.org/wiki/Monotonic_function]].
      * @note
      *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
      *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
      */
    def orderedHashValue: Double = orderedHashOf(lhs)
