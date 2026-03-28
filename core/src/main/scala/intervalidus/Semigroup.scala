package intervalidus

/**
  * Type class for a semigroup. See [[https://en.wikipedia.org/wiki/Semigroup]]
  */
trait Semigroup[V]:
  /**
    * Associative binary operator combining two elements. The combine operation should be associative, i.e.,
    * `combine(combine(a, b), c) == combine(a, combine(b, c))`
    *
    * The type system can't guarantee associativity. If the combine operation isn't associative, then set theory laws
    * that depend on associativity, e.g., those involving ∪ and ∩, may fail.
    *
    * @param lhs
    *   left-hand side value
    * @param rhs
    *   right-hand side value
    * @return
    *   the combined value
    */
  def combine(lhs: V, rhs: V): V

  extension (lhs: V)
    /**
      * Associative binary operator combining this element with that element. The combine operation should be
      * associative, i.e., `((a combineWith b) combineWith c) == (a combineWith (b combineWith c))`
      *
      * @param that
      *   element to combine with
      * @return
      *   the combined value
      */
    infix def combineWith(that: V): V = combine(lhs, that)
