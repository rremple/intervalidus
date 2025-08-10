package intervalidus

import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.Box

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * An interval in multiple dimensions over a contiguous set of domain values in D. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @param start
  *   the "infimum", i.e., the left (and/or below and/or back, depending on dimensions and context) boundary of the
  *   interval
  * @param end
  *   the "supremum", i.e., the right (and/or above and/or front, depending on dimensions and context) boundary of the
  *   interval -- must be greater than or equal to the start in all dimensions
  * @tparam D
  *   the domain type -- [[DomainLike]] non-empty tuples.
  */
case class Interval[D <: NonEmptyTuple](
  start: D,
  end: D
)(using domainLike: DomainLike[D]):
  /**
    * Either the start is before end (by both start and end ordering), or, when equal, both bounds must be closed (i.e.,
    * interval at a single point).
    */
  require(domainLike.validIntervalBounds(start, end), s"Interval $this invalid")

  /**
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the value type
    * @return
    *   valid data in this interval
    */
  infix def withValue[V](value: V): ValidData[V, D] = ValidData(value, this)

  /**
    * Returns individual discrete domain points in this interval. (Empty if domain values are continuous in any
    * dimension.)
    */
  def points: Iterable[D] = start.pointsTo(end)

  /**
    * Tests if there is no gap or overlap between this and that, and they could be merged to form a single interval.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isAdjacentTo(that: Interval[D]): Boolean = isLeftAdjacentTo(that) || isRightAdjacentTo(that)

  /**
    * Tests if this interval is to the left of/below/behind/etc. that interval, and there is no gap or overlap between
    * them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLeftAdjacentTo(that: Interval[D]): Boolean = domainLike.intervalIsLeftAdjacentTo(this, that)

  /**
    * Tests if this interval is to the right of/above/in front of/etc. that interval, and there is no gap or overlap
    * between them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isRightAdjacentTo(that: Interval[D]): Boolean = that isLeftAdjacentTo this

  /**
    * Tests if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   the interval to test.
    */
  infix def intersects(that: Interval[D]): Boolean = (this ∩ that).isDefined

  /**
    * Finds the intersection of this with that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and None otherwise.
    */
  infix def intersectionWith(that: Interval[D]): Option[Interval[D]] =
    val (maxStart, minEnd) = (this.start maxStart that.start, this.end minEnd that.end)
    if domainLike.validIntervalBounds(maxStart, minEnd) then Some(Interval(maxStart, minEnd)) else None

  /**
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gaps that may
    * exist between them. So it is a proper union in the cases where this and that are adjacent (see
    * [[https://en.wikipedia.org/wiki/Union_(set_theory)]]), and a bit more than that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  infix def joinedWith(that: Interval[D]): Interval[D] =
    Interval(this.start minStart that.start, this.end maxEnd that.end)

  /**
    * Returns the interval to the "right" of this one in all dimensions if one can be constructed (i.e., no dimensions
    * of this interval end at the `Top`), otherwise returns None.
    */
  def after: Option[Interval[D]] =
    val endComplement = end.rightAdjacent
    if endComplement equiv domainLike.top then None
    else Some(Interval.intervalFrom(endComplement))

  /**
    * Returns the interval to the "left" of this one in all dimensions if one can be constructed (i.e., no dimensions of
    * this interval start at the `Bottom`), otherwise returns None.
    */
  def before: Option[Interval[D]] =
    val startComplement = start.leftAdjacent
    if startComplement equiv domainLike.bottom then None
    else Some(Interval.intervalTo(startComplement))

  /**
    * Returns a new interval starting at the provided value.
    *
    * @param newStart
    *   the start of the new interval
    */
  def from(newStart: D): Interval[D] = copy(start = newStart)

  /**
    * Returns a new interval starting adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new start of the new interval should be adjacent
    */
  def fromAfter(adjacentDomain: D): Interval[D] = from(adjacentDomain.rightAdjacent)

  /**
    * Returns a new interval with the same end as this interval but with an unbounded start.
    */
  def fromBottom: Interval[D] = from(domainLike.bottom)

  /**
    * Returns a new interval ending at the provided value.
    *
    * @param newEnd
    *   the end of the new interval
    */
  def to(newEnd: D): Interval[D] = copy(end = newEnd)

  /**
    * Returns a new interval ending adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new end of the new interval should be adjacent
    */
  def toBefore(adjacentDomain: D): Interval[D] = to(adjacentDomain.leftAdjacent)

  /**
    * Returns a new interval with the same start as this interval but with an unbounded end.
    */
  def toTop: Interval[D] = to(domainLike.top)

  /**
    * Returns a new singleton interval containing only the start of this interval.
    */
  def atStart: Interval[D] = to(start)

  /**
    * Returns a new singleton interval containing only the end of this interval.
    */
  def atEnd: Interval[D] = from(end)

  /**
    * Excludes that interval from this one. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    *
    * There are three possible outcomes in each dimension:
    *   1. this interval is a subset of that one, so once it is excluded, nothing remains. `Remainder.None` is returned.
    *   1. that either lies outside of this or has a simple edge intersection (only contains the start or the end, but
    *      not both), and a single interval remains. `Remainder.Single` is returned.
    *   1. that interval is a proper subset of this one, containing neither the start nor the end of this, so this
    *      interval gets split, and two intervals remain. `Remainder.Split` is returned
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   A tuple of each dimension-specific `Remainder` after excluding that.
    */
  infix def excluding(that: Interval[D]): NonEmptyTuple =
    domainLike.intervalExcluding(this, that)

  /**
    * Similar to [[excluding]], separates each dimension of the current interval into the intersection with that
    * interval along with the remainder intervals covering this, i.e., the union of all the returned intervals will
    * match this. The interval collection returned will be ordered by interval starts and will be disjoint. In each
    * dimension, there are three possible outcomes:
    *   1. just one interval (this interval) is returned if this is a subset of that or this doesn't intersect that.
    *   1. two intervals (the intersection with that along with the portion of this lying outside of that) are returned
    *      if this has a simple edge intersection.
    *   1. three intervals (the intersection with that along with both portions of this that are before and after that)
    *      are returned if that is a proper subset of this, containing neither the start nor the end of this.
    *
    * So for n dimensions, the smallest possible returned collection will contain just one interval (i.e., case 1 in all
    * dimensions), and the largest will contain 3<sup>n</sup> intervals (i.e., case 3 in all dimensions).
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   A collection of intervals covering this without overlaps after being separated by that.
    */
  infix def separateUsing(that: Interval[D]): Iterable[Interval[D]] =
    domainLike.intervalSeparateUsing(this, that)

  /**
    * Returns the gap between this and that interval if one exists.
    *
    * @param that
    *   the interval to evaluate.
    * @return
    *   if one exists, some interval representing the gap between this and that in all dimensions, and None otherwise.
    */
  infix def gapWith(that: Interval[D]): Option[Interval[D]] =
    domainLike.intervalGapWith(this, that)

  /**
    * Cross this interval with that one-dimensional interval to arrive at a new higher-dimensional interval.
    * @param that
    *   a one-dimensional interval to be appended
    * @tparam X
    *   domain value type for that interval
    * @return
    *   a new higher-dimensional interval with that interval appended.
    */
  infix def x[X: DomainValueLike](that: Interval1D[X])(using
    DomainLike[Tuple.Append[D, Domain1D[X]]]
  ): Interval[Tuple.Append[D, Domain1D[X]]] =
    Interval(start x that.start, end x that.end)

  /**
    * Test for equivalence by comparing the start and end of this and that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this and that have the same start and end.
    */
  infix def equiv(that: Interval[D]): Boolean = (start equiv that.start) && (end equiv that.end)

  // Use mathematical interval notation -- default.
  override def toString: String = domainLike.intervalToString(this, withBraces = true)

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String =
    domainLike.intervalToCodeLikeString(this, withParens = false)

  /*
   * Methods specific to Interval, not available in Interval1D (though Interval1D can be converted implicitly to
   * Interval, and then all these will work).
   */

  /**
    * Alternative to toString for something that looks more like code. Use when having an interval with more than one
    * dimension enclosed in parens is preferred.
    */
  def toCodeLikeStringWithParens: String =
    domainLike.intervalToCodeLikeString(this, withParens = true)

  /**
    * Prepend that one-dimensional interval to this interval to arrive at a new higher-dimensional interval.
    *
    * @param that
    *   a one-dimensional interval to be prepended
    * @tparam X
    *   domain value type for that interval
    * @return
    *   a new higher-dimensional interval with that interval prepended.
    */
  infix def withHead[X: DomainValueLike](that: Interval1D[X])(using
    DomainLike[Domain1D[X] *: D]
  ): Interval[Domain1D[X] *: D] =
    Interval(start withHead that.start, end withHead that.end)

  /**
    * Extract a one-dimensional interval in a particular dimension.
    *
    * @param dimensionIndex
    *   the dimension to extract (e.g., the head dimension is index 0)
    * @tparam H
    *   the domain value type of the element at the extracted dimension. There is a type safety check that ensures the
    *   domain type for the extracted dimension is `Domain1D[H]`.
    * @return
    *   the one-dimensional interval.
    */
  def apply[H: DomainValueLike](dimensionIndex: Int)(using
    Tuple.Elem[D, dimensionIndex.type] =:= Domain1D[H]
  ): Interval1D[H] =
    Interval1D(start(dimensionIndex), end(dimensionIndex))

  /**
    * Extract the one-dimensional head interval. (Equivalent to `apply[H](0)`)
    *
    * @tparam H
    *   the domain value type of the head element. There is a type safety check that ensures the head domain type is
    *   `Domain1D[H]`.
    * @return
    *   the one-dimensional head interval.
    */
  def headInterval1D[H: DomainValueLike](using
    Tuple.Head[D] =:= Domain1D[H]
  ): Interval1D[H] =
    Interval1D(start.head, end.head)

  /**
    * For an n-dimensional interval where n > 1, extracts the tail interval of n-1 dimensions.
    *
    * There are type safety checks that ensure this is not a one-dimensional interval and that the tail forms a valid
    * interval.
    *
    * @return
    *   the tail interval of n-1 dimensions.
    */
  def tailInterval(using
    Tuple.Tail[D] =:= NonEmptyTail[D],
    DomainLike[NonEmptyTail[D]]
  ): Interval[NonEmptyTail[D]] = Interval(start.tail, end.tail)

  /**
    * Returns a new interval with an updated head interval and the other dimensions unchanged.
    *
    * @tparam H
    *   the domain value type of the head element. There are type safety checks that ensure the head domain type is
    *   `Domain1D[H]` and that it can be prepended to the tail to form the original domain.
    * @param update
    *   function to apply to this head interval, used in the head dimension of the returned interval.
    */
  def withHeadUpdate[H: DomainValueLike](update: Interval1D[H] => Interval1D[H])(using
    Tuple.Head[D] =:= Domain1D[H],
    Domain1D[H] *: Tuple.Tail[D] =:= D
  ): Interval[D] =
    val updatedHead = update(headInterval1D[H])
    Interval(updatedHead.start *: start.tail, updatedHead.end *: end.tail)

  /**
    * Tests if this interval contains a specific element of the domain.
    *
    * @param domain
    *   domain element to test.
    * @return
    *   true if the domain element is contained in this interval.
    */
  infix def contains(domain: D): Boolean =
    domain.isClosedOrUnbounded && // strictly speaking, open points are not "contained" in anything
      (domain afterOrAtStart start) && (domain beforeOrAtEnd end)

  /**
    * Tests if that is a subset (proper or improper) of this.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if that is a subset of this.
    */
  infix def contains(that: Interval[D]): Boolean =
    (this ∩ that).contains(that)

  /**
    * Tests if this is a subset (proper or improper) of that. See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: Interval[D]): Boolean =
    that contains this

  /**
    * Approximate this interval as a box in double space based on the domain ordered hash.
    *
    * @return
    *   a new box that can be managed in a box search tree
    */
  def asBox: Box =
    Box(start.asCoordinate, end.asCoordinate)

  /**
    * Tests if there is no fixed start or end - spans the entire domain.
    */
  def isUnbounded: Boolean =
    start.isUnbounded && end.isUnbounded

  /**
    * Tests if this and that have the same start.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameStartAs(that: Interval[D]): Boolean =
    start equiv that.start

  /**
    * Tests if this and that have the same end.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameEndAs(that: Interval[D]): Boolean =
    end equiv that.end

  /*
   * Equivalent symbolic method names
   */

  /**
    * Same as [[isAdjacentTo]]
    *
    * Tests if there is no gap or overlap between this and that, and they could be merged to form a single interval.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def ~(that: Interval[D]): Boolean = isAdjacentTo(that)

  /**
    * Same as [[isLeftAdjacentTo]]
    *
    * Tests if this interval is to the left of/below/behind/etc. that interval, and there is no gap or overlap between
    * them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def ~>(that: Interval[D]): Boolean = isLeftAdjacentTo(that)

  /**
    * Same as [[withValue]]
    *
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the value type
    * @return
    *   valid data in this interval
    */
  infix def ->[V](value: V): ValidData[V, D] = withValue(value)

  /**
    * Same as [[intersectionWith]].
    *
    * Finds the intersection between this and that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and None otherwise.
    */
  def ∩(that: Interval[D]): Option[Interval[D]] = this intersectionWith that

  /**
    * Same as [[joinedWith]].
    *
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gaps that may
    * exist between them. So it is a proper union in the cases where this and that are adjacent (see
    * [[https://en.wikipedia.org/wiki/Union_(set_theory)]]), and a bit more than that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  def ∪(that: Interval[D]): Interval[D] = this joinedWith that

  /**
    * Same as [[isSubsetOf]].
    *
    * Tests if this is a subset (proper or improper) of that. See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  def ⊆(that: Interval[D]): Boolean = this isSubsetOf that

  /**
    * Same as [[excluding]].
    *
    * Excludes that interval from this one. The horizontal, vertical, and depth results are returned as a tuple. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion (a tuple of Interval1D.ExclusionRemainder).
    */
  def \(that: Interval[D]): NonEmptyTuple = this excluding that

/**
  * Companion for the multidimensional interval used in defining and operating on valid data.
  */
object Interval:
  import DomainLike.given

  object Patterns:

    object x_: {

      /**
        * Decomposes a multidimensional interval by extracting its constituent head 1D interval and remaining tail
        * interval. It is essentially the inverse of `Interval.x`. Useful when chaining a match on a fixed number of
        * dimensions, where this pattern will match in all but the last position, e.g.,
        * {{{
        *   dataIntervals.collect:
        *     case horizontal x_: vertical x_: depth =>
        *       randSubinterval(horizontal) x randSubinterval(vertical) x randSubinterval(depth)
        * }}}
        */
      def unapply[D <: NonEmptyTuple, H](i: Interval[D])(using
        Tuple.Head[D] =:= Domain1D[H],
        Tuple.Tail[D] =:= NonEmptyTail[D],
        DomainLike[NonEmptyTail[D]],
        DomainValueLike[H]
      ): (Interval1D[H], Interval[NonEmptyTail[D]]) =
        (Interval1D(i.start.head, i.end.head), Interval(i.start.tail, i.end.tail))

      /**
        * Specific to a two-dimensional interval, decomposes it by extracting its two constituent 1D intervals. It is
        * essentially the inverse of `Interval1D.x`. Useful when chaining a match on a fixed number of dimensions, where
        * this pattern will match in the last position, e.g.,
        * {{{
        *   dataIntervals.collect:
        *     case horizontal x_: vertical x_: depth =>
        *       randSubinterval(horizontal) x randSubinterval(vertical) x randSubinterval(depth)
        * }}}
        */
      def unapply[H1: DomainValueLike, H2: DomainValueLike](
        i: Interval.In2D[H1, H2]
      ): (Interval1D[H1], Interval1D[H2]) =
        (i.start, i.end) match
          case ((s1, s2), (e1, e2)) => (Interval1D(s1, e1), Interval1D(s2, e2))
    }

    object ->: {

      /**
        * Decomposes valid data by extracting its constituent value and interval. It is essentially the inverse of
        * `Interval.->`. Useful when matching a valid value in an interval of a fixed number of dimensions, e.g.,
        * {{{
        *   validData.collect:
        *     case (horizontal x_: vertical) ->: value =>
        *       (vertical x horizontal) -> s"$value (flipped)"
        * }}}
        */
      def unapply[V, D <: NonEmptyTuple: DomainLike](
        data: ValidData[V, D]
      ): (Interval[D], V) = (data.interval, data.value)
    }

  // used in return type of excluding
  enum Remainder[+G]:
    case None
    case Single(g: G)
    case Split(left: G, right: G)

  type In1D[R1] = Interval[Domain.In1D[R1]]
  type In2D[R1, R2] = Interval[Domain.In2D[R1, R2]]
  type In3D[R1, R2, R3] = Interval[Domain.In3D[R1, R2, R3]]
  type In4D[R1, R2, R3, R4] = Interval[Domain.In4D[R1, R2, R3, R4]]

  def in1D[T1: DomainValueLike](
    horizontal: Interval1D[T1]
  ): Interval.In1D[T1] = Interval(
    horizontal.start,
    horizontal.end
  )
  def in2D[T1: DomainValueLike, T2: DomainValueLike](
    horizontal: Interval1D[T1],
    vertical: Interval1D[T2]
  ): Interval.In2D[T1, T2] = Interval(
    horizontal.start x vertical.start,
    horizontal.end x vertical.end
  )
  def in3D[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike](
    horizontal: Interval1D[T1],
    vertical: Interval1D[T2],
    depth: Interval1D[T3]
  ): Interval.In3D[T1, T2, T3] = Interval(
    horizontal.start x vertical.start x depth.start,
    horizontal.end x vertical.end x depth.end
  )
  def in4D[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    horizontal: Interval1D[T1],
    vertical: Interval1D[T2],
    depth: Interval1D[T3],
    fourth: Interval1D[T4]
  ): Interval.In4D[T1, T2, T3, T4] = Interval(
    horizontal.start x vertical.start x depth.start x fourth.start,
    horizontal.end x vertical.end x depth.end x fourth.end
  )

  /**
    * Returns an interval from the input value that is unbounded on the right.
    */
  def intervalFrom[D <: NonEmptyTuple](s: D)(using domainLike: DomainLike[D]): Interval[D] = apply(s, domainLike.top)

  /**
    * Returns an interval from after the input value that is unbounded on the right.
    */
  def intervalFromAfter[D <: NonEmptyTuple](s: D)(using domainLike: DomainLike[D]): Interval[D] =
    apply(s.rightAdjacent, domainLike.top)

  /**
    * Returns an interval to the input value that is unbounded on the left.
    */
  def intervalTo[D <: NonEmptyTuple](e: D)(using domainLike: DomainLike[D]): Interval[D] = apply(domainLike.bottom, e)

  /**
    * Returns an interval to before the input value that is unbounded on the left.
    */
  def intervalToBefore[D <: NonEmptyTuple](e: D)(using domainLike: DomainLike[D]): Interval[D] =
    apply(domainLike.bottom, e.leftAdjacent)

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[D <: NonEmptyTuple: DomainLike](s: D): Interval[D] =
    val closestPoint = s.closeIfOpen
    interval(closestPoint, closestPoint)

  /**
    * Returns an interval that starts and ends at the different values.
    */
  def interval[D <: NonEmptyTuple: DomainLike](s: D, e: D): Interval[D] = apply(s, e)

  /**
    * Returns the interval between `before` and `after`. This is equivalent to `before.gapWith(after).get`, but without
    * intersection and adjacency checks. Only use this function if you know there is a gap between `before` and `after`,
    * e.g., they are exclusion remainders.
    *
    * @param before
    *   interval on the left/bottom/back side
    * @param after
    *   interval on the right/top/front side
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   the interval made from the gap between the two inputs
    */
  def between[D <: NonEmptyTuple: DomainLike](
    before: Interval[D],
    after: Interval[D]
  ): Interval[D] = interval(before.end.rightAdjacent, after.start.leftAdjacent)

  /**
    * Returns an interval unbounded on both the left and right.
    */
  def unbounded[D <: NonEmptyTuple](using domainLike: DomainLike[D]): Interval[D] =
    Interval(domainLike.bottom, domainLike.top)

  /*
   * These methods operate on collections of intervals.
   */

  /**
    * Generic compression algorithm used by both `compress` and `DimensionalBase.compressInPlace`.
    *
    * @param initialState
    *   initial state
    * @param result
    *   extracts the result from the final state
    * @param dataIterable
    *   based on current state, extracts the data iterable
    * @param interval
    *   extracts the interval from the data
    * @param valueMatch
    *   checks if the values of two data elements match
    * @param lookup
    *   based on the current state, looks up data by domain
    * @param compressAdjacent
    *   based on the current state, applies a compression action to two adjacent data elements resulting in a new state
    * @tparam State
    *   the state type
    * @tparam Data
    *   the data type
    * @tparam Result
    *   the result type
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   a result extracted from the final state
    */
  def compressGeneric[State, Data, Result, D <: NonEmptyTuple](
    initialState: State,
    result: State => Result,
    dataIterable: State => Iterable[Data],
    interval: Data => Interval[D],
    valueMatch: (Data, Data) => Boolean,
    lookup: (State, D) => Option[Data],
    compressAdjacent: (Data, Data, State) => State
  )(using domainLike: DomainLike[D]): Result =
    /*
     * Each mutation gives rise to other compression possibilities. And applying a compression action can invalidate
     * the remainder of the actions (e.g., three-in-a-row). In all dimensions greater than one, there is no safe order
     * to fold over ordered intervals to avoid these issues. Instead, we evaluate every entry with every other entry,
     *  get the first compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(state: State): Result =
      val compressionActions: Iterator[State => State] = for
        leftData <- dataIterable(state).iterator
        rightAdjacentKey <- domainLike.intervalRightAdjacentKeys(interval(leftData))
        rightData <- lookup(state, rightAdjacentKey)
        if valueMatch(leftData, rightData) && (interval(leftData) ~> interval(rightData))
      yield compressAdjacent(leftData, rightData, _)

      compressionActions.nextOption() match
        case None         => result(state) // done
        case Some(update) => compressRecursively(update(state))

    compressRecursively(initialState)

  /**
    * Compresses a collection of intervals by joining all adjacent intervals.
    *
    * @param intervals
    *   a collection of intervals.
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[D <: NonEmptyTuple](
    intervals: Iterable[Interval[D]]
  )(using domainLike: DomainLike[D]): Iterable[Interval[D]] = compressGeneric(
    initialState = TreeMap.from(intervals.map(i => i.start -> i)), // intervals by start
    result = _.values,
    dataIterable = _.values,
    interval = identity, // data are just intervals
    valueMatch = (_, _) => true, // no value to match
    lookup = _.get(_),
    compressAdjacent = (r, s, treeMap) => treeMap.removed(s.start).updated(r.start, r ∪ s)
  )

  /**
    * Checks if the collection of intervals is compressible. That is, are there any intervals that are adjacent to, or
    * intersecting with, their neighbors. If true, calling [[compress]] on the collection results in a smaller
    * collection covering the same domain.
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[D <: NonEmptyTuple: DomainLike](intervals: Iterable[Interval[D]]): Boolean =
    intervals.exists: left =>
      intervals.exists: right =>
        (left < right) && ((left ~> right) || (left intersects right))

  /**
    * Checks if the collection of intervals is disjoint. That is, all neighboring intervals do not intersect. See
    * [[https://en.wikipedia.org/wiki/Disjoint_sets]].
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[D <: NonEmptyTuple: DomainLike](intervals: Iterable[Interval[D]]): Boolean =
    intervals.forall: r =>
      intervals
        .filter: d =>
          r.start < d.start
        .forall: d =>
          !(d intersects r)

  /**
    * Finds all intervals, including all overlaps and gaps between intervals, as intervals. Inputs may be overlapping.
    * The result is disjoint and covers the span of the input intervals.
    *
    * @param intervals
    *   collection of intervals
    * @tparam D
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[D <: NonEmptyTuple](
    intervals: Iterable[Interval[D]]
  )(using domainLike: DomainLike[D]): Iterable[Interval[D]] =
    domainLike.intervalUniqueIntervals(intervals)

  /**
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps). See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]]. Result invariants:
    *   - `isDisjoint(intervals ++ complement(intervals)) == true`
    *   - `compress(intervals ++ complement(intervals)).toSeq == Seq(unbounded)`
    *
    * @param intervals
    *   a collection of intervals -- must be disjoint and ordered by start.
    * @tparam T
    *   the domain type -- [[DomainLike]] non-empty tuples.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def complement[T <: NonEmptyTuple](
    intervals: Iterable[Interval[T]]
  )(using domainLike: DomainLike[T]): Iterable[Interval[T]] =
    immutable.Data(intervals.map(_ -> false)).fill(unbounded -> true).filter(_.value).domain

  /**
    * Intervals are ordered by start
    */
  given [D <: NonEmptyTuple: DomainLike](using domainOrder: Ordering[D]): Ordering[Interval[D]] with
    override def compare(x: Interval[D], y: Interval[D]): Int = domainOrder.compare(x.start, y.start)
