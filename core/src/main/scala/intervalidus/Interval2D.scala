package intervalidus

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A two-dimensional interval over a contiguous set of domain values in `T1` and `T2`. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @param horizontal
  *   the abscissa interval: the horizontal X-axis
  * @param vertical
  *   the ordinate interval: the vertical Y-axis
  * @tparam T1
  *   a domain value type for this interval's horizontal domain
  * @tparam T2
  *   a domain value type for this interval's vertical domain
  */
case class Interval2D[T1: DomainValueLike, T2: DomainValueLike](
  horizontal: Interval1D[T1],
  vertical: Interval1D[T2]
) extends IntervalLike[Domain2D[T1, T2], Interval2D[T1, T2]]:

  import Interval1D.Remainder

  override type ExclusionRemainder = (Remainder[Interval1D[T1]], Remainder[Interval1D[T2]])

  override infix def withValue[V](value: V): ValidData2D[V, T1, T2] = ValidData2D(value, this)

  override val start: Domain2D[T1, T2] = horizontal.start x vertical.start

  override val end: Domain2D[T1, T2] = horizontal.end x vertical.end

  override infix def contains(domainElement: Domain2D[T1, T2]): Boolean =
    (this.horizontal contains domainElement.horizontalIndex) && (this.vertical contains domainElement.verticalIndex)

  override def points: Iterable[Domain2D[T1, T2]] =
    for
      horizontalPoint <- horizontal.points
      verticalPoint <- vertical.points
    yield horizontalPoint x verticalPoint

  override infix def isAdjacentTo(that: Interval2D[T1, T2]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that) ||
      (this isLowerAdjacentTo that) || (this isUpperAdjacentTo that)

  override infix def intersects(that: Interval2D[T1, T2]): Boolean =
    (this.horizontal intersects that.horizontal) && (this.vertical intersects that.vertical)

  override infix def intersectionWith(that: Interval2D[T1, T2]): Option[Interval2D[T1, T2]] =
    for
      horizontalIntersection <- horizontal intersectionWith that.horizontal
      verticalIntersection <- vertical intersectionWith that.vertical
    yield horizontalIntersection x verticalIntersection

  override infix def joinedWith(that: Interval2D[T1, T2]): Interval2D[T1, T2] =
    (this.horizontal joinedWith that.horizontal) x (this.vertical joinedWith that.vertical)

  override infix def contains(that: Interval2D[T1, T2]): Boolean =
    (this.horizontal contains that.horizontal) && (this.vertical contains that.vertical)

  override def after: Option[Interval2D[T1, T2]] = (horizontal.after, vertical.after) match
    case (Some(horizontalAfter), Some(verticalAfter)) => Some(Interval2D(horizontalAfter, verticalAfter))
    case _                                            => None

  override def before: Option[Interval2D[T1, T2]] = (horizontal.before, vertical.before) match
    case (Some(horizontalBefore), Some(verticalBefore)) => Some(Interval2D(horizontalBefore, verticalBefore))
    case _                                              => None

  override def from(newStart: Domain2D[T1, T2]): Interval2D[T1, T2] =
    horizontal.from(newStart.horizontalIndex) x vertical.from(newStart.verticalIndex)

  override def fromBottom: Interval2D[T1, T2] =
    from(Domain1D.Bottom x Domain1D.Bottom)

  override def to(newEnd: Domain2D[T1, T2]): Interval2D[T1, T2] =
    horizontal.to(newEnd.horizontalIndex) x vertical.to(newEnd.verticalIndex)

  override def toTop: Interval2D[T1, T2] =
    to(Domain1D.Top x Domain1D.Top)

  override infix def isLeftAdjacentTo(that: Interval2D[T1, T2]): Boolean =
    (this.horizontal.end.rightAdjacent equiv that.horizontal.start) && (this.vertical equiv that.vertical)

  override infix def excluding(that: Interval2D[T1, T2]): ExclusionRemainder =
    (this.horizontal excluding that.horizontal, this.vertical excluding that.vertical)

  override infix def gapWith(that: Interval2D[T1, T2]): Option[Interval2D[T1, T2]] =
    for
      horizontalGap <- horizontal gapWith that.horizontal
      verticalGap <- vertical gapWith that.vertical
    yield horizontalGap x verticalGap

  override def toString: String = s"{$horizontal, $vertical}"

  override def toCodeLikeString: String = s"${horizontal.toCodeLikeString} x ${vertical.toCodeLikeString}"

  /**
    * Returns a new two-dimensional interval with the same vertical interval and the provided horizontal interval.
    *
    * @param newHorizontal
    *   the new horizontal interval
    */
  def withHorizontal(newHorizontal: Interval1D[T1]): Interval2D[T1, T2] =
    copy(horizontal = newHorizontal)

  /**
    * Returns a new two-dimensional interval with the same horizontal interval and the provided vertical interval.
    *
    * @param newVertical
    *   the new horizontal interval
    */
  def withVertical(newVertical: Interval1D[T2]): Interval2D[T1, T2] =
    copy(vertical = newVertical)

  /**
    * Returns a new two-dimensional interval with the same vertical interval and an updated horizontal interval.
    *
    * @param update
    *   function to apply to this horizontal interval and used in the returned interval.
    */
  def withHorizontalUpdate(update: Interval1D[T1] => Interval1D[T1]): Interval2D[T1, T2] =
    withHorizontal(update(this.horizontal))

  /**
    * Returns a new two-dimensional interval with the same horizontal interval and an updated vertical interval.
    *
    * @param update
    *   function to apply to this vertical interval and used in the returned interval.
    */
  def withVerticalUpdate(update: Interval1D[T2] => Interval1D[T2]): Interval2D[T1, T2] =
    withVertical(update(this.vertical))

  /**
    * Tests if this interval is below that interval, there is no vertical gap between them, and their horizontal
    * intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLowerAdjacentTo(that: Interval2D[T1, T2]): Boolean =
    (this.vertical.end.rightAdjacent equiv that.vertical.start) && (this.horizontal equiv that.horizontal)

  /**
    * Tests if this interval is above that interval, there is no vertical gap between them, and their horizontal
    * intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isUpperAdjacentTo(that: Interval2D[T1, T2]): Boolean = that isLowerAdjacentTo this

  /**
    * Cross this interval with that interval to arrive at a new three-dimensional interval.
    * @param that
    *   a one-dimensional interval to be used in the depth dimension
    * @tparam T3
    *   domain value type for that interval
    * @return
    *   a new three-dimensional interval with this interval as the horizontal and vertical components and that interval
    *   as the depth component.
    */
  infix def x[T3: DomainValueLike](that: Interval1D[T3]): Interval3D[T1, T2, T3] =
    Interval3D(this.horizontal, this.vertical, that)

  /**
    * Flips this interval by swapping the vertical and horizontal components with one another.
    */
  def flip: Interval2D[T2, T1] = vertical x horizontal

  // equivalent symbolic method names

  override infix def ->[V](value: V): ValidData2D[V, T1, T2] = withValue(value)

/**
  * Companion for the two-dimensional interval used in defining and operating on valid data.
  */
object Interval2D:

  /**
    * Returns an interval unbounded in both dimensions.
    */
  def unbounded[T1: DomainValueLike, T2: DomainValueLike]: Interval2D[T1, T2] =
    Interval1D.unbounded[T1] x Interval1D.unbounded[T2]

  /**
    * Generic compression algorithm used by both `compress` and `Data2DBase.compressInPlace`.
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
    * @tparam S
    *   the state type
    * @tparam D
    *   the data type
    * @tparam R
    *   the result type
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @return
    *   a result extracted from the final state
    */
  def compressGeneric[C, S, D, R, T1: DomainValueLike, T2: DomainValueLike](
    initialState: S,
    result: S => R,
    dataIterable: S => Iterable[D],
    interval: D => Interval2D[T1, T2],
    valueMatch: (D, D) => Boolean,
    lookup: (S, Domain2D[T1, T2]) => Option[D],
    compressAdjacent: (D, D, S) => S
  ): R =
    /*
     * Each mutation gives rise to other compression possibilities. And applying a compression action
     * can invalidate the remainder of the actions (e.g., three-in-a-row). Unlike in one dimension, there
     * is no safe order to fold over to avoid these issues. So, instead, we evaluate every entry with every
     * other entry, get the first compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(state: S): R =
      val maybeUpdate = dataIterable(state)
        .map: r =>
          val key = interval(r).start
          def rightKey = key.copy(horizontalIndex = interval(r).horizontal.end.rightAdjacent)
          def upperKey = key.copy(verticalIndex = interval(r).vertical.end.rightAdjacent)
          def rightAdjacent =
            lookup(state, rightKey).filter(s => valueMatch(r, s) && (interval(s) isRightAdjacentTo interval(r)))
          def upperAdjacent =
            lookup(state, upperKey).filter(s => valueMatch(r, s) && (interval(s) isUpperAdjacentTo interval(r)))
          rightAdjacent // preferred
            .orElse(upperAdjacent) // next preferred
            .map: s =>
              () => compressAdjacent(r, s, state)
        .collectFirst:
          case Some(updated) => updated
      maybeUpdate match
        case None              => result(state) // done
        case Some(updateState) => compressRecursively(updateState())

    compressRecursively(initialState)

  /**
    * Compresses a collection of intervals by joining all adjacent intervals.
    *
    * @param intervals
    *   a collection of disjoint intervals.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T1: DomainValueLike, T2: DomainValueLike](
    intervals: Iterable[Interval2D[T1, T2]]
  ): Iterable[Interval2D[T1, T2]] = compressGeneric(
    initialState = TreeMap.from(intervals.map(i => i.start -> i)), // intervals by start
    result = _.values,
    dataIterable = _.values,
    interval = identity, // data are just intervals
    valueMatch = (_, _) => true, // no value to match
    lookup = _.get(_),
    compressAdjacent = (r, s, state) => state.removed(s.start).updated(r.start, r ∪ s)
  )

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[T1: DomainValueLike, T2: DomainValueLike](s: Domain2D[T1, T2]): Interval2D[T1, T2] =
    apply(Interval1D.intervalAt(s.horizontalIndex), Interval1D.intervalAt(s.verticalIndex))

  /*
   * These methods operate on collections of two-dimensional intervals.
   */

  /**
    * Checks if the collection of two-dimensional intervals is compressible. That is, are there any intervals that are
    * adjacent to, or intersecting with, their neighbors in one dimension while being equivalent to the same neighbor in
    * the other dimension. Because there is no natural order to find all adjacent neighbors, all pairs of intervals are
    * considered.
    *
    * @param intervals
    *   a collection of two-dimensional intervals.
    * @tparam T1
    *   a domain value type for the horizontal interval domain.
    * @tparam T2
    *   a domain value type for the vertical interval domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T1: DomainValueLike, T2: DomainValueLike](
    intervals: Iterable[Interval2D[T1, T2]]
  ): Boolean =
    // evaluates every pair of 2D intervals twice, so we only need to check for left/lower adjacency
    intervals.exists: r =>
      intervals
        .filter: d =>
          !(d equiv r)
        .exists: d =>
          (d isLeftAdjacentTo r) || (d isLowerAdjacentTo r) || (d intersects r)

  /**
    * Tests if there are no intersections between intervals in the collection. See
    * [[https://en.wikipedia.org/wiki/Disjoint_sets]].
    *
    * @param intervals
    *   a collection of two-dimensional intervals.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T1: DomainValueLike, T2: DomainValueLike](
    intervals: Iterable[Interval2D[T1, T2]]
  ): Boolean = !intervals.exists: r =>
    intervals
      .filter(_.start <= r.start) // by symmetry
      .filterNot(_ equiv r)
      .exists(_ intersects r)

  /**
    * Finds all intervals, including all overlaps and gaps between intervals, as intervals. Inputs may be overlapping.
    * The result is disjoint and covers the span of the input intervals.
    *
    * @param intervals
    *   collection of intervals
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T1: DomainValueLike, T2: DomainValueLike](
    intervals: Iterable[Interval2D[T1, T2]]
  ): Iterable[Interval2D[T1, T2]] =
    for
      horizontal <- Interval1D.uniqueIntervals(intervals.map(_.horizontal))
      vertical <- Interval1D.uniqueIntervals(intervals.map(_.vertical))
    yield horizontal x vertical

  /**
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps). See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]]. Result invariants:
    *   - `isDisjoint(intervals ++ complement(intervals)) == true`
    *   - `compress(intervals ++ complement(intervals)).toSeq == Seq(unbounded)`
    *
    * @param intervals
    *   a collection of intervals -- must be disjoint.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @return
    *   a new collection of intervals representing disjoint complement of the input.
    */
  def complement[T1: DomainValueLike, T2: DomainValueLike](
    intervals: Iterable[Interval2D[T1, T2]]
  ): Iterable[Interval2D[T1, T2]] =
    immutable.DataIn2D(intervals.map(_ -> false)).fill(unbounded -> true).filter(_.value).domain
