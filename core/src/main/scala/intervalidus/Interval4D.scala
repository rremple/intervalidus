package intervalidus

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A four-dimensional interval over a contiguous set of domain values in `T1`, `T2`, `T3`, and `T4`. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @param horizontal
  *   the abscissa interval: the horizontal X-axis (from left to right)
  * @param vertical
  *   the ordinate interval: the vertical Y-axis (from lower to upper)
  * @param depth
  *   the third dimension interval: the (right-handed) depth Z-axis (from back to front)
  * @param fourth
  *   the fourth dimension interval: the W-axis
  * @tparam T1
  *   a domain value type for this interval's horizontal domain
  * @tparam T2
  *   a domain value type for this interval's vertical domain
  * @tparam T3
  *   a domain value type for this interval's depth domain
  * @tparam T4
  *   a domain value type for this interval's fourth domain
  */
case class Interval4D[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
  horizontal: Interval1D[T1],
  vertical: Interval1D[T2],
  depth: Interval1D[T3],
  fourth: Interval1D[T4]
) extends IntervalLike[Domain4D[T1, T2, T3, T4], Interval4D[T1, T2, T3, T4]]:

  import Interval1D.Remainder

  override type ExclusionRemainder =
    (Remainder[Interval1D[T1]], Remainder[Interval1D[T2]], Remainder[Interval1D[T3]], Remainder[Interval1D[T4]])

  override infix def withValue[V](value: V): ValidData4D[V, T1, T2, T3, T4] = ValidData4D(value, this)

  override val start: Domain4D[T1, T2, T3, T4] = horizontal.start x vertical.start x depth.start x fourth.start

  override val end: Domain4D[T1, T2, T3, T4] = horizontal.end x vertical.end x depth.end x fourth.end

  override infix def contains(domainElement: Domain4D[T1, T2, T3, T4]): Boolean =
    (this.horizontal contains domainElement.horizontalIndex) &&
      (this.vertical contains domainElement.verticalIndex) &&
      (this.depth contains domainElement.depthIndex) &&
      (this.fourth contains domainElement.fourthIndex)

  override def points: Iterable[Domain4D[T1, T2, T3, T4]] =
    for
      horizontalPoint <- horizontal.points
      verticalPoint <- vertical.points
      depthPoint <- depth.points
      fourthPoint <- fourth.points
    yield horizontalPoint x verticalPoint x depthPoint x fourthPoint

  override infix def isAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that) ||
      (this isLowerAdjacentTo that) || (this isUpperAdjacentTo that) ||
      (this isBackAdjacentTo that) || (this isFrontAdjacentTo that) ||
      (this isFourthBeforeAdjacentTo that) || (this isFourthAfterAdjacentTo that)

  override infix def intersects(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.horizontal intersects that.horizontal) &&
      (this.vertical intersects that.vertical) &&
      (this.depth intersects that.depth) &&
      (this.fourth intersects that.fourth)

  override infix def intersectionWith(that: Interval4D[T1, T2, T3, T4]): Option[Interval4D[T1, T2, T3, T4]] =
    for
      horizontalIntersection <- horizontal intersectionWith that.horizontal
      verticalIntersection <- vertical intersectionWith that.vertical
      depthIntersection <- depth intersectionWith that.depth
      fourthIntersection <- fourth intersectionWith that.fourth
    yield horizontalIntersection x verticalIntersection x depthIntersection x fourthIntersection

  override infix def joinedWith(that: Interval4D[T1, T2, T3, T4]): Interval4D[T1, T2, T3, T4] =
    (this.horizontal joinedWith that.horizontal) x
      (this.vertical joinedWith that.vertical) x
      (this.depth joinedWith that.depth) x
      (this.fourth joinedWith that.fourth)

  override infix def contains(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.horizontal contains that.horizontal) &&
      (this.vertical contains that.vertical) &&
      (this.depth contains that.depth) &&
      (this.fourth contains that.fourth)

  override def after: Option[Interval4D[T1, T2, T3, T4]] =
    (horizontal.after, vertical.after, depth.after, fourth.after) match
      case (Some(horizontalAfter), Some(verticalAfter), Some(depthAfter), Some(fourthAfter)) =>
        Some(Interval4D(horizontalAfter, verticalAfter, depthAfter, fourthAfter))
      case _ =>
        None

  override def before: Option[Interval4D[T1, T2, T3, T4]] =
    (horizontal.before, vertical.before, depth.before, fourth.before) match
      case (Some(horizontalBefore), Some(verticalBefore), Some(depthBefore), Some(fourthBefore)) =>
        Some(Interval4D(horizontalBefore, verticalBefore, depthBefore, fourthBefore))
      case _ =>
        None

  override def from(newStart: Domain4D[T1, T2, T3, T4]): Interval4D[T1, T2, T3, T4] =
    horizontal.from(newStart.horizontalIndex) x
      vertical.from(newStart.verticalIndex) x
      depth.from(newStart.depthIndex) x
      fourth.from(newStart.fourthIndex)

  override def fromBottom: Interval4D[T1, T2, T3, T4] =
    from(Domain1D.Bottom x Domain1D.Bottom x Domain1D.Bottom x Domain1D.Bottom)

  override def to(newEnd: Domain4D[T1, T2, T3, T4]): Interval4D[T1, T2, T3, T4] =
    horizontal.to(newEnd.horizontalIndex) x
      vertical.to(newEnd.verticalIndex) x
      depth.to(newEnd.depthIndex) x
      fourth.to(newEnd.fourthIndex)

  override def toTop: Interval4D[T1, T2, T3, T4] =
    to(Domain1D.Top x Domain1D.Top x Domain1D.Top x Domain1D.Top)

  override infix def isLeftAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.horizontal.end.rightAdjacent equiv that.horizontal.start) &&
      (this.vertical equiv that.vertical) && (this.depth equiv that.depth) && (this.fourth equiv that.fourth)

  override infix def excluding(that: Interval4D[T1, T2, T3, T4]): ExclusionRemainder = (
    this.horizontal excluding that.horizontal,
    this.vertical excluding that.vertical,
    this.depth excluding that.depth,
    this.fourth excluding that.fourth
  )

  override infix def gapWith(that: Interval4D[T1, T2, T3, T4]): Option[Interval4D[T1, T2, T3, T4]] =
    for
      horizontalGap <- horizontal gapWith that.horizontal
      verticalGap <- vertical gapWith that.vertical
      depthGap <- depth gapWith that.depth
      fourthGap <- fourth gapWith that.fourth
    yield horizontalGap x verticalGap x depthGap x fourthGap

  override def toString: String = s"{$horizontal, $vertical, $depth, $fourth}"

  override def toCodeLikeString: String =
    s"${horizontal.toCodeLikeString} x ${vertical.toCodeLikeString} x " +
      s"${depth.toCodeLikeString} x ${fourth.toCodeLikeString}"

  /**
    * Returns a new four-dimensional interval with the same vertical, depth, and fourth intervals and the provided
    * horizontal interval.
    *
    * @param newHorizontal
    *   the new horizontal interval
    */
  def withHorizontal(newHorizontal: Interval1D[T1]): Interval4D[T1, T2, T3, T4] =
    copy(horizontal = newHorizontal)

  /**
    * Returns a new four-dimensional interval with the same horizontal, depth, and fourth intervals and the provided
    * vertical interval.
    *
    * @param newVertical
    *   the new horizontal interval
    */
  def withVertical(newVertical: Interval1D[T2]): Interval4D[T1, T2, T3, T4] =
    copy(vertical = newVertical)

  /**
    * Returns a new four-dimensional interval with the same horizontal, vertical, and fourth intervals and the provided
    * depth interval.
    *
    * @param newDepth
    *   the new depth interval
    */
  def withDepth(newDepth: Interval1D[T3]): Interval4D[T1, T2, T3, T4] =
    copy(depth = newDepth)

  /**
    * Returns a new four-dimensional interval with the same horizontal, vertical, and depth intervals and the provided
    * fourth interval.
    *
    * @param newFourth
    *   the new fourth interval
    */
  def withFourth(newFourth: Interval1D[T4]): Interval4D[T1, T2, T3, T4] =
    copy(fourth = newFourth)

  /**
    * Returns a new four-dimensional interval with the same vertical, depth, and fourth intervals and an updated
    * horizontal interval.
    *
    * @param update
    *   function to apply to this horizontal interval and used in the returned interval.
    */
  def withHorizontalUpdate(update: Interval1D[T1] => Interval1D[T1]): Interval4D[T1, T2, T3, T4] =
    withHorizontal(update(this.horizontal))

  /**
    * Returns a new four-dimensional interval with the same horizontal, depth, and fourth intervals and an updated
    * vertical interval.
    *
    * @param update
    *   function to apply to this vertical interval and used in the returned interval.
    */
  def withVerticalUpdate(update: Interval1D[T2] => Interval1D[T2]): Interval4D[T1, T2, T3, T4] =
    withVertical(update(this.vertical))

  /**
    * Returns a new four-dimensional interval with the same horizontal, vertical, and fourth intervals and an updated
    * depth interval.
    *
    * @param update
    *   function to apply to this depth interval and used in the returned interval.
    */
  def withDepthUpdate(update: Interval1D[T3] => Interval1D[T3]): Interval4D[T1, T2, T3, T4] =
    withDepth(update(this.depth))

  /**
    * Returns a new four-dimensional interval with the same horizontal, vertical, and depth intervals and an updated
    * fourth interval.
    *
    * @param update
    *   function to apply to this fourth interval and used in the returned interval.
    */
  def withFourthUpdate(update: Interval1D[T4] => Interval1D[T4]): Interval4D[T1, T2, T3, T4] =
    withFourth(update(this.fourth))

  /**
    * Tests if this interval is below that interval, there is no vertical gap between them, and their horizontal, depth,
    * and fourth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLowerAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.vertical.end.rightAdjacent equiv that.vertical.start) &&
      (this.horizontal equiv that.horizontal) && (this.depth equiv that.depth) && (this.fourth equiv that.fourth)

  /**
    * Tests if this interval is in back of (behind) that interval, and there is no gap between them, and their vertical,
    * horizontal, and fourth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isBackAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.depth.end.rightAdjacent equiv that.depth.start) &&
      (this.vertical equiv that.vertical) && (this.horizontal equiv that.horizontal) && (this.fourth equiv that.fourth)

  /**
    * Tests if this interval is before that interval in the fourth dimension, and there is no gap between them, and
    * their horizontal, vertical, and depth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isFourthBeforeAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean =
    (this.fourth.end.rightAdjacent equiv that.fourth.start) &&
      (this.vertical equiv that.vertical) && (this.horizontal equiv that.horizontal) && (this.depth equiv that.depth)

  /**
    * Tests if this interval is above that interval, there is no vertical gap between them, and their horizontal, depth,
    * and fourth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isUpperAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean = that isLowerAdjacentTo this

  /**
    * Tests if this interval is in front of that interval, and there is no gap between them, and their vertical,
    * horizontal, and fourth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isFrontAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean = that isBackAdjacentTo this

  /**
    * Tests if this interval is after that interval in the fourth dimension, and there is no gap between them, and their
    * horizontal, vertical, and depth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isFourthAfterAdjacentTo(that: Interval4D[T1, T2, T3, T4]): Boolean = that isFourthBeforeAdjacentTo this

  // equivalent symbolic method names

  override infix def ->[V](value: V): ValidData4D[V, T1, T2, T3, T4] = withValue(value)

/**
  * Companion for the four-dimensional interval used in defining and operating on valid data.
  */
object Interval4D:

  /**
    * Returns an interval unbounded in all dimensions.
    */
  def unbounded[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike]
    : Interval4D[T1, T2, T3, T4] =
    Interval1D.unbounded[T1] x Interval1D.unbounded[T2] x Interval1D.unbounded[T3] x Interval1D.unbounded[T4]

  /**
    * Generic compression algorithm used by both `compress` and `Data4DBase.compressInPlace`.
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
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   a result extracted from the final state
    */
  def compressGeneric[S, D, R, T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    initialState: S,
    result: S => R,
    dataIterable: S => Iterable[D],
    interval: D => Interval4D[T1, T2, T3, T4],
    valueMatch: (D, D) => Boolean,
    lookup: (S, Domain4D[T1, T2, T3, T4]) => Option[D],
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
          def frontKey = key.copy(depthIndex = interval(r).depth.end.rightAdjacent)
          def fourthKey = key.copy(fourthIndex = interval(r).fourth.end.rightAdjacent)
          def rightAdjacent =
            lookup(state, rightKey).filter(s => valueMatch(r, s) && (interval(s) isRightAdjacentTo interval(r)))
          def upperAdjacent =
            lookup(state, upperKey).filter(s => valueMatch(r, s) && (interval(s) isUpperAdjacentTo interval(r)))
          def frontAdjacent =
            lookup(state, frontKey).filter(s => valueMatch(r, s) && (interval(s) isFrontAdjacentTo interval(r)))
          def fourthAdjacent =
            lookup(state, fourthKey).filter(s => valueMatch(r, s) && (interval(s) isFourthAfterAdjacentTo interval(r)))
          rightAdjacent // preferred
            .orElse(upperAdjacent) // next preferred
            .orElse(frontAdjacent) // next preferred
            .orElse(fourthAdjacent)
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
    *   a collection of intervals.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    intervals: Iterable[Interval4D[T1, T2, T3, T4]]
  ): Iterable[Interval4D[T1, T2, T3, T4]] = compressGeneric(
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
  def intervalAt[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    s: Domain4D[T1, T2, T3, T4]
  ): Interval4D[T1, T2, T3, T4] = apply(
    Interval1D.intervalAt(s.horizontalIndex),
    Interval1D.intervalAt(s.verticalIndex),
    Interval1D.intervalAt(s.depthIndex),
    Interval1D.intervalAt(s.fourthIndex)
  )

  // These methods operate on collections of four-dimensional intervals.

  /**
    * Checks if the collection of four-dimensional intervals is compressible. That is, are there any intervals that are
    * adjacent to, or intersecting with, their neighbors in one dimension while being equivalent to the same neighbor in
    * the other dimension. Because there is no natural order to find all compressible neighbors, all pairs of intervals
    * are considered.
    *
    * @param intervals
    *   a collection of four-dimensional intervals -- must be ordered by start.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    intervals: Iterable[Interval4D[T1, T2, T3, T4]]
  ): Boolean =
    // evaluates every pair of 2D intervals twice, so we only need to check for left/lower adjacency
    intervals.exists: r =>
      intervals
        .filter: d =>
          !(d equiv r)
        .exists: d =>
          (d isLeftAdjacentTo r) || (d isLowerAdjacentTo r) || (d isBackAdjacentTo r) ||
            (d isFourthBeforeAdjacentTo r) || (d intersects r)

  /**
    * Tests if there are no intersections between intervals in the collection. See
    * [[https://en.wikipedia.org/wiki/Disjoint_sets]].
    *
    * @param intervals
    *   a collection of four-dimensional intervals.
    * @tparam T1
    *   a domain value type for this interval's horizontal domain.
    * @tparam T2
    *   a domain value type for this interval's vertical domain.
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    intervals: Iterable[Interval4D[T1, T2, T3, T4]]
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
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    intervals: Iterable[Interval4D[T1, T2, T3, T4]]
  ): Iterable[Interval4D[T1, T2, T3, T4]] =
    for
      horizontal <- Interval1D.uniqueIntervals(intervals.map(_.horizontal))
      vertical <- Interval1D.uniqueIntervals(intervals.map(_.vertical))
      depth <- Interval1D.uniqueIntervals(intervals.map(_.depth))
      fourth <- Interval1D.uniqueIntervals(intervals.map(_.fourth))
    yield horizontal x vertical x depth x fourth

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
    * @tparam T3
    *   a domain value type for this interval's depth domain.
    * @tparam T4
    *   a domain value type for this interval's fourth domain.
    * @return
    *   a new collection of intervals representing disjoint complement of the input.
    */
  def complement[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
    intervals: Iterable[Interval4D[T1, T2, T3, T4]]
  ): Iterable[Interval4D[T1, T2, T3, T4]] =
    immutable.DataIn4D(intervals.map(_ -> false)).fill(unbounded -> true).filter(_.value).domain
