package intervalidus

import intervalidus.collection.Box2D

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A two-dimensional interval over a contiguous set of discrete values in T1 and T2. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @param horizontal
  *   the abscissa interval: the horizontal X-axis
  * @param vertical
  *   the ordinate interval: the vertical Y-axis
  * @tparam T1
  *   a discrete value type for this interval's horizontal domain
  * @tparam T2
  *   a discrete value type for this interval's vertical domain
  */
case class DiscreteInterval2D[T1: DiscreteValue, T2: DiscreteValue](
  horizontal: DiscreteInterval1D[T1],
  vertical: DiscreteInterval1D[T2]
) extends DimensionalBase.IntervalLike[DiscreteDomain2D[T1, T2], DiscreteInterval2D[T1, T2]]:

  def asBox: Box2D = Box2D(start.asCoordinate, end.asCoordinate)

  override infix def withValue[V](value: V): ValidData2D[V, T1, T2] = ValidData2D(value, this)

  override val start: DiscreteDomain2D[T1, T2] = horizontal.start x vertical.start

  override val end: DiscreteDomain2D[T1, T2] = horizontal.end x vertical.end

  override infix def contains(domainElement: DiscreteDomain2D[T1, T2]): Boolean =
    (this.horizontal contains domainElement.horizontalIndex) && (this.vertical contains domainElement.verticalIndex)

  override infix def isUnbounded: Boolean = this.horizontal.isUnbounded && this.vertical.isUnbounded

  override def points: Iterable[DiscreteDomain2D[T1, T2]] =
    for
      horizontalPoint <- horizontal.points
      verticalPoint <- vertical.points
    yield horizontalPoint x verticalPoint

  override infix def isAdjacentTo(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that) ||
      (this isLowerAdjacentTo that) || (this isUpperAdjacentTo that)

  override infix def hasSameStartAs(that: DiscreteInterval2D[T1, T2]): Boolean =
    (horizontal hasSameStartAs that.horizontal) &&
      (vertical hasSameStartAs that.vertical)

  override infix def hasSameEndAs(that: DiscreteInterval2D[T1, T2]): Boolean =
    (horizontal hasSameEndAs that.horizontal) &&
      (vertical hasSameEndAs that.vertical)

  override infix def intersects(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this.horizontal intersects that.horizontal) && (this.vertical intersects that.vertical)

  override infix def intersectionWith(that: DiscreteInterval2D[T1, T2]): Option[DiscreteInterval2D[T1, T2]] =
    for
      horizontalIntersection <- horizontal intersectionWith that.horizontal
      verticalIntersection <- vertical intersectionWith that.vertical
    yield horizontalIntersection x verticalIntersection

  override infix def joinedWith(that: DiscreteInterval2D[T1, T2]): DiscreteInterval2D[T1, T2] =
    (this.horizontal joinedWith that.horizontal) x (this.vertical joinedWith that.vertical)

  override infix def equiv(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this.horizontal equiv that.horizontal) && (this.vertical equiv that.vertical)

  override infix def contains(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this.horizontal contains that.horizontal) && (this.vertical contains that.vertical)

  override infix def isSubsetOf(that: DiscreteInterval2D[T1, T2]): Boolean = that contains this

  override def after: Option[DiscreteInterval2D[T1, T2]] = (horizontal.after, vertical.after) match
    case (Some(horizontalAfter), Some(verticalAfter)) => Some(DiscreteInterval2D(horizontalAfter, verticalAfter))
    case _                                            => None

  override def before: Option[DiscreteInterval2D[T1, T2]] = (horizontal.before, vertical.before) match
    case (Some(horizontalBefore), Some(verticalBefore)) => Some(DiscreteInterval2D(horizontalBefore, verticalBefore))
    case _                                              => None

  override def toString: String = s"{$horizontal, $vertical}"

  override def toCodeLikeString: String = s"${horizontal.toCodeLikeString} x ${vertical.toCodeLikeString}"

  /**
    * Returns a new two-dimensional interval with the same vertical interval and the provided horizontal interval.
    *
    * @param newHorizontal
    *   the new horizontal interval
    */
  def withHorizontal(newHorizontal: DiscreteInterval1D[T1]): DiscreteInterval2D[T1, T2] =
    copy(horizontal = newHorizontal)

  /**
    * Returns a new two-dimensional interval with the same horizontal interval and the provided vertical interval.
    *
    * @param newVertical
    *   the new horizontal interval
    */
  def withVertical(newVertical: DiscreteInterval1D[T2]): DiscreteInterval2D[T1, T2] =
    copy(vertical = newVertical)

  /**
    * Returns a new two-dimensional interval with the same vertical interval and an updated horizontal interval.
    *
    * @param update
    *   function to apply to this horizontal interval and used in the returned interval.
    */
  def withHorizontalUpdate(update: DiscreteInterval1D[T1] => DiscreteInterval1D[T1]): DiscreteInterval2D[T1, T2] =
    withHorizontal(update(this.horizontal))

  /**
    * Returns a new two-dimensional interval with the same horizontal interval and an updated vertical interval.
    *
    * @param update
    *   function to apply to this vertical interval and used in the returned interval.
    */
  def withVerticalUpdate(update: DiscreteInterval1D[T2] => DiscreteInterval1D[T2]): DiscreteInterval2D[T1, T2] =
    withVertical(update(this.vertical))

  /**
    * Returns true only if this interval is below that interval, there is no vertical gap between them, and their
    * horizontal intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLowerAdjacentTo(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this.vertical.end.successor equiv that.vertical.start) && (this.horizontal equiv that.horizontal)

  /**
    * Returns true only if this interval is to the left of that interval, and there is no gap between them, and their
    * vertical intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLeftAdjacentTo(that: DiscreteInterval2D[T1, T2]): Boolean =
    (this.horizontal.end.successor equiv that.horizontal.start) && (this.vertical equiv that.vertical)

  /**
    * Returns true only if this interval is above that interval, there is no vertical gap between them, and their
    * horizontal intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isUpperAdjacentTo(that: DiscreteInterval2D[T1, T2]): Boolean = that isLowerAdjacentTo this

  /**
    * Returns true only if this interval is to the right of that interval, and there is no gap between them, and their
    * vertical intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isRightAdjacentTo(that: DiscreteInterval2D[T1, T2]): Boolean = that isLeftAdjacentTo this

  import DiscreteInterval1D.Remainder

  /**
    * Excludes that interval from this one. The horizontal and vertical results are returned as a pair
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion.
    */
  infix def excluding(
    that: DiscreteInterval2D[T1, T2]
  ): (Remainder[DiscreteInterval1D[T1]], Remainder[DiscreteInterval1D[T2]]) =
    (this.horizontal excluding that.horizontal, this.vertical excluding that.vertical)

  /**
    * Cross this interval with that interval to arrive at a new three-dimensional interval.
    * @param that
    *   a one-dimensional interval to be used in the depth dimension
    * @tparam T3
    *   discrete value type for that interval
    * @return
    *   a new three-dimensional interval with this interval as the horizontal and vertical components and that interval
    *   as the depth component.
    */
  infix def x[T3: DiscreteValue](that: DiscreteInterval1D[T3]): DiscreteInterval3D[T1, T2, T3] =
    DiscreteInterval3D(this.horizontal, this.vertical, that)

  /**
    * Flips this interval by swapping the vertical and horizontal components with one another.
    */
  def flip: DiscreteInterval2D[T2, T1] = vertical x horizontal

  // equivalent symbolic method names

  override infix def ->[V](value: V): ValidData2D[V, T1, T2] = withValue(value)

  /**
    * Same as [[excluding]].
    *
    * Excludes that interval from this one. The horizontal and vertical results are returned as a pair
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion.
    */
  def \(
    that: DiscreteInterval2D[T1, T2]
  ): (Remainder[DiscreteInterval1D[T1]], Remainder[DiscreteInterval1D[T2]]) = this excluding that

/**
  * Companion for the two-dimensional interval used in defining and operating on a valid data.
  */
object DiscreteInterval2D:

  /**
    * Returns an interval unbounded in both dimensions.
    */
  def unbounded[T1: DiscreteValue, T2: DiscreteValue]: DiscreteInterval2D[T1, T2] =
    DiscreteInterval1D.unbounded[T1] x DiscreteInterval1D.unbounded[T2]

  /**
    * Compresses a collection of intervals by joining all adjacent and intersecting intervals.
    *
    * @param intervals
    *   a collection of intervals.
    * @tparam T1
    *   a discrete value type for this interval's horizontal domain.
    * @tparam T2
    *   a discrete value type for this interval's vertical domain.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T1: DiscreteValue, T2: DiscreteValue](
    intervals: Iterable[DiscreteInterval2D[T1, T2]]
  ): Iterable[DiscreteInterval2D[T1, T2]] =
    /*
     * Each mutation gives rise to other compression possibilities. And applying a compression action can invalidate
     * the remainder of the actions (e.g., three-in-a-row). Unlike in one dimension, there is no safe order to fold
     * over to avoid these issues. So, instead, we evaluate every entry with every other entry, get the first
     * compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(
      intervalByStart: TreeMap[DiscreteDomain2D[T1, T2], DiscreteInterval2D[T1, T2]]
    ): Iterable[DiscreteInterval2D[T1, T2]] =
      val maybeUpdate = intervalByStart.values
        .map: r =>
          def rightKey = r.start.copy(horizontalIndex = r.horizontal.end.successor)
          def upperKey = r.start.copy(verticalIndex = r.vertical.end.successor)
          def rightAdjacent = intervalByStart.get(rightKey).filter(_ isRightAdjacentTo r)
          def upperAdjacent = intervalByStart.get(upperKey).filter(_ isUpperAdjacentTo r)
          rightAdjacent // preferred
            .orElse(upperAdjacent)
            .map: s =>
              () => intervalByStart.removed(s.start).updated(r.start, r ∪ s)
        .collectFirst:
          case Some(updated) => updated
      maybeUpdate match
        case None         => intervalByStart.values // done
        case Some(update) => compressRecursively(update())

    compressRecursively(TreeMap.from(intervals.map(i => i.start -> i)))

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[T1: DiscreteValue, T2: DiscreteValue](s: DiscreteDomain2D[T1, T2]): DiscreteInterval2D[T1, T2] =
    apply(DiscreteInterval1D.intervalAt(s.horizontalIndex), DiscreteInterval1D.intervalAt(s.verticalIndex))

  /*
   * These methods operate on collections of two-dimensional intervals.
   */

  /**
    * Checks if the collection of two-dimensional intervals is compressible. That is, are there any intervals that are
    * adjacent to, or intersecting with, their neighbors in one dimension while being equivalent to the same neighbor in
    * the other dimension. Because there is no natural order to find all compressible neighbors, all pairs of intervals
    * are considered.
    *
    * @param intervals
    *   a collection of two-dimensional intervals -- must be ordered by start.
    * @tparam T1
    *   a discrete value type for this interval's horizontal domain.
    * @tparam T2
    *   a discrete value type for this interval's vertical domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T1: DiscreteValue, T2: DiscreteValue](
    intervals: Iterable[DiscreteInterval2D[T1, T2]]
  ): Boolean =
    // evaluates every pair of 2D intervals twice, so we only need to check for left/lower adjacency
    intervals.exists: r =>
      intervals
        .filter: d =>
          !(d equiv r)
        .exists: d =>
          (d intersects r) || (d isLeftAdjacentTo r) || (d isLowerAdjacentTo r)

  /**
    * Returns true if there are no intersections between intervals in the collection.
    *
    * @param intervals
    *   a collection of two-dimensional intervals.
    * @tparam T1
    *   a discrete value type for this interval's horizontal domain.
    * @tparam T2
    *   a discrete value type for this interval's vertical domain.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T1: DiscreteValue, T2: DiscreteValue](
    intervals: Iterable[DiscreteInterval2D[T1, T2]]
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
    *   a discrete value type for this interval's horizontal domain.
    * @tparam T2
    *   a discrete value type for this interval's vertical domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T1: DiscreteValue, T2: DiscreteValue](
    intervals: Iterable[DiscreteInterval2D[T1, T2]]
  ): Iterable[DiscreteInterval2D[T1, T2]] =
    for
      horizontal <- DiscreteInterval1D.uniqueIntervals(intervals.map(_.horizontal))
      vertical <- DiscreteInterval1D.uniqueIntervals(intervals.map(_.vertical))
    yield horizontal x vertical
