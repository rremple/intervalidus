package intervalidus

import intervalidus.collection.{Box3D, Coordinate3D}

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A two-dimensional interval over a contiguous set of discrete values in T1 and T2. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @param horizontal
  *   the abscissa interval: the horizontal X-axis (from left to right)
  * @param vertical
  *   the ordinate interval: the vertical Y-axis (from lower to upper)
  * @param depth
  *   the third dimension interval: the (right handed) depth Z-axis (behind to in front)
  * @tparam T1
  *   a discrete value type for this interval's horizontal domain
  * @tparam T2
  *   a discrete value type for this interval's vertical domain
  */
case class DiscreteInterval3D[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
  horizontal: DiscreteInterval1D[T1],
  vertical: DiscreteInterval1D[T2],
  depth: DiscreteInterval1D[T3]
) extends DimensionalBase.IntervalLike[DiscreteDomain3D[T1, T2, T3], DiscreteInterval3D[T1, T2, T3]]:

  def asBox: Box3D = Box3D(start.asCoordinate, end.asCoordinate)

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
  infix def withValue[V](value: V): ValidData3D[V, T1, T2, T3] = ValidData3D(value, this)

  override val start: DiscreteDomain3D[T1, T2, T3] = horizontal.start x vertical.start x depth.start

  override val end: DiscreteDomain3D[T1, T2, T3] = horizontal.end x vertical.end x depth.end

  override infix def contains(domainElement: DiscreteDomain3D[T1, T2, T3]): Boolean =
    (this.horizontal contains domainElement.horizontalIndex) &&
      (this.vertical contains domainElement.verticalIndex) &&
      (this.depth contains domainElement.depthIndex)

  override infix def isUnbounded: Boolean =
    this.horizontal.isUnbounded && this.vertical.isUnbounded && this.depth.isUnbounded

  override def points: Iterable[DiscreteDomain3D[T1, T2, T3]] =
    for
      horizontalPoint <- horizontal.points
      verticalPoint <- vertical.points
      depthPoint <- depth.points
    yield horizontalPoint x verticalPoint x depthPoint

  override def toString: String = s"{$horizontal, $vertical, $depth}"

  override def toCodeLikeString: String =
    s"${horizontal.toCodeLikeString} x ${vertical.toCodeLikeString} x ${depth.toCodeLikeString}"

  /**
    * Returns a new three-dimensional interval with the same vertical and depth intervals and the provided horizontal
    * interval.
    *
    * @param newHorizontal
    *   the new horizontal interval
    */
  def withHorizontal(newHorizontal: DiscreteInterval1D[T1]): DiscreteInterval3D[T1, T2, T3] =
    copy(horizontal = newHorizontal)

  /**
    * Returns a new three-dimensional interval with the same horizontal and depth intervals and the provided vertical
    * interval.
    *
    * @param newVertical
    *   the new horizontal interval
    */
  def withVertical(newVertical: DiscreteInterval1D[T2]): DiscreteInterval3D[T1, T2, T3] =
    copy(vertical = newVertical)

  /**
    * Returns a new three-dimensional interval with the same horizontal and vertical intervals and the provided depth
    * interval.
    *
    * @param newDepth
    *   the new depth interval
    */
  def withDepth(newDepth: DiscreteInterval1D[T3]): DiscreteInterval3D[T1, T2, T3] =
    copy(depth = newDepth)

  /**
    * Returns a new three-dimensional interval with the same vertical and depth intervals and an updated horizontal
    * interval.
    *
    * @param update
    *   function to apply to this horizontal interval and used in the returned interval.
    */
  def withHorizontalUpdate(update: DiscreteInterval1D[T1] => DiscreteInterval1D[T1]): DiscreteInterval3D[T1, T2, T3] =
    withHorizontal(update(this.horizontal))

  /**
    * Returns a new three-dimensional interval with the same horizontal and depth intervals and an updated vertical
    * interval.
    *
    * @param update
    *   function to apply to this vertical interval and used in the returned interval.
    */
  def withVerticalUpdate(update: DiscreteInterval1D[T2] => DiscreteInterval1D[T2]): DiscreteInterval3D[T1, T2, T3] =
    withVertical(update(this.vertical))

  /**
    * Returns a new three-dimensional interval with the same horizontal and vertical intervals and an updated depth
    * interval.
    *
    * @param update
    *   function to apply to this depth interval and used in the returned interval.
    */
  def withDepthUpdate(update: DiscreteInterval1D[T3] => DiscreteInterval1D[T3]): DiscreteInterval3D[T1, T2, T3] =
    withDepth(update(this.depth))

  /**
    * Returns true only if this interval is below that interval, there is no vertical gap between them, and their
    * horizontal and depth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLowerAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.vertical.end.successor equiv that.vertical.start) &&
      (this.horizontal equiv that.horizontal) && (this.depth equiv that.depth)

  /**
    * Returns true only if this interval is to the left of that interval, and there is no gap between them, and their
    * vertical intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLeftAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.horizontal.end.successor equiv that.horizontal.start) &&
      (this.vertical equiv that.vertical) && (this.depth equiv that.depth)

  /**
    * Returns true only if this interval is in back of (behind) that interval, and there is no gap between them, and
    * their vertical and horizontal intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isBackAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.depth.end.successor equiv that.depth.start) &&
      (this.vertical equiv that.vertical) && (this.horizontal equiv that.horizontal)

  /**
    * Returns true only if this interval is above that interval, there is no vertical gap between them, and their
    * horizontal and depth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isUpperAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean = that isLowerAdjacentTo this

  /**
    * Returns true only if this interval is to the right of that interval, and there is no gap between them, and their
    * vertical and depth intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isRightAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean = that isLeftAdjacentTo this

  /**
    * Returns true only if this interval is in front of that interval, and there is no gap between them, and their
    * vertical and horizontal intervals are equivalent.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isFrontAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean = that isBackAdjacentTo this

  /**
    * Returns true only if there is no gap between this and that.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isAdjacentTo(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that) ||
      (this isLowerAdjacentTo that) || (this isUpperAdjacentTo that) ||
      (this isBackAdjacentTo that) || (this isFrontAdjacentTo that)

  /**
    * Returns true only if this and that have the same start.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameStartAs(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (horizontal hasSameStartAs that.horizontal) &&
      (vertical hasSameStartAs that.vertical) &&
      (depth hasSameStartAs that.depth)

  /**
    * Returns true only if this and that have the same end.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameEndAs(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (horizontal hasSameEndAs that.horizontal) &&
      (vertical hasSameEndAs that.vertical) &&
      (depth hasSameEndAs that.depth)

  /**
    * Returns true only if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   the interval to test.
    */
  infix def intersects(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.horizontal intersects that.horizontal) &&
      (this.vertical intersects that.vertical) &&
      (this.depth intersects that.depth)

  /**
    * Finds the intersection between this and that.
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and none otherwise.
    */
  infix def intersectionWith(that: DiscreteInterval3D[T1, T2, T3]): Option[DiscreteInterval3D[T1, T2, T3]] =
    for
      horizontalIntersection <- horizontal intersectionWith that.horizontal
      verticalIntersection <- vertical intersectionWith that.vertical
      depthIntersection <- depth intersectionWith that.depth
    yield horizontalIntersection x verticalIntersection x depthIntersection

  import DiscreteInterval1D.Remainder

  /**
    * Excludes that interval from this one. The horizontal, vertical, and depth results are returned as a tuple.
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion.
    */
  infix def excluding(
    that: DiscreteInterval3D[T1, T2, T3]
  ): (Remainder[DiscreteInterval1D[T1]], Remainder[DiscreteInterval1D[T2]], Remainder[DiscreteInterval1D[T3]]) =
    (this.horizontal excluding that.horizontal, this.vertical excluding that.vertical, this.depth excluding that.depth)

  /**
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gaps that may
    * exist between them. So it is a proper union in the cases where this and that are adjacent, and a bit more than
    * that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  infix def joinedWith(that: DiscreteInterval3D[T1, T2, T3]): DiscreteInterval3D[T1, T2, T3] =
    (this.horizontal joinedWith that.horizontal) x
      (this.vertical joinedWith that.vertical) x
      (this.depth joinedWith that.depth)

  /**
    * Test for equivalence by comparing the horizontal and vertical intervals of this and that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true only if this and that have the same start and end.
    */
  infix def equiv(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.horizontal equiv that.horizontal) && (this.vertical equiv that.vertical) && (this.depth equiv that.depth)

  /**
    * Tests if that is a subset (proper or improper) of this.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if that is a subset of this.
    */
  infix def contains(that: DiscreteInterval3D[T1, T2, T3]): Boolean =
    (this.horizontal contains that.horizontal) &&
      (this.vertical contains that.vertical) &&
      (this.depth contains that.depth)

  /**
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: DiscreteInterval3D[T1, T2, T3]): Boolean = that contains this

  /**
    * If there are intervals after the horizontal, vertical, and depth components, returns the interval after this one
    * in three dimensions. Otherwise returns none. Does not include adjacent intervals behind, above, and to the right.
    */
  def after: Option[DiscreteInterval3D[T1, T2, T3]] = (horizontal.after, vertical.after, depth.after) match
    case (Some(horizontalAfter), Some(verticalAfter), Some(depthAfter)) =>
      Some(DiscreteInterval3D(horizontalAfter, verticalAfter, depthAfter))
    case _ =>
      None

  /**
    * If there are intervals before the horizontal, vertical, and depth components, returns the interval before this one
    * in three dimensions. Otherwise returns none. Does not include adjacent intervals in front of, below and to the
    * left.
    */
  def before: Option[DiscreteInterval3D[T1, T2, T3]] = (horizontal.before, vertical.before, depth.before) match
    case (Some(horizontalBefore), Some(verticalBefore), Some(depthBefore)) =>
      Some(DiscreteInterval3D(horizontalBefore, verticalBefore, depthBefore))
    case _ =>
      None

  /**
    * Flips this interval by swapping the vertical and horizontal components with one another and keeping the depth
    * component unchanged.
    */
  def flipAboutDepth: DiscreteInterval3D[T2, T1, T3] = vertical x horizontal x depth

  /**
    * Flips this interval by swapping the vertical and depth components with one another and keeping the horizontal
    * component unchanged.
    */
  def flipAboutHorizontal: DiscreteInterval3D[T1, T3, T2] = horizontal x depth x vertical

  /**
    * Flips this interval by swapping the depth and horizontal components with one another and keeping the vertical
    * component unchanged.
    */
  def flipAboutVertical: DiscreteInterval3D[T3, T2, T1] = depth x vertical x horizontal

  // equivalent symbolic method names

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
  infix def ->[V](value: V): ValidData3D[V, T1, T2, T3] = withValue(value)

  /**
    * Same as [[intersectionWith]].
    *
    * Finds the intersection between this and that.
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and none otherwise.
    */
  def ∩(that: DiscreteInterval3D[T1, T2, T3]): Option[DiscreteInterval3D[T1, T2, T3]] = this intersectionWith that

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
    that: DiscreteInterval3D[T1, T2, T3]
  ): (Remainder[DiscreteInterval1D[T1]], Remainder[DiscreteInterval1D[T2]], Remainder[DiscreteInterval1D[T3]]) =
    this excluding that

  /**
    * Same as [[joinedWith]].
    *
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gaps that may
    * exist between them. So it is a proper union in the cases where this and that are adjacent, and a bit more than
    * that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  def ∪(that: DiscreteInterval3D[T1, T2, T3]): DiscreteInterval3D[T1, T2, T3] = this joinedWith that

  /**
    * Same as [[isSubsetOf]].
    *
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  def ⊆(that: DiscreteInterval3D[T1, T2, T3]): Boolean = this isSubsetOf that

/**
  * Companion for the three-dimensional interval used in defining and operating on a valid data.
  */
object DiscreteInterval3D:

  /**
    * Returns an interval unbounded in both dimensions.
    */
  def unbounded[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue]: DiscreteInterval3D[T1, T2, T3] =
    DiscreteInterval1D.unbounded[T1] x DiscreteInterval1D.unbounded[T2] x DiscreteInterval1D.unbounded[T3]

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
    s: DiscreteDomain3D[T1, T2, T3]
  ): DiscreteInterval3D[T1, T2, T3] = apply(
    DiscreteInterval1D.intervalAt(s.horizontalIndex),
    DiscreteInterval1D.intervalAt(s.verticalIndex),
    DiscreteInterval1D.intervalAt(s.depthIndex)
  )

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
    * @tparam T3
    *   a discrete value type for this interval's depth domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
    intervals: Iterable[DiscreteInterval3D[T1, T2, T3]]
  ): Boolean =
    // evaluates every pair of 2D intervals twice, so we only need to check for left/lower adjacency
    intervals.exists: r =>
      intervals
        .filter: d =>
          !(d equiv r)
        .exists: d =>
          (d intersects r) || (d isLeftAdjacentTo r) || (d isLowerAdjacentTo r) || (d isBackAdjacentTo r)

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
  def isDisjoint[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
    intervals: Iterable[DiscreteInterval3D[T1, T2, T3]]
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
    * @tparam T3
    *   a discrete value type for this interval's depth domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
    intervals: Iterable[DiscreteInterval3D[T1, T2, T3]]
  ): Iterable[DiscreteInterval3D[T1, T2, T3]] =
    for
      horizontal <- DiscreteInterval1D.uniqueIntervals(intervals.map(_.horizontal))
      vertical <- DiscreteInterval1D.uniqueIntervals(intervals.map(_.vertical))
      depth <- DiscreteInterval1D.uniqueIntervals(intervals.map(_.depth))
    yield horizontal x vertical x depth
