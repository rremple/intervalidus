package intervalidus

/**
  * Base for a one-dimensional or multidimensional interval.
  *
  * @tparam MinorD
  *   the domain type
  * @tparam MajorD
  *   the domain type of the multidimensional image of `MinorD` -- a non-empty tuple that is DomainLike.
  * @tparam Remainder
  *   the type returned by [[excluding]]
  * @tparam Self
  *   F-bounded self-type
  *
  * @define intervalToTest
  *   the interval to test.
  */
trait IntervalBase[
  MinorD,
  MajorD <: NonEmptyTuple: DomainLike,
  Remainder,
  Self <: IntervalBase[MinorD, MajorD, Remainder, Self]
]:
  this: Self =>

  /**
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the type of the value managed as data.
    * @return
    *   valid data in this interval
    */
  infix def withValue[V](value: V): ValidData[V, MajorD]

  /**
    * Returns individual discrete domain points in this interval. (Empty if domain values are continuous.)
    */
  def points: Iterable[MinorD]

  /**
    * Tests if this interval is to the left of that interval and there is no gap or overlap between them.
    *
    * @param that
    *   $intervalToTest
    */
  infix def isLeftAdjacentTo(that: Self): Boolean

  /**
    * Finds the intersection of this with that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and None otherwise.
    */
  infix def intersectionWith(that: Self): Option[Self]

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
  infix def joinedWith(that: Self): Self

  /**
    * Returns the interval to the right of this one if one can be constructed (i.e., this does not end at the `Top`),
    * otherwise returns None.
    */
  def after: Option[Self]

  /**
    * Returns the interval to the left of this one if one can be constructed (i.e., this does not start at the
    * `Bottom`), otherwise returns None.
    */
  def before: Option[Self]

  /**
    * Returns a new interval starting at the provided value.
    *
    * @param newStart
    *   the start of the new interval
    */
  def from(newStart: MinorD): Self

  /**
    * Returns a new interval starting adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new start of the new interval should be adjacent
    */
  def fromAfter(adjacentDomain: MinorD): Self

  /**
    * Returns a new interval with the same end as this interval but with an unbounded start.
    */
  def fromBottom: Self

  /**
    * Returns a new interval ending at the provided value.
    *
    * @param newEnd
    *   the end of the new interval
    */
  def to(newEnd: MinorD): Self

  /**
    * Returns a new interval ending adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new end of the new interval should be adjacent
    */
  def toBefore(adjacentDomain: MinorD): Self

  /**
    * Returns a new interval with the same start as this interval but with an unbounded end.
    */
  def toTop: Self

  /**
    * Returns a new singleton interval containing only the start of this interval.
    */
  def atStart: Self

  /**
    * Returns a new singleton interval containing only the end of this interval.
    */
  def atEnd: Self

  /**
    * Excludes that interval from this one. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    *
    * There are three possible outcomes:
    *   1. this interval is a subset of that one, so once it is excluded, nothing remains. `Remainder.None` is returned.
    *   1. that either lies outside of this or has a simple edge intersection (only contains the start or the end, but
    *      not both), and a single interval remains. `Remainder.Single` is returned.
    *   1. that interval is a proper subset of this one, containing neither the start nor the end of this, so this
    *      interval gets split, and two intervals remain. `Remainder.Split` is returned
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder after excluding that.
    */
  infix def excluding(that: Self): Remainder

  /**
    * Similar to [[excluding]], separates the current interval into the intersection with that interval along with the
    * remainder intervals covering this, i.e., the union of all the returned intervals will match this. The interval
    * collection returned will be ordered by interval starts and will be disjoint. There are three possible outcomes:
    *   1. just one interval (this interval) is returned if this is a subset of that or this doesn't intersect that.
    *   1. two intervals (the intersection with that along with the portion of this lying outside of that) are returned
    *      if this has a simple edge intersection.
    *   1. three intervals (the intersection with that along with both portions of this that are before and after that)
    *      are returned if that is a proper subset of this, containing neither the start nor the end of this.
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   A collection of intervals covering this without overlaps after being separated by that.
    */
  infix def separateUsing(that: Self): Iterable[Self]

  /**
    * Returns the gap between this and that interval if one exists.
    *
    * @param that
    *   the interval to evaluate.
    * @return
    *   if one exists, some interval representing the gap between this and that in all dimensions, and None otherwise.
    */
  infix def gapWith(that: Self): Option[Self]

  /**
    * Test for equivalence by comparing the start and end of this and that.
    *
    * @param that
    *   $intervalToTest
    * @return
    *   true if this and that have the same start and end.
    */
  infix def equiv(that: Self): Boolean

  /**
    * Tests if this interval contains a specific element of the domain.
    *
    * @param domain
    *   domain element to test.
    * @return
    *   true if the domain element is contained in this interval.
    */
  infix def contains(domain: MinorD): Boolean

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String

  /*
   * Implemented based on definitions above
   */

  /**
    * Tests if there is no gap or overlap between this and that, and they could be merged to form a single interval.
    *
    * @param that
    *   $intervalToTest
    */
  infix def isAdjacentTo(that: Self): Boolean = (this isLeftAdjacentTo that) || (that isLeftAdjacentTo this)

  /**
    * Tests if this interval is to the right of that interval and there is no gap or overlap between them.
    *
    * @param that
    *   $intervalToTest
    */
  infix def isRightAdjacentTo(that: Self): Boolean = that isLeftAdjacentTo this

  /**
    * Tests if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   $intervalToTest
    */
  infix def intersects(that: Self): Boolean = (this ∩ that).isDefined

  /**
    * Tests if that is a subset (proper or improper) of this.
    *
    * @param that
    *   $intervalToTest
    * @return
    *   true if that is a subset of this.
    */
  infix def contains(that: Self): Boolean = (this ∩ that).contains(that)

  /**
    * Tests if this is a subset (proper or improper) of that. See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   $intervalToTest
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: Self): Boolean = that contains this

  /*
   * Equivalent symbolic method names
   */

  /**
    * Same as [[isAdjacentTo]]
    *
    * Tests if there is no gap or overlap between this and that, and they could be merged to form a single interval.
    *
    * @param that
    *   $intervalToTest
    */
  infix def ~(that: Self): Boolean = isAdjacentTo(that)

  /**
    * Same as [[isLeftAdjacentTo]]
    *
    * Tests if this interval is to the left of that interval and there is no gap between them.
    *
    * @param that
    *   $intervalToTest
    */
  infix def ~>(that: Self): Boolean = isLeftAdjacentTo(that)

  /**
    * Same as [[withValue]]
    *
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the type of the value managed as data.
    * @return
    *   valid data in this interval
    */
  infix def ->[V](value: V): ValidData[V, MajorD] = withValue(value)

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
  def ∩(that: Self): Option[Self] =
    intersectionWith(that)

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
  def ∪(that: Self): Self = joinedWith(that)

  /**
    * Same as [[isSubsetOf]].
    *
    * Tests if this is a subset (proper or improper) of that. See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   $intervalToTest
    * @return
    *   true if this is a subset of that.
    */
  def ⊆(that: Self): Boolean = this isSubsetOf that

  /**
    * Same as [[excluding]].
    *
    * Excludes that interval from this one. The horizontal, vertical, and depth results are returned as a tuple. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion.
    */
  def \(that: Self): Remainder = excluding(that)
