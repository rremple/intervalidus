package intervalidus

import intervalidus.collection.Box

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * An interval over a contiguous set of ordered elements of a domain.
  *
  * @tparam D
  *   the type of domain used in the interval (e.g., Domain1D[Int]).
  * @tparam Self
  *   F-bounded self type.
  */
trait IntervalLike[D: DomainLike: Ordering, Self <: IntervalLike[D, Self]]:
  this: Self =>

  type ExclusionRemainder

  /**
    * The "infimum", i.e., the left/lower/back boundary of the interval (inclusive). When stored in a collection, this
    * aspect of the interval can be used as the key. (E.g., the start of a 1D interval, the lower/left corner of a 2D
    * interval).
    */
  def start: D

  /**
    * The "supremum", i.e., the right/upper/front boundary of the interval (inclusive). Must be greater than or equal to
    * the start.
    */
  def end: D

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String

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
  infix def withValue[V](value: V): ValidDataLike[V, D, Self, ?]

  /**
    * Tests if this interval contains a specific element of the domain.
    *
    * @param domainIndex
    *   domain element to test.
    * @return
    *   true if the domain element is contained in this interval.
    */
  infix def contains(domainIndex: D): Boolean

  /**
    * Returns individual discrete domain points in this interval. (Empty if domain values are continuous.)
    */
  def points: Iterable[D]

  /**
    * Tests if there is no gap between this and that.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isAdjacentTo(that: Self): Boolean

  /**
    * Tests if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   the interval to test.
    */
  infix def intersects(that: Self): Boolean

  /**
    * Finds the intersection between this and that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
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
    * Tests if that is a subset (proper or improper) of this.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if that is a subset of this.
    */
  infix def contains(that: Self): Boolean

  /**
    * If there are intervals after each interval component (i.e., none of them end at Top), returns the interval after
    * this one in all dimensions, otherwise returns None.
    */
  def after: Option[Self]

  /**
    * If there are intervals before each interval component (i.e., none of them start at Bottom), returns the interval
    * before this one in all dimensions, otherwise returns None.
    */
  def before: Option[Self]

  /**
    * Returns a new interval starting at the provided value.
    *
    * @param newStart
    *   the start of the new interval
    */
  def from(newStart: D): Self

  /**
    * Returns a new interval starting adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new start of the new interval should be adjacent
    */
  def fromAfter(adjacentDomain: D): Self = from(adjacentDomain.rightAdjacent)

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
  def to(newEnd: D): Self

  /**
    * Returns a new interval ending adjacent to the provided value.
    *
    * @param adjacentDomain
    *   the domain with which the new end of the new interval should be adjacent
    */
  def toBefore(adjacentDomain: D): Self = to(adjacentDomain.leftAdjacent)

  /**
    * Returns a new interval with the same start as this interval but with an unbounded end.
    */
  def toTop: Self

  /**
    * Tests if this interval is to the left of that interval and there is no gap between them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLeftAdjacentTo(that: Self): Boolean

  /**
    * Excludes that interval from this one. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder in each dimension after exclusion.
    */
  infix def excluding(that: Self): ExclusionRemainder

  /**
    * Returns gap between this and that interval, if one exists.
    *
    * @param that
    *   the interval to evaluate.
    * @return
    *   if one exists, some interval representing the gap between this and that in all dimensions, and None otherwise.
    */
  infix def gapWith(that: Self): Option[Self]

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
  infix def ->[V](value: V): ValidDataLike[V, D, Self, ?]

  /**
    * Approximate this interval as a box in double space based on the domain ordered hash.
    *
    * @return
    *   a new box that can be managed in a box search tree
    */
  def asBox: Box = Box(start.asCoordinate, end.asCoordinate)

  /**
    * Tests if there is no fixed start or end - spans the entire domain.
    */
  def isUnbounded: Boolean = start.isUnbounded && end.isUnbounded

  /**
    * Tests if this and that have the same start.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameStartAs(that: Self): Boolean = start equiv that.start

  /**
    * Tests if this and that have the same end.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameEndAs(that: Self): Boolean = end equiv that.end

  /**
    * Test for equivalence by comparing all interval components of this and that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this and that have the same start and end.
    */
  infix def equiv(that: Self): Boolean = (start equiv that.start) && (end equiv that.end)

  /**
    * Tests if this is a subset (proper or improper) of that. See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: Self): Boolean = that contains this

  /**
    * Tests if this interval is to the right of that interval and there is no gap between them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isRightAdjacentTo(that: Self): Boolean = that isLeftAdjacentTo this

  /**
    * Returns a new singleton interval containing only the end of this interval.
    */
  def atEnd: Self = from(end)

  /**
    * Returns a new singleton interval containing only the start of this interval.
    */
  def atStart: Self = to(start)

  // equivalent symbolic method names and other implementations

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
  def ∩(that: Self): Option[Self] = this intersectionWith that

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
  def ∪(that: Self): Self = this joinedWith that

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
  def \(that: Self): ExclusionRemainder = this excluding that

object IntervalLike:
  /**
    * Intervals are ordered by start
    */
  given [
    D: DomainLike,
    I <: IntervalLike[D, I]
  ](using domainOrder: Ordering[D]): Ordering[I] with
    override def compare(x: I, y: I): Int = domainOrder.compare(x.start, y.start)
