package intervalidus

/**
  * An interval over a contiguous set of ordered elements of a discrete domain.
  *
  * @tparam D
  *   the type of discrete domain used in the discrete interval (e.g., DiscreteDomain1D[Int]).
  * @tparam Self
  *   F-bounded self type.
  */
trait DiscreteIntervalLike[D <: DiscreteDomainLike[D], Self <: DiscreteIntervalLike[D, Self]]:
  this: Self =>

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
    * Tests if there is no fixed start or end - spans the entire domain.
    */
  infix def isUnbounded: Boolean

  /**
    * Returns individual discrete domain points in this interval.
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
    * Tests if this and that have the same start.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameStartAs(that: Self): Boolean

  /**
    * Tests if this and that have the same end.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameEndAs(that: Self): Boolean

  /**
    * Tests if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   the interval to test.
    */
  infix def intersects(that: Self): Boolean

  /**
    * Finds the intersection between this and that.
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and None otherwise.
    */
  infix def intersectionWith(that: Self): Option[Self]

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
  infix def joinedWith(that: Self): Self

  /**
    * Test for equivalence by comparing all interval components of this and that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this and that have the same start and end.
    */
  infix def equiv(that: Self): Boolean

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
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: Self): Boolean

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
  infix def ->[V](value: V): ValidDataLike[V, D, Self, ?]

  /**
    * Same as [[intersectionWith]].
    *
    * Finds the intersection between this and that.
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
    * exist between them. So it is a proper union in the cases where this and that are adjacent, and a bit more than
    * that otherwise.
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
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  def ⊆(that: Self): Boolean = this isSubsetOf that
