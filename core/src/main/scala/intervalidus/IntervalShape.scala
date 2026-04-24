package intervalidus

import intervalidus.immutable.Data

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Constructs multidimensional, multi-interval shapes.
  *
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
object IntervalShape:
  type In1D[R1] = IntervalShape[Domain.In1D[R1]]
  type In2D[R1, R2] = IntervalShape[Domain.In2D[R1, R2]]
  type In3D[R1, R2, R3] = IntervalShape[Domain.In3D[R1, R2, R3]]
  type In4D[R1, R2, R3, R4] = IntervalShape[Domain.In4D[R1, R2, R3, R4]]

  /**
    * The empty set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def empty[D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): IntervalShape[D] =
    new IntervalShape[D](Data.empty)

  /**
    * Same as [[empty]]
    *
    * The empty set.
    *
    * @tparam D
    *   $intervalDomainType
    */
  def ∅[D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): IntervalShape[D] = empty

  /**
    * The universal set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def universe[D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): IntervalShape[D] =
    of(Interval.unbounded[D])

  /**
    * Same as [[universe]]
    *
    * The universal set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def ξ[D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): IntervalShape[D] = universe

  /**
    * Constructor for a one initial interval.
    *
    * @param initialInterval
    *   a single interval.
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[IntervalShape]] shape with the initial interval present.
    */
  def of[D <: NonEmptyTuple: DomainLike](
    initialInterval: Interval[D]
  )(using config: CoreConfig[D]): IntervalShape[D] = withoutChecks(Iterable.single(initialInterval))

  /**
    * Constructor for multiple (or no) initial intervals.
    *
    * @param initialIntervals
    *   collection of intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[IntervalShape]] shape with initial intervals present.
    */
  def apply[D <: NonEmptyTuple: DomainLike](
    initialIntervals: Iterable[Interval[D]]
  )(using config: CoreConfig[D]): IntervalShape[D] =
    require(Interval.isDisjoint(initialIntervals), s"IntervalShape based on $initialIntervals is invalid: not disjoint")
    new IntervalShape(Data(initialIntervals.map(valid)).recompressAll())

  /**
    * Constructor for multiple (or no) initial intervals without checking if they are disjoint and compressed.
    *
    * @param initialIntervals
    *   collection of intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[IntervalShape]] shape with initial intervals present.
    */
  def withoutChecks[D <: NonEmptyTuple: DomainLike](
    initialIntervals: Iterable[Interval[D]]
  )(using config: CoreConfig[D]): IntervalShape[D] =
    new IntervalShape(Data(initialIntervals.map(valid)))

  /**
    * Get a Builder based on an intermediate buffer of intervals.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[D <: NonEmptyTuple: DomainLike](using
    config: CoreConfig[D]
  ): mutable.Builder[Interval[D], IntervalShape[D]] = new mutable.Builder:
    private val iterableBuilder: mutable.Builder[Interval[D], Iterable[Interval[D]]] = Iterable.newBuilder

    override def clear(): Unit = iterableBuilder.clear()

    override def addOne(elem: Interval[D]): this.type =
      iterableBuilder.addOne(elem)
      this

    override def result(): IntervalShape[D] = iterableBuilder.result().toShape

  private def valid[D <: NonEmptyTuple: DomainLike](interval: Interval[D]): ValidData[Unit, D] = interval -> ()

  /**
    * So a single interval can be used when the general notion of a multi-interval shape is needed.
    * @param config
    *   $configParam
    */
  given [D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Conversion[Interval[D], IntervalShape[D]] = of(_)

  extension [D <: NonEmptyTuple: DomainLike](is: Iterable[Interval[D]])
    /**
      * Convert a collection of intervals into an interval-based shape.
      * @param config
      *   $configParam
      */
    def toShape(using config: CoreConfig[D]): IntervalShape[D] = apply(is)

/**
  * A mutidimensional shape over some domain, represented by a collection of disjoint interval components. While an
  * [[Interval]] represents a single hyper-rectangle, an [[IntervalShape]] represents a complex, potentially disjoint
  * region of space, behaving like a first-class geometric entity. (This is structurally similar to, and supported
  * internally by, an [[immutable.Data]] structure with a single unit value.)
  *
  * [[IntervalShape]] supports powerful operators with semantics rooted in set theory:
  *   - [[union]] (or [[∪]]) combines two shapes into one
  *   - [[intersection]] (or [[∩]]) finds the overlapping region of two shapes
  *   - [[complement]] (or [[c]]) is the inverse of a shape with respect to the universe
  *   - [[difference]] (or [[\]]) chisels one shape out of another
  *   - [[symmetricDifference]] (or [[△]]) finds the disjunctive union of two shapes (a.k.a., the exclusive or)
  *   - [[isEquivalentTo]] (or [[≡]]) establishes if two shapes are logically identical
  *   - [[isSubsetOf]] (or [[⊆]]) establishes if one shape is contained in another
  *
  * This class is verified against the Laws of Set Theory. See the `laws` project in the source for the full test suite.
  * The following summarizes the key laws that are proven there:
  *   1. Associative -- the grouping of operations does not matter:
  *      - `(a ∪ (b ∪ c)) ≡ ((a ∪ b) ∪ c)`
  *      - `(a ∩ (b ∩ c)) ≡ ((a ∩ b) ∩ c)`
  *      - `(a △ (b △ c)) ≡ ((a △ b) △ c)`
  *   1. Distributive -- intersections distribute over unions and vice versa:
  *      - `(a ∪ (b ∩ c)) ≡ ((a ∪ b) ∩ (a ∪ c))`
  *      - `(a ∩ (b ∪ c)) ≡ ((a ∩ b) ∪ (a ∩ c))`
  *      - `(a ∩ (b △ c)) ≡ ((a ∩ b) △ (a ∩ c))`
  *   1. Commutative -- the order of operations does not matter:
  *      - `(a ∪ b) ≡ (b ∪ a)`
  *      - `(a ∩ b) ≡ (b ∩ a)`
  *      - `(a △ b) ≡ (b △ a)`
  *   1. De Morgan’s -- relates the complement of unions and intersections:
  *      - `(a ∪ b).c ≡ (a.c ∩ b.c)`
  *      - `(a ∩ b).c ≡ (a.c ∪ b.c)`
  *   1. Absorption -- simplifies specific combinations:
  *      - `(a ∪ (a ∩ b)) ≡ a`
  *      - `(a ∩ (a ∪ b)) ≡ a`
  *   1. Idempotent -- a set combined with itself is the set itself:
  *      - `(a ∪ a) ≡ a`
  *      - `(a ∩ a) ≡ a`
  *   1. Nilpotent -- a set combined with itself is ∅ (self-inverse):
  *      - `(a △ a) ≡≡ ∅`
  *      - `(a \ a) ≡≡ ∅`
  *   1. Identity -- operations where ξ and ∅ are the identity:
  *      - `(a ∪ ∅) ≡ a`
  *      - `(a ∩ ξ) ≡ a`
  *      - `(a △ ∅) ≡ a`
  *      - `(a \ ∅) ≡ a`
  *   1. Complement -- operations with a set's complement:
  *      - `(a ∪ a.c) ≡ ξ`
  *      - `(a ∩ a.c) ≡ ∅`
  *      - `(a △ a.c) ≡ ξ`
  *      - `a.c.c ≡ a` (involution)
  *   1. Domination -- operations where ξ and ∅ dominate:
  *      - `(a ∪ ξ) ≡ ξ`
  *      - `(a ∩ ∅) ≡ ∅`
  *      - `(∅ \ a) ≡ ∅`
  *   1. Consistency Laws -- equivalent definitions based on set algebra:
  *      - `a.c ≡ (ξ \ a)`
  *      - `(b \ a.c) ≡ (a \ b.c)`
  *      - `((a \ b) ∪ (b \ a)) ≡ ((a ∪ b) \ (a ∩ b))`
  *      - `(a ∪ b) ≡ ((a △ b) ∪ (a ∩ b))`
  *      - `(a ∩ b) ⊆ a`
  *      - `(a ∩ b) ⊆ b`
  *      - `a ⊆ (a ∪ b)`
  *      - `b ⊆ (a ∪ b)`
  *      - `(a \ b) ⊆ a`
  *      - `(b \ a) ⊆ b`
  *      - `a ⊆ ξ`
  *      - `∅ ⊆ a`
  *
  * @param config
  *   context parameter for configuration -- uses defaults if not given explicitly
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  */
class IntervalShape[D <: NonEmptyTuple: DomainLike] private (
  val underlying: Data[Unit, D]
)(using config: CoreConfig[D]):

  import IntervalShape.{valid, ξ, ∅}

  /**
    * Construct a new instance with an updated underlying structure
    */
  private def withUnderlying(f: Data[Unit, D] => Data[Unit, D]) = new IntervalShape(f(underlying))

  // ---------- Query API methods: Retrieval and Inspection ----------
  // ---- Evaluates equality/equivalence, inspects/queries/extracts shape data, and represents shapes as strings. ----

  override def equals(obj: Any): Boolean = obj match
    case that: IntervalShape[?] => underlying.equals(that.underlying)
    case _                      => false

  /**
    * Is that shape "logically equivalent to" this one? That is, either this and that are equal, or they are equal after
    * being decompressed using the same base intervals (the same decompression used in [[recompressAll]]).
    *
    * @param that
    *   shape to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def isEquivalentTo(that: IntervalShape[D]): Boolean = underlying isEquivalentTo that.underlying

  /**
    * Same as [[isEquivalentTo]]
    *
    * Is that shape "logically equivalent to" this one? That is, either this and that are equal, or they are equal after
    * being decompressed using the same base intervals (the same decompression used in [[recompressAll]]).
    *
    * @param that
    *   shape to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def ≡(that: IntervalShape[D]): Boolean = isEquivalentTo(that)

  private def toStringTransform(f: Interval[D] => String): String =
    allIntervals.toList match
      case List()                   => "∅"
      case List(d) if d.isUnbounded => "ξ"
      case multiple                 => multiple.map(f).mkString("IntervalShape(", ", ", ")")

  override def toString: String = toStringTransform(_.toString)

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String = toStringTransform(_.toCodeLikeString)

  /**
    * Are there no intervals in this shape? I.e., this ≡ ∅?
    *
    * @return
    *   true if there are no intervals, false otherwise.
    */
  def isEmpty: Boolean = underlying.isEmpty

  /**
    * Is this shape the universal shape? I.e., this ≡ ξ?
    *
    * @return
    *   true if this shape is equivalent to ξ, false otherwise.
    */
  def isUniverse: Boolean = isEquivalentTo(ξ)

  /**
    * The number of interval components representing this shape.
    */
  def size: Int = underlying.size

  /**
    * Is the domain element part of this shape?
    *
    * @param domainElement
    *   can be a specific data point or the special notions of "bottom" or "top" of the domain.
    * @return
    *   true if this shape contains the provided domain element, and false otherwise.
    */
  infix def contains(domainElement: D): Boolean = underlying.isDefinedAt(domainElement)

  /**
    * Is the interval part of this shape?
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this shape completely contains the provided interval, and false otherwise.
    */
  infix def contains(interval: Interval[D]): Boolean = interval ⊆ this // Interval lifted into an IntervalShape

  /**
    * Does this contain some or all of the elements of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are intervals that are present somewhere on the interval.
    */
  infix def intersects(interval: Interval[D]): Boolean = underlying.intersects(interval)

  /**
    * Is this a subset (proper or improper) of that? See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   shape to test
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: IntervalShape[D]): Boolean = underlying isSubsetOf that.underlying

  /**
    * Same as [[isSubsetOf]]
    *
    * Is this a subset (proper or improper) of that? See [[https://en.wikipedia.org/wiki/Subset]].
    *
    * @param that
    *   shape to test
    * @return
    *   true if this is a subset of that.
    */
  infix def ⊆(that: IntervalShape[D]): Boolean = isSubsetOf(that)

  /**
    * Returns all the intervals representing this shape.
    */
  def allIntervals: Iterable[Interval[D]] = underlying.allIntervals

  // ---------- Algebra API methods: Set Algebra ----------
  // ---- Standard set-theoretic operations for multidimensional shapes. ----

  /**
    * The complement of this shape. That is, a shape with all intervals components in the universe and not in this
    * shape's [[allIntervals]]. See [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    *
    * @return
    *   a new shape that is the complement of this.
    */
  def complement: IntervalShape[D] = IntervalShape.withoutChecks(underlying.domainComplement)

  /**
    * Same as [[complement]].
    *
    * The complement of this shape. That is, a shape with all intervals components in the universe and not in this
    * shape's [[allIntervals]]. See [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    *
    * @return
    *   a new shape that is the complement of this.
    */
  def c: IntervalShape[D] = complement

  /**
    * The intersection of this and that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   shape to intersect.
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def intersection(that: IntervalShape[D]): IntervalShape[D] = IntervalShape.withoutChecks(
    underlying.intersectingIntervals(that.underlying)
  )

  /**
    * Same as [[intersection]].
    *
    * The intersection of this and that. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   shape to intersect.
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def ∩(that: IntervalShape[D]): IntervalShape[D] = intersection(that)

  /**
    * The union of this and that. See [[https://en.wikipedia.org/wiki/Union_(set_theory)]].
    *
    * @param that
    *   the shape to unite.
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def union(that: IntervalShape[D]): IntervalShape[D] =
    withUnderlying(_.setMany(that.underlying.getAll))

  /**
    * Same as [[union]].
    *
    * The union of this and that. See [[https://en.wikipedia.org/wiki/Union_(set_theory)]].
    *
    * @param that
    *   the shape to unite.
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def ∪(that: IntervalShape[D]): IntervalShape[D] = union(that)

  /**
    * The elements in this that are not in that. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    * @param that
    *   shape to remove.
    * @return
    *   a new shape that is the difference of this and that.
    */
  infix def difference(that: IntervalShape[D]): IntervalShape[D] = withUnderlying(_.difference(that.underlying))

  /**
    * Same as [[difference]].
    *
    * The elements in this that are not in that. See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
    * @param that
    *   shape to remove.
    * @return
    *   a new shape that is the difference of this and that.
    */
  infix def \(that: IntervalShape[D]): IntervalShape[D] = difference(that)

  /**
    * Returns the "exclusive or" of this and that. That is, the portions of the shapes that are in this or that, but not
    * in both. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
    *
    * @param that
    *   shape to combine.
    * @return
    *   a new shape with the elements that are in this and that, but not in both.
    */
  infix def symmetricDifference(that: IntervalShape[D]): IntervalShape[D] =
    withUnderlying(_.symmetricDifference(that.underlying))

  /**
    * Same as [[symmetricDifference]].
    *
    * Returns the "exclusive or" of this and that. That is, the portions of the shapes that are in this or that, but not
    * in both. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
    *
    * @param that
    *   shape to combine.
    * @return
    *   a new shape with the elements that are in this and that, but not in both.
    */
  infix def △(that: IntervalShape[D]): IntervalShape[D] = symmetricDifference(that)

  /**
    * The intersection of this and a single interval. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param interval
    *   a single interval with which to intersect.
    * @return
    *   a new shape that is the intersection of this and the interval (i.e., this is "clipped" within the interval).
    */
  infix def intersection(interval: Interval[D]): IntervalShape[D] = withUnderlying(_.intersection(interval))

  /**
    * Same as [[intersection]].
    *
    * The intersection of this and a single interval. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param interval
    *   a single interval to intersect.
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def ∩(interval: Interval[D]): IntervalShape[D] = intersection(interval)

  // ---------- Manipulation API methods: Set Manipulation ----------
  // ---- Adds to and removes from shapes. ----

  /**
    * Adds a single interval.
    *
    * @param interval
    *   a single interval to add.
    * @return
    *   a new shape with the interval added.
    */
  infix def add(interval: Interval[D]): IntervalShape[D] = withUnderlying(_.set(valid(interval)))

  /**
    * Same as [[add]].
    *
    * Adds a single interval.
    *
    * @param interval
    *   a single interval to add.
    * @return
    *   a new shape with the interval added.
    */
  infix def +(interval: Interval[D]): IntervalShape[D] = add(interval)

  /**
    * Adds multiple intervals.
    *
    * @param intervals
    *   multiple intervals to add.
    * @return
    *   a new shape with the intervals added.
    */
  infix def addMany(intervals: IterableOnce[Interval[D]]): IntervalShape[D] =
    withUnderlying(_.setMany(intervals.iterator.map(valid)))

  /**
    * Same as [[addMany]].
    *
    * Adds multiple intervals.
    *
    * @param intervals
    *   multiple intervals to add.
    * @return
    *   a new shape with the intervals added.
    */
  infix def ++(intervals: IterableOnce[Interval[D]]): IntervalShape[D] = addMany(intervals)

  /**
    * Adds a single interval if it does not intersect with any existing interval components of this.
    *
    * @param interval
    *   a single interval to add.
    * @return
    *   if there is no conflict, some new shape with the interval added, otherwise None.
    */
  infix def addIfNoConflict(interval: Interval[D]): Option[IntervalShape[D]] =
    underlying.setIfNoConflict(valid(interval)).map(new IntervalShape(_))

  /**
    * Removes a single interval.
    *
    * @param interval
    *   a single interval to remove.
    * @return
    *   a new shape with the interval removed.
    */
  infix def remove(interval: Interval[D]): IntervalShape[D] = withUnderlying(_.remove(interval))

  /**
    * Same as [[remove]].
    *
    * Removes a single interval.
    *
    * @param interval
    *   a single interval to remove.
    * @return
    *   a new shape with the interval removed.
    */
  infix def -(interval: Interval[D]): IntervalShape[D] = remove(interval)

  /**
    * Removes a multiple intervals.
    *
    * @param intervals
    *   multiple interval to remove.
    * @return
    *   a new shape with the intervals removed.
    */
  infix def removeMany(intervals: IterableOnce[Interval[D]]): IntervalShape[D] = withUnderlying(_.removeMany(intervals))

  /**
    * Same as [[removeMany]].
    *
    * Removes a multiple intervals.
    *
    * @param intervals
    *   multiple interval to remove.
    * @return
    *   a new shape with the intervals removed.
    */
  infix def --(intervals: IterableOnce[Interval[D]]): IntervalShape[D] = removeMany(intervals)

  // ---------- High-order API methods: Monadic and Other High-Order ----------
  // ---- High-order functions, including monadic functions, for processing shapes. ----

  /**
    * Applies a function to all interval components. The domain type can be changed in the mapping.
    *
    * @param f
    *   the function to apply to each interval component.
    * @tparam S
    *   the domain type of the returned shape.
    * @return
    *   a new shape resulting from applying the provided function to each interval component of this shape.
    */
  def map[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): IntervalShape[S] = IntervalShape(allIntervals.map(f))(using config = altConfig)

  /**
    * Builds a new shape by applying a function to all the interval components of this shape and concatenating the
    * resulting shapes into a new shape. The domain type can be changed in the mapping.
    *
    * @param f
    *   the function to apply to each interval component which results in a new shape.
    * @tparam S
    *   the domain type of the returned shape.
    * @return
    *   a new shape resulting from concatenating the results of function f applied to each interval component.
    */
  def flatMap[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => IntervalShape[S]
  )(using altConfig: CoreConfig[S]): IntervalShape[S] =
    IntervalShape(allIntervals.flatMap(f(_).allIntervals))(using config = altConfig)

  /**
    * Builds a new shape with all the interval components of this shape that satisfy a predicate.
    *
    * @param p
    *   the predicate used to test interval components.
    * @return
    *   a new shape consisting of all the interval components that satisfy the provided predicate p.
    */
  def filter(p: Interval[D] => Boolean): IntervalShape[D] =
    IntervalShape.withoutChecks(allIntervals.filter(p))

  /**
    * Builds a new shape by applying a partial function to all interval components of this shape. The domain type can be
    * changed in the mapping.
    *
    * @param pf
    *   the partial function to apply to each interval component
    * @tparam S
    *   the domain type of the returned shape.
    * @return
    *   a new shape resulting from applying the provided function to each interval component of this shape.
    */
  def collect[S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[Interval[D], Interval[S]]
  )(using altConfig: CoreConfig[S]): IntervalShape[S] =
    IntervalShape(allIntervals.collect(pf))(using config = altConfig)

  /**
    * Applies a binary operator to a start value and all interval components, going left to right.
    *
    * @param z
    *   the start value.
    * @param op
    *   the binary operator.
    * @tparam B
    *   the result type of the binary operator.
    * @return
    *   the result of inserting op between consecutive interval components, going left to right with the start value z
    *   on the left. Returns z if this is ∅.
    */
  def foldLeft[B](z: B)(op: (B, Interval[D]) => B): B = allIntervals.foldLeft(z)(op)

  // ---------- Physical API methods: Physical Manipulation ----------
  // ---- Methods that slice, copy, recompress, and hydrate shapes. ----

  /**
    * Project as a shape in n-1 dimensions based on a lookup in the head dimension.
    *
    * (Equivalent to `getByDimension[H, Domain.NonEmptyTail[D]](0, domain)`, though the type checking is simpler)
    *
    * @tparam H
    *   the domain value type of the 1D domain used for filtering. There are type safety checks that ensure
    *   - the head 1D domain has the specified domain value type
    *   - the current domain tail is a non-empty domain (i.e., the current domain type `D` has at least two dimensions)
    *   - the current domain type can be constructed by concatenating the 1D domain type specified and the current
    *     domain tail.
    * @param domain
    *   the head dimension domain element
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using altConfig: CoreConfig[Domain.NonEmptyTail[D]]): IntervalShape[Domain.NonEmptyTail[D]] =
    new IntervalShape(underlying.getByHeadDimension(domain))(using config = altConfig)

  /**
    * Project as a shape in n-1 dimensions based on a lookup in the specified dimension.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    * @param domain
    *   the domain element used for filtering
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    * @tparam R
    *   domain of intervals in the returned shape. There is a type safety check that ensures the domain type for this
    *   result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): IntervalShape[R] =
    new IntervalShape(underlying.getByDimension(dimensionIndex, domain))(using config = altConfig)

  /**
    * Unlike in 1D, there is no unique compression of interval components in higher dimensions. For example, discrete
    * interval components {[1..5], [1..2]} + {[1..2], [3..4]} could also be represented physically as {[1..2], [1..4]} +
    * {[3..5], [1..2]}. (These representations are equivalent but not equal.)
    *
    * First, this method decompresses this shape as a unique arrangement of "atomic" interval components. In the above
    * example, that would be the following: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
    * recompresses the intervals, which results in a unique physical representation. It may be useful when comparing two
    * shapes to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    *
    * @param otherShape
    *   other intervals to be considered when decompressing the space. This is useful in testing equivalence of two
    *   shapes where their starting interval components differ enough that they result in a different enough
    *   decompression that it results in different recompressions.
    * @return
    *   a new, equivalent shape with a recompressed representation
    */
  def recompressAll(otherShape: IntervalShape[D] = ∅): IntervalShape[D] =
    withUnderlying(_.recompressAll(otherShape.allIntervals))

  /**
    * Creates a copy.
    *
    * @return
    *   a new shape with the same interval components.
    */
  def copy: IntervalShape[D] = withUnderlying(_.copy)

  /**
    * Hydrate this shape with a constant value.
    * @param constantValue
    *   constant value make valid in every interval component.
    * @tparam V
    *   value type
    * @return
    *   an immutable data structure where the constant value is valid in the shape. All interval components are
    *   included.
    */
  def withValue[V](constantValue: V): immutable.Data[V, D] = underlying.mapValues(_ => constantValue)

  /**
    * Same as [[withValue]]
    *
    * Hydrate this shape with a constant value.
    *
    * @param constantValue
    *   constant value make valid in every interval component.
    * @tparam V
    *   value type
    * @return
    *   an immutable data structure where the constant value is valid in the shape. All interval components are
    *   included.
    */
  infix def ->[V](constantValue: V): immutable.Data[V, D] = withValue(constantValue)

  /**
    * Hydrate this shape with a constant monoid value.
    * @param constantValue
    *   constant monoid value make valid in every interval component.
    * @tparam V
    *   value type, must be a Monoid
    * @return
    *   an immutable data monoid structure where the constant value is valid in the shape. All interval components are
    *   included.
    */
  def withMonoidValue[V: Monoid](constantValue: V): immutable.DataMonoid[V, D] =
    immutable.DataMonoid(allIntervals.map(_ -> constantValue))

  /**
    * Same as [[withMonoidValue]]
    *
    * Hydrate this shape with a constant monoid value.
    * @param constantValue
    *   constant monoid value make valid in every interval component.
    * @tparam V
    *   value type, must be a Monoid
    * @return
    *   an immutable data monoid structure where the constant value is valid in the shape. All interval components are
    *   included.
    */
  def +>[V: Monoid](constantValue: V): immutable.DataMonoid[V, D] = withMonoidValue(constantValue)

  /**
    * Hydrate this shape with a value based on some or all interval components. Note that the mapping deals with the
    * underlying physical structure. Because `IntervalShape` may subdivide space to maintain its internal structure, a
    * partial function should generally avoid matching on exact interval boundaries unless they've been aligned to
    * something specific.
    * @param mapping
    *   a partial function of physical interval components to values.
    * @tparam V
    *   value type
    * @return
    *   an immutable data structure where the mapped value is valid in the shape. Interval components not mapped will
    *   not be included.
    */
  def collectWithValue[V](
    mapping: PartialFunction[Interval[D], V]
  ): immutable.Data[V, D] = underlying.collect:
    case t if mapping.isDefinedAt(t.interval) => t.copy(value = mapping(t.interval))

  /**
    * Hydrate this shape with a value based on some or all interval components.
    *
    * @param alignTo
    *   decompresses the underlying structure to align to a known set of intervals. Without this,
    *   the mapping deals with the underlying physical structure. Because `IntervalShape`
    *   may subdivide space to maintain its internal structure, a partial function should generally avoid matching on
    *   exact interval boundaries unless they've been aligned to something specific. Best Practice: since this relies on
    *   decompression, the resulting intervals may be smaller than the alignTo intervals specify (e.g., this shape only
    *   covers part of an interval), but they should never be larger, so functions like `intersect` and `isSubsetOf` are
    *   better suited when matching the interval component than doing an exact match. Example:
    *   {{{
    *     case i if i ⊆ Morning => "AM"
    *   }}} is safer than
    *   {{{
    *     case Morning => "AM"
    *   }}}
    * @param mapping
    *   a partial function of aligned interval components to values.
    * @tparam V
    *   value type
    * @return
    *   an immutable data structure where the mapped value is valid in the shape. Interval components not mapped will
    *   not be included.
    */
  def collectWithValue[V](alignTo: IterableOnce[Interval[D]])(
    mapping: PartialFunction[Interval[D], V]
  ): immutable.Data[V, D] = immutable.Data[V, D](
    underlying
      .decompressedData(alignTo)
      .collect:
        case t if mapping.isDefinedAt(t.interval) => t.copy(value = mapping(t.interval))
  )
