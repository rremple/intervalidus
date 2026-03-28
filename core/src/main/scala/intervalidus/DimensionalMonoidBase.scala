package intervalidus

/**
  * Constructs dimensional data where values can be combined as monoids.
  *
  * @define dataValueType
  *   the type of the value managed as data -- must be a Monoid (can be combined and has an identity).
  */
trait DimensionalMonoidBaseObject extends DimensionalBaseConstructorParams:

  /**
    * The empty set.
    *
    * @tparam D
    *   $intervalDomainType
    */
  def empty[V: Monoid, D <: NonEmptyTuple: DomainLike]: DimensionalMonoidBase[V, D]

  /**
    * Same as [[empty]]
    */
  def ∅[V: Monoid, D <: NonEmptyTuple: DomainLike]: DimensionalMonoidBase[V, D]

  /**
    * The universal set.
    *
    * @tparam D
    *   $intervalDomainType
    */
  def universe[V, D <: NonEmptyTuple: DomainLike](using m: Monoid[V]): DimensionalMonoidBase[V, D]

  /**
    * Same as [[universe]]
    */
  def ξ[V: Monoid, D <: NonEmptyTuple: DomainLike]: DimensionalMonoidBase[V, D]

  /**
    * Shorthand constructor for a single initial monoid value that is valid in the full interval domain.
    *
    * @param value
    *   monoid value that is valid in the full domain (`Interval.unbounded[D]`).
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalMonoidBase]] structure with a single valid value.
    */
  def of[V: Monoid, D <: NonEmptyTuple: DomainLike](value: V): DimensionalMonoidBase[V, D]

  /**
    * Shorthand constructor for a single initial monoid value that is valid in a particular interval.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param data
    *   value valid within an interval.
    * @return
    *   [[DimensionalMonoidBase]] structure with a single valid value.
    */
  def of[V: Monoid, D <: NonEmptyTuple: DomainLike](data: ValidData[V, D]): DimensionalMonoidBase[V, D]

  /**
    * Constructor for multiple (or no) initial monoid values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid monoid values within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalMonoidBase]] structure with zero or more valid values.
    */
  def apply[V: Monoid, D <: NonEmptyTuple: DomainLike](initialData: Iterable[ValidData[V, D]])(using
    Experimental
  ): DimensionalMonoidBase[V, D]

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): scala.collection.mutable.Builder[ValidData[V, D], DimensionalMonoidBase[V, D]]

/**
  * Base for all dimensional data, both mutable and immutable, where values can be combined as monoids.
  *
  * @tparam V
  *   the type of the value managed as data -- must be a Monoid (can be combined and has an identity).
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define unionDesc
  *   The union of this and that, applying the monoid combine operator in the intersection. See
  *   [[https://en.wikipedia.org/wiki/Union_(set_theory)]].
  * @define unionParmThat
  *   the shape to unite.
  * @define intersectionDesc
  *   The intersection of this and that applying the monoid combine operator. See
  *   [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
  * @define intersectionParamThat
  *   shape to intersect.
  * @define complementDesc
  *   The complement of this shape. That is, a shape with all intervals components in the universe that are not in the
  *   domain of this. All intervals in the complement take the monoid identity value, equivalent to ξ \ this. See
  *   [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
  */
trait DimensionalMonoidBase[V: Monoid, D <: NonEmptyTuple: DomainLike](using
  Experimental
) extends DimensionalBase[V, D]:

  // These overrides aren't strictly necessary, but they prevent this trait from getting optimized away
  // and losing the scaladoc definitions for subclasses.

  override def toMutable: intervalidus.mutable.DataMonoid[V, D]

  override def toImmutable: intervalidus.immutable.DataMonoid[V, D]
