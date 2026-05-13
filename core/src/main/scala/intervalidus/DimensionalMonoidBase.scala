package intervalidus

/**
  * Constructs dimensional data where values can be combined as monoids.
  *
  * @tparam Constructed
  *   Constructed type.
  * @define dataValueType
  *   the type of the value managed as data -- must be a Monoid (can be combined and has an identity).
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
trait DimensionalMonoidBaseObject[Constructed[_, _ <: NonEmptyTuple] <: DimensionalMonoidBase[?, ?]]:

  // ---------- Abstract ----------

  /**
    * Constructor for multiple initial monoid values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid monoid values within intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with zero or more valid values.
    */
  def apply[V: Monoid, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using config: CoreConfig[D]): Constructed[V, D]

  // ---------- Concrete ----------

  /**
    * Constructor where no values are valid. The empty set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def empty[V: Monoid, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] =
    apply(Iterable.empty)

  /**
    * Same as [[empty]]
    *
    * Constructor where no values are valid. The empty set.
    *
    * @param config
    *   $configParam
    */
  def ∅[V: Monoid, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] = empty

  /**
    * The universal set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def universe[V, D <: NonEmptyTuple: DomainLike](using
    config: CoreConfig[D],
    monoid: Monoid[V]
  ): Constructed[V, D] = of(monoid.identity)

  /**
    * Same as [[universe]]
    *
    * The universal set.
    * @param config
    *   $configParam
    */
  def ξ[V: Monoid, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] = universe

  /**
    * Shorthand constructor for a single initial monoid value that is valid in the full interval domain.
    *
    * @param value
    *   monoid value that is valid in the full domain (`Interval.unbounded[D]`).
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with a single valid value.
    */
  def of[V: Monoid, D <: NonEmptyTuple: DomainLike](value: V)(using config: CoreConfig[D]): Constructed[V, D] =
    of(Interval.unbounded[D] -> value)

  /**
    * Shorthand constructor for a single initial monoid value that is valid in a particular interval.
    *
    * @param data
    *   value valid within an interval.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with a single valid value.
    */
  def of[V: Monoid, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using config: CoreConfig[D]): Constructed[V, D] = apply(Iterable.single(data))

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    config: CoreConfig[D]
  ): scala.collection.mutable.Builder[ValidData[V, D], Constructed[V, D]] =
    ValidData.Builds[V, D, Constructed[V, D]](apply(_))

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
trait DimensionalMonoidBase[V: Monoid, D <: NonEmptyTuple: DomainLike] extends DimensionalBase[V, D]:

  /**
    * Creates a new structure with n-1 dimensions by collapsing overlapping lower-dimensional intervals and merging
    * their values. This is an n-1 dimensional "squashing" of the original structure (e.g., squash a translucent 3d cube
    * into its 2d shadow representing how much light passes through it). Values are merged using the monoid combine
    * function.
    *
    * @param dimensionIndex
    *   dimension to drop. Must be a value with a singleton type known at compile time, e.g., a numeric literal. (The
    *   head dimension is dimension 0.)
    * @param altConfig
    *   $configParam
    * @tparam R
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a lower-dimensional (n-1) structure
    */
  def flattenDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DimensionalMonoidBase[V, R]
