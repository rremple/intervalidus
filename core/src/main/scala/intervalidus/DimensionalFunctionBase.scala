package intervalidus

import intervalidus.immutable.Data as ImmutableData
import intervalidus.mutable.Data as MutableData

import scala.annotation.nowarn

/**
  * Common DimensionalFunction structures.
  */
object DimensionalFunctionBase:
  /**
    * The underlying value type for domain functions.
    */
  type DomainFunction[V, D <: NonEmptyTuple] = D => V

  /**
    * The underlying valid data type for domain functions.
    */
  type ValidFunction[V, D <: NonEmptyTuple] = ValidData[DomainFunction[V, D], D]

  /**
    * The underlying structure type for domain functions.
    */
  type MutableFunctionData[V, D <: NonEmptyTuple] = MutableData[DomainFunction[V, D], D]

  /**
    * The immutable version of the underlying structure type for domain functions.
    */
  type ImmutableFunctionData[V, D <: NonEmptyTuple] = ImmutableData[DomainFunction[V, D], D]

  extension [V, B, D <: NonEmptyTuple: DomainLike](lhs: DomainFunction[V, D])
    /**
      * Fuses this domain function with another on the same domain into a single domain function that results in the
      * tuple of the domain function results. Useful internally on methods like DataFunction.zip.
      */
    infix def fuse(rhs: DomainFunction[B, D]): DomainFunction[(V, B), D] =
      (d: D) => (lhs(d), rhs(d))

  extension [V, D <: NonEmptyTuple: DomainLike](lhs: DomainFunction[V, D])
    /**
      * Merges this domain function with another on the same domain into a single domain function that results in the
      * domain function results being merged. Useful internally on methods like DataFunction.merge.
      */
    def merge(rhs: DomainFunction[V, D], mergeValues: (V, V) => V): DomainFunction[V, D] =
      (d: D) => mergeValues(lhs(d), rhs(d))

import DimensionalFunctionBase.*

/**
  * Constructs dimensional data where values are domain functions.
  *
  * @tparam Constructed
  *   Constructed type.
  * @define dataValueType
  *   the result type of the domain function managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
trait DimensionalFunctionBaseObject[Constructed[_, _ <: NonEmptyTuple] <: DimensionalFunctionBase[?, ?]]:
  // ---------- Abstract ----------

  /**
    * Create a new instance from underlying data.
    */
  protected def fromData[V, D <: NonEmptyTuple: DomainLike](
    data: MutableData[DomainFunction[V, D], D]
  ): Constructed[V, D]

  // ---------- Concrete ----------

  extension [V, D <: NonEmptyTuple: DomainLike](data: DimensionalBase[DomainFunction[V, D], D])
    /**
      * Creates a functional structure from a non-functional structure with functional values.
      *
      * @return
      *   A new functional structure with the same valid functions.
      */
    def asDataFunction: Constructed[V, D] = fromData(MutableData.asData(data))

  extension [V, D <: NonEmptyTuple: DomainLike](dataFunction: DimensionalFunctionBase[V, D])
    /**
      * Creates a non-functional structure with functional values from a functional structure.
      *
      * @return
      *   A new non-functional structure with the same valid functions.
      */
    def asData: ImmutableData[DomainFunction[V, D], D] = dataFunction.underlying.toImmutable

    /**
      * Automatically converts a non-functional structure with functional values to a functional structure.
      */
  given [V, D <: NonEmptyTuple: DomainLike]: Conversion[
    DimensionalBase[DomainFunction[V, D], D],
    Constructed[V, D]
  ] = _.asDataFunction

  /**
    * Automatically converts a functional structure to a non-functional structure with functional values.
    */
  given [V, D <: NonEmptyTuple: DomainLike]: Conversion[
    DimensionalFunctionBase[V, D],
    ImmutableData[DomainFunction[V, D], D]
  ] = _.asData

  /**
    * Constructor for multiple initial function values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid function values within intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with zero or more valid functions.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidFunction[V, D]] = Iterable.empty[ValidFunction[V, D]]
  )(using config: CoreConfig[D]): Constructed[V, D] = fromData(MutableData(initialData))

  /**
    * Constructor where no domain functions are valid.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with no valid functions.
    */
  def empty[V, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] =
    apply(Iterable.empty)

    /**
      * Same as [[empty]]
      *
      * Constructor where no values are valid.
      *
      * @param config
      *   $configParam
      */
  def ∅[V, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] = empty

  /**
    * Shorthand constructor for one or more domain functions that are valid in specific intervals.
    *
    * @param data
    *   domain function that is valid within an interval.
    * @param moreData
    *   additional domain functions valid within other intervals.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with one or more valid domain functions.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidFunction[V, D],
    moreData: ValidFunction[V, D]*
  )(using config: CoreConfig[D]): Constructed[V, D] = apply(data +: moreData)

  /**
    * Shorthand constructor for a single initial domain function that is valid in the full domain.
    *
    * @param value
    *   Domain function that is valid in the full domain (`Interval.unbounded[D]`).
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with a single valid domain function.
    */
  def ofValue[V, D <: NonEmptyTuple: DomainLike](value: DomainFunction[V, D])(using
    config: CoreConfig[D]
  ): Constructed[V, D] = of(Interval.unbounded[D] -> value)

  /**
    * Get a Builder based on an intermediate buffer of valid domain functions.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    config: CoreConfig[D]
  ): scala.collection.mutable.Builder[ValidFunction[V, D], Constructed[V, D]] =
    ValidData.Builds[DomainFunction[V, D], D, Constructed[V, D]](apply(_))

/**
  * Interface is similar to [[DimensionalBase]], but it operates on an underlying [[mutable.Data]] where values are
  * domain functions.
  *
  * @note
  *   This class exports methods from the underlying [[mutable.Data]] structure. Because of an open
  *   [[https://github.com/scala/scala3/issues/14342 Scala issue]], only exported methods without parameters are
  *   rendered correctly in the API docs. Although not in the API doc, these methods are also available:
  *   - [[mutable.Data.isDefinedAt isDefinedAt]]
  *   - [[mutable.Data.getDataAt getDataAt]]
  *   - [[mutable.Data.getIntersecting getIntersecting]]
  *   - [[mutable.Data.intersects intersects]]
  *   - [[mutable.Data.isSubsetOf isSubsetOf]]
  *   - [[mutable.Data.intervals intervals]]
  *   - [[mutable.Data.foldLeft foldLeft]]
  *   - [[mutable.Data.diffActionsFrom diffActionsFrom]]
  *   - [[mutable.Data.⊆ ⊆]]
  *
  * @tparam V
  *   the result type of the domain function managed as data.
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  *
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  * @define dataValueType
  *   the result type of the domain function managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define immutableReturn
  *   a new, updated structure.
  * @define mutableAction
  *   Data are mutated in place.
  * @define intersectionDesc
  *   The intersection of this and a single interval. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
  * @define intersectionParamInterval
  *   a single interval with which to intersect.
  * @define symmetricDifferenceDesc
  *   The "exclusive or" of this and that. That is, the portions of the that which are not in the domain of this and the
  *   portions of this which are not in the domain of that. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
  * @define symmetricDifferenceParamThat
  *   structure to combine.
  * @define mapValuesDesc
  *   Applies a function to all valid function results (i.e., functions are composed).
  * @define mapValuesParamF
  *   the function to compose with each valid function.
  * @define flatMapParamF
  *   the function to apply to each valid function which results in a new structure.
  * @define differenceDesc
  *   The elements in this which are not in the domain of that. The values of that are ignored. See
  *   [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
  * @define differenceParamThat
  *   shape to remove.
  * @define syncWithDesc
  *   Synchronizes this with another structure by getting and applying the applicable diff actions.
  * @define syncWithParamThat
  *   the structure with which this is synchronized.
  * @define mergeDesc
  *   Merges this structure with data from that structure. In intervals where both structures have valid functions, the
  *   two functions are merged (e.g., keep this data). In intervals where this does not have valid function data but
  *   that does, the data are added (a fill operation).
  * @define mergeParamThat
  *   structure to merge with this one
  * @define mergeParamMergeValues
  *   function that merges function results where both this and that have valid functions
  * @define mergeManyDesc
  *   Merges this structure with a collection of other data. For each, in intervals where valid functions already
  *   exists, the two functions are merged. In intervals where this does not have valid function data, the data are
  *   added (a fill operation).
  * @define mergeManyParamThatData
  *   collection of other data to merge with this
  * @define mergeManyParamMergeValues
  *   function that merges function results where both this and that have valid functions
  * @define setDesc
  *   Set new valid function data. Replaces any data previously valid in this interval.
  * @define setParamData
  *   the valid function data to set.
  * @define setManyDesc
  *   Set a collection of new valid function data. Replaces any data previously valid in this interval.
  * @define setManyNote
  *   if intervals overlap, later items will update earlier ones, so order can matter.
  * @define setManyParamData
  *   collection of valid function data to set.
  * @define removeDesc
  *   Remove valid functions on the interval. If there are values valid on portions of the interval, those values have
  *   their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeParamInterval
  *   the interval where any valid functions are removed.
  * @define removeManyDesc
  *   Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
  *   intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeManyParamIntervals
  *   the intervals where any valid functions are removed.
  */
trait DimensionalFunctionBase[V, D <: NonEmptyTuple: DomainLike](
  private[intervalidus] val underlying: MutableData[DomainFunction[V, D], D]
) extends PartialFunction[D, V]:

  @nowarn("msg=pattern selector should be an instance of Matchable")
  override def equals(obj: Any): Boolean = obj match
    case that: DimensionalFunctionBase[?, ?] => underlying == that.underlying
    case _                                   => false

  override def hashCode(): Int = underlying.hashCode()

  /**
    * Indicates whether some other dimensional function structure is "logically equivalent to" this one. That is, either
    * this and that are equal, or they are equal after being decompressed using the same base intervals (the same
    * decompression used in recompressAll).
    * @param that
    *   dimensional structure to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def isEquivalentTo(that: DimensionalFunctionBase[V, D]): Boolean = underlying.isEquivalentTo(that.underlying)

  /**
    * Same as [[isEquivalentTo]]
    *
    * Indicates whether some other dimensional function structure is "logically equivalent to" this one. That is, either
    * this and that are equal, or they are equal after being decompressed using the same base intervals (the same
    * decompression used in recompressAll).
    * @param that
    *   dimensional structure to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def ≡(that: DimensionalFunctionBase[V, D]): Boolean = isEquivalentTo(that)

  // Utility methods for accessing state, not part of the API

  protected def transformUnderlying[B, S <: NonEmptyTuple: DomainLike](
    f: ImmutableFunctionData[V, D] => ImmutableFunctionData[B, S]
  ): MutableFunctionData[B, S] = f(underlying.toImmutable).toMutable

  protected def zipData[B](that: DimensionalFunctionBase[B, D]): MutableFunctionData[(V, B), D] = transformUnderlying(
    _.zip(that.underlying).mapValues(_ fuse _)
  )

  protected def zipAllData[B](
    that: DimensionalFunctionBase[B, D],
    thisDefault: DomainFunction[V, D],
    thatDefault: DomainFunction[B, D]
  ): MutableFunctionData[(V, B), D] = transformUnderlying(
    _.zipAll(that.underlying, thisDefault, thatDefault).mapValues(_ fuse _)
  )

  protected def getByHeadDimensionData[H: DomainValueLike](domain: Domain1D[H])(using
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  )(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): MutableFunctionData[V, Domain.NonEmptyTail[D]] = transformUnderlying(
    _.getByHeadDimension(domain).mapValues(fd => fdTail => fd(fdTail.withHead(domain)))
  )

  protected def getByDimensionData[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    Domain.IsInsertedInResult[R, dimensionIndex.type, H, D]
  ): MutableFunctionData[V, R] = transformUnderlying(
    _.getByDimension(dimensionIndex, domain).mapValues(fd => (r => fd(r.insertDimension(dimensionIndex, domain))))
  )

  protected def collapseDimensionData[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mapFunctions: DomainFunction[V, D] => DomainFunction[V, R],
    mergeValues: (V, V) => V
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): MutableFunctionData[V, R] =
    val result = MutableData.empty[DomainFunction[V, R], R]
    val dataEntries = underlying.getAll.map(d => d.interval.dropDimension(dimensionIndex) -> mapFunctions(d.value))
    result.mergeMany(dataEntries, _.merge(_, mergeValues))
    result

  protected def extrudeDimensionData[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R],
    Domain.IsDroppedInResult[R, dimensionIndex.type, D]
  ): MutableFunctionData[V, R] = transformUnderlying(
    _.extrudeDimension(dimensionIndex, extent).mapValues(fd => (r => fd(r.dropDimension(dimensionIndex))))
  )

  // ---------- API methods like those in DimensionalBase, to be implemented by inheritor ----------

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same underlying data.
    */
  def copy(using CoreConfig[D]): DimensionalFunctionBase[V, D]

  /**
    * Returns a new structure formed from this structure and another structure by fusing the corresponding domain
    * functions (all intersections) into a new domain function that results in a pair. The other structure can have a
    * different value type but must have the same domain type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair domain function result.
    */
  def zip[B](that: DimensionalFunctionBase[B, D]): DimensionalFunctionBase[(V, B), D]

  /**
    * Returns a new structure formed from this structure and another structure by fusing the corresponding domain
    * functions (all intersections) into a new domain function that results in a pair. If one of the two collections has
    * a valid function in an interval where the other one doesn't, default domain functions are used in the result. The
    * other structure can have a different value type but must have the same domain type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisDefault
    *   default domain function used in intervals where functions are valid in that but not this.
    * @param thatDefault
    *   default domain function used in intervals where functions are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair domain function result.
    */
  def zipAll[B](
    that: DimensionalFunctionBase[B, D],
    thisDefault: DomainFunction[V, D],
    thatDefault: DomainFunction[B, D]
  ): DimensionalFunctionBase[(V, B), D]

  /**
    * Project as data in n-1 dimensions based on a lookup in the head dimension. All n-dimensional domain functions are
    * mapped as functions in n-1 dimensions. All n-dimensional domain functions are mapped as functions in n-1
    * dimensions.
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
    * @param altConfig
    *   $configParam
    * @return
    *   a lower-dimensional (n-1) structure
    */
  def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  )(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): DimensionalFunctionBase[V, Domain.NonEmptyTail[D]]

  /**
    * Creates a new structure with n-1 dimensions based on a lookup in the specified dimension. This is an n-1
    * dimensional "slicing" of the original structure (e.g., slice a 3d cube into its 2d sliver). All n-dimensional
    * domain functions are mapped as functions in n-1 dimensions.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    *
    * @param domain
    *   the domain element used for filtering
    * @param altConfig
    *   $configParam
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    *
    * @tparam R
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    *
    * @return
    *   a lower-dimensional (n-1) structure
    */
  def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    Domain.IsInsertedInResult[R, dimensionIndex.type, H, D] // extra witness for domain function mapping
  ): DimensionalFunctionBase[V, R]

  /**
    * Creates a new structure with n-1 dimensions by collapsing overlapping lower-dimensional intervals and
    * mapping/merging their domain functions. This is an n-1 dimensional "squashing" of the original structure (e.g.,
    * squash a translucent 3d cube into its 2d shadow representing how much light passes through it). All n-dimensional
    * domain functions are mapped as functions in n-1 dimensions (e.g., by sampling at a particular point in the dropped
    * dimension).
    *
    * @param dimensionIndex
    *   dimension to drop. Must be a value with a singleton type known at compile time, e.g., a numeric literal. (The
    *   head dimension is dimension 0.)
    *
    * @param mapFunctions
    *   function that maps a domain function from a domain in n dimensions to a domain in n-1 dimensions.
    * @param mergeValues
    *   function that merges values where there are multiple valid functions in a lower-dimensional interval (if
    *   mergeValues is not associative, results of multiple merges may be unpredictable).
    *
    * @param altConfig
    *   $configParam
    * @tparam R
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    *
    * @return
    *   a lower-dimensional (n-1) structure
    */
  def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mapFunctions: DomainFunction[V, D] => DomainFunction[V, R],
    mergeValues: (V, V) => V
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DimensionalFunctionBase[V, R]

  /**
    * Creates a new structure with n+1 dimensions by "extruding" all intervals in the specified dimension. This is an
    * n+1 dimensional "stretching" of the original structure (e.g., stretch a 2d square into a 3d cube). All
    * n-dimensional domain functions are mapped as functions in n+1 dimensions (by ignoring the added dimensions).
    *
    * @param dimensionIndex
    *   the dimension where the 1D interval is inserted (e.g., inserting a new head dimension is index 0). Existing
    *   dimensions are pushed to the right.
    *
    * @param extent
    *   the 1D interval to be inserted
    * @tparam H
    *   the domain value type of the extent
    * @tparam R
    *   the result domain. There is a type safety check that ensures the domain type for this result type is a
    *   concatenation of elements before the insert, the inserted dimension, and the elements after the insert.
    *
    * @return
    *   a higher-dimensional (n+1) structure
    */
  def extrudeDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R],
    Domain.IsDroppedInResult[R, dimensionIndex.type, D] // extra witness for domain function mapping
  ): DimensionalFunctionBase[V, R]

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: mutable.DataFunction[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: immutable.DataFunction[V, D]

  // ---------- API methods like those in DimensionalBase, implemented here ----------

  // from Object - print a uniform grid representing the underlying data ("<function>" is shown as the value)
  override def toString: String = underlying.toString

  // from PartialFunction
  override def apply(key: D): V = underlying(key).apply(key)

  /**
    * Returns a value that is valid at the specified domain element. That is, where the specified domain element is a
    * member of some valid function data interval. If no such valid function exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D): Option[V] =
    underlying.getAt(domainIndex).map(_.apply(domainIndex))

  /**
    * Intervals in the intersection of this and another structure. See
    * [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   another structure.
    * @return
    *   intervals representing the intersection of this and that. (The values in this and that are ignored.)
    */
  def intersection(that: DimensionalFunctionBase[?, D]): IntervalShape[D] = underlying.intersection(that.underlying)

  // ---------- Other API methods in DimensionalBase can be exported directly from underlying data ----------
  // ---------- (no function-specific arguments or return types) ----------

  export underlying.{
    // These methods don't take parameters, so scaladoc generates fine
    isEmpty,
    size,
    get,
    getOption,
    getAll,
    domain,
    values,
    allIntervals,
    boundingInterval,
    // These methods take parameters, so scaladoc does not generate
    isDefinedAt,
    getDataAt,
    getIntersecting,
    intersects,
    isSubsetOf,
    intervals,
    foldLeft,
    diffActionsFrom,
    ⊆
  }
