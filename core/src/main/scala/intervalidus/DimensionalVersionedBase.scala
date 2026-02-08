package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.mutable.Data as MutableData

import java.time.LocalDateTime
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Common definitions used in all versioned dimensional data (with a hidden version dimension).
  */
object DimensionalVersionedBase:
  type In1D[V, R1] = DimensionalVersionedBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalVersionedBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalVersionedBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalVersionedBase[V, Domain.In4D[R1, R2, R3, R4]]

  /**
    * The domain value type of the version.
    */
  type VersionDomainValue = Int

  /**
    * The domain type of the version.
    */
  type VersionDomain = Domain1D[VersionDomainValue]

  /**
    * The domain type for intervals used in the underlying data. It is one dimension higher than the provided
    * domain-like non-empty tuple, where the head dimension is the version domain.
    */
  type Versioned[X <: NonEmptyTuple] = VersionDomain *: X

  /**
    * Metadata tracked per version: datetime and comment
    */
  type VersionMetadata = (LocalDateTime, String)

  // Special version at which we place (or delete) unapproved stuff (fixed)
  val unapprovedStartVersion: VersionDomainValue = IntDiscreteValue.maxValue
  val unapprovedStartVersionDomain: VersionDomain = Domain1D.domain(unapprovedStartVersion)
  val approvedEndVersionDomain: VersionDomain = unapprovedStartVersionDomain.leftAdjacent

  /**
    * Context parameter for selecting the version interval on which to operate in selection and mutation. Typically,
    * "Current" is the right thing to use (and is provided as the default given) unless you are operating only on
    * potentially unapproved data or delving into version history by operating on a specific past version.
    */
  enum VersionSelection:
    case Current
    case Unapproved // potentially: when used in selection, returns unapproved plus approved not occluded by unapproved
    case Specific(version: VersionDomainValue)

  /**
    * Default version selection context when it isn't overridden locally or specified explicitly.
    */
  given VersionSelection = VersionSelection.Current

  /**
    * Version selection constructors
    */
  object VersionSelection:
    /**
      * Shorthand to construct version selection for a specific version.
      * @param version
      *   version to select.
      * @return
      *   version selection for that specific version.
      */
    def apply(version: VersionDomainValue): VersionSelection = VersionSelection.Specific(version)

import DimensionalVersionedBase.*

/**
  * Constructs data in multidimensional intervals that are also versioned (hidden extra dimension).
  *
  * @define objectDesc
  *   Constructs data in multidimensional intervals that are also versioned (hidden extra dimension).
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define paramInitialVersion
  *   the version to start with, typically zero
  */
trait DimensionalVersionedBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific interval starting at the initial
    * version.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param data
    *   value valid within an interval.
    * @param initialVersion
    *   $paramInitialVersion
    * @return
    *   [[DimensionalVersionedBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D],
    initialVersion: VersionDomainValue,
    initialComment: String
  )(using Experimental, DomainLike[Versioned[D]], CurrentDateTime): DimensionalVersionedBase[V, D]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval starting at the initial
    * version.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param value
    *   value that is valid in the full domain (`Interval.unbounded[D]`).
    * @param initialVersion
    *   $paramInitialVersion
    * @return
    *   [[DimensionalVersionedBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V,
    initialVersion: VersionDomainValue,
    initialComment: String
  )(using Experimental, DomainLike[Versioned[D]], CurrentDateTime): DimensionalVersionedBase[V, D]

  /**
    * Shorthand constructor for a collection of initial valid values starting at the initial version.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param initialData
    *   a collection of values valid within intervals -- intervals must be disjoint.
    * @param initialVersion
    *   $paramInitialVersion
    * @return
    *   [[DimensionalVersionedBase]] structure with the provided initial values
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]],
    initialVersion: VersionDomainValue,
    initialComment: String
  )(using Experimental, DomainLike[Versioned[D]], CurrentDateTime): DimensionalVersionedBase[V, D]

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @param initialVersion
    *   $paramInitialVersion
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V, D <: NonEmptyTuple: DomainLike](
    initialVersion: VersionDomainValue,
    initialComment: String
  )(using
    Experimental,
    DomainLike[Versioned[D]],
    CurrentDateTime
  ): mutable.Builder[ValidData[V, D], DimensionalVersionedBase[V, D]]

/**
  * Interface is similar to [[DimensionalBase]], but it operates on an underlying [[intervalidus.mutable.Data]] using an
  * extra integer-valued head dimension to version data. One use case would be versioned data that are valid in two
  * dimensions of time, so the underlying data actually vary in terms of version and two dimensions of time (three
  * dimensions). Most methods require some generic version selection criteria rather than specific integer intervals,
  * therefore this does not extend [[DimensionalBase]].
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue). When getting data, by default, we return "current"
  * version data (a.k.a., approved). When updating data, by default, we don't rewrite history, so mutations start with
  * the "current" version too.
  *
  * @note
  *   Updates starting with "current" also update unapproved changes (since intervalFrom goes to the Top).
  * @tparam V
  *   the type of the value managed as data.
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  *
  * @define classDescUseCase
  *   One use case would be versioned data that are valid in two dimensions of time, so the underlying data actually
  *   vary in terms of version and two dimensions of time (three dimensions).
  * @define classDescFeatures
  *   The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved
  *   data (unapproved data are pushed up to start at version maxValue). When getting data, by default, we return
  *   "current" version data (a.k.a., approved). When updating data, by default, we don't rewrite history, so mutations
  *   start with the "current" version too.
  * @define classNote
  *   Updates starting with "current" also update unapproved changes (since intervalFrom goes to the Top).
  * @define noVersionSelection
  *   Does not use a version selection context -- operates on full underlying structure.
  * @define noVersionSelectionFunction
  *   Does not use a version selection context -- the function is applied to the underlying data, so it
  * @define noVersionSelectionApply
  *   can operate on the underlying version information as well as the valid interval/value.
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define immutableReturn
  *   a new, updated structure.
  * @define mutableAction
  *   Data are mutated in place.
  * @define mapDesc
  *   Applies a function to all valid data.
  * @define mapParamF
  *   the function to apply to each valid data element.
  * @define collectDesc
  *   Applies a partial function to all valid data on which it is defined.
  * @define collectParamPf
  *   the partial function to apply to each versioned data element.
  * @define mapValuesDesc
  *   Applies a function to all valid data values.
  * @define mapValuesParamF
  *   the function to apply to the value part of each valid data element.
  * @define mapIntervalsDesc
  *   Applies a function to all valid data intervals.
  * @define mapIntervalsParamF
  *   the function to apply to the versioned interval part of each valid data element.
  * @define flatMapDesc
  *   Applies a function to all the elements of this structure
  * @define flatMapParamF
  *   the function to apply to each valid data element which results in a new structure.
  * @define filterDesc
  *   Updates structure to only include elements which satisfy a predicate.
  * @define filterParamP
  *   the predicate used to test elements.
  * @define setDesc
  *   Set new valid data. Given a version selection context, any data previously valid in this interval are replaced by
  *   this data.
  * @define setParamData
  *   the valid data to set.
  * @define setManyDesc
  *   Set a collection of new valid data. Given a version selection context, any data previously valid in the intervals
  *   are replaced by these data.
  * @define setManyNote
  *   if intervals overlap, later items will update earlier ones, so order can matter.
  * @define setManyParamData
  *   collection of valid data to set.
  * @define setIfNoConflictDesc
  *   Set new valid data, but only if there are no previously valid values in its interval and given the version
  *   selection context.
  * @define setIfNoConflictParamData
  *   the valid data to set.
  * @define updateDesc
  *   Update everything valid in the data's interval and the given version selection context to have the data's value.
  *   No new intervals of validity are added as part of this operation. Data with overlaps are adjusted accordingly.
  * @define updateParamData
  *   the new value and interval existing data should take on.
  * @define removeDesc
  *   Remove valid values on the interval and the given version selection context. If there are values valid on portions
  *   of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeParamInterval
  *   the interval where any valid values are removed.
  * @define removeManyDesc
  *   Remove data in all the intervals given a version selection context. If there are values valid on portions of any
  *   interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeManyParamIntervals
  *   the intervals where any valid values are removed.
  * @define removeValueDesc
  *   Remove the value in all the intervals where it is valid in the given version selection context.
  * @define removeValueParamValue
  *   the value that is removed.
  * @define compressDesc
  *   Compress out adjacent intervals with the same value.
  * @define compressParamValue
  *   value for which valid data are compressed.
  * @define compressAllDesc
  *   Compress out adjacent intervals with the same value for all values.
  * @define recompressAllDesc
  *   Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
  *   unique physical representation.
  * @define applyDiffActionsDesc
  *   Applies a sequence of diff actions to this structure.
  * @define applyDiffActionsParamDiffActions
  *   actions to be applied.
  * @define syncWithDesc
  *   Synchronizes this with another structure by getting and applying the applicable diff actions.
  * @define syncWithParamThat
  *   the structure with which this is synchronized.
  * @define fillDesc
  *   Given the version selection context, adds a value as valid in portions of the interval where there aren't already
  *   valid values.
  * @define fillParamData
  *   value to make valid in any validity gaps found in the interval
  * @define mergeDesc
  *   Merges this structure with data from that structure. In intervals where both structures have valid values, the two
  *   values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
  *   are added (a fill operation). (Version timestamps of this and that are merged.)
  * @define mergeParamThat
  *   versioned structure to merge with this one
  * @define mergeParamMergeValues
  *   function that merges values where both this and that have valid values, where the default merge operation is to
  *   give this data values priority and drop that data values
  * @define setCurrentVersionDesc
  *   Sets the current version. No version history is rewritten, which may cause some unexpected results (especially if
  *   the version is set to something from the past, which also rewrites version timestamp history). Use with caution.
  * @define setCurrentVersionParamVersion
  *   the new current version
  * @define incrementCurrentVersionDesc
  *   Increments the current version.
  * @define resetToVersionDesc
  *   Eliminate all version information after the specified version (including any unapproved changes).
  * @define resetToVersionParamVersion
  *   the version after which all version information is removed.
  * @define collapseVersionHistoryDesc
  *   Include only data based on the version selection context, but without any version history.
  * @define approveDesc
  *   Special case where we change the version interval by approving everything that is unapproved.
  * @define approveParamData
  *   currently unapproved data to approved
  * @define approveAllDesc
  *   Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
  * @define approveAllParamInterval
  *   interval in which all changes (updates and deletes) are approved
  */
trait DimensionalVersionedBase[V, D <: NonEmptyTuple: DomainLike](
  initialData: Iterable[ValidData[V, Versioned[D]]],
  initialVersion: VersionDomainValue,
  versionTimestamps: mutable.Map[VersionDomainValue, VersionMetadata],
  withCurrentVersion: Option[VersionDomainValue]
)(using
  Experimental,
  DomainLike[Versioned[D]]
) extends PartialFunction[Versioned[D], V]:

  // considers versions, but does not consider version metadata history in equality check
  override def equals(obj: Any): Boolean = obj match
    case that: DimensionalVersionedBase[V, D] @unchecked =>
      size == that.size && underlying.getAll.zip(that.underlying.getAll).forall(_ == _)
    case _ => false

  // Utility methods for managing "versioned" state, not part of API

  private type VersionInterval = Interval1D[VersionDomainValue]

  // The current version, mutable via access methods only
  protected var currentVersion: VersionDomainValue =
    withCurrentVersion.getOrElse(initialVersion)

  // prefer the later timestamp of both this and that
  protected def mergeVersionTimestamps(
    that: DimensionalVersionedBase[?, ?]
  ): mutable.Map[VersionDomainValue, VersionMetadata] =
    val those = that.getVersionTimestamps
    mutable.Map.from(
      (versionTimestamps.keySet ++ those.keySet)
        .map(k => (k, versionTimestamps.get(k), those.get(k)))
        .collect:
          case (k, Some(left), None)  => k -> left
          case (k, None, Some(right)) => k -> right
          case (k, Some((leftTimestamp, leftComment)), Some((rightTimestamp, rightComment))) =>
            val laterTimestamp = if leftTimestamp.isAfter(rightTimestamp) then leftTimestamp else rightTimestamp
            val combinedComment = if leftComment == rightComment then leftComment else s"$leftComment & $rightComment"
            k -> (laterTimestamp, combinedComment)
    )

  def getVersionTimestamps: Map[VersionDomainValue, VersionMetadata] =
    versionTimestamps.toMap

  // Underlying n+1 dimensional representation of versioned n dimensional data (mutable)
  protected val underlying: MutableData[V, Versioned[D]] = MutableData(initialData)

  // Construct an underlying domain from a public domain plus version selection
  protected def underlyingDomain(domain: D)(using versionSelection: VersionSelection): Versioned[D] =
    domain withHead versionSelection.boundary

  // Construct an underlying interval from a public interval with a version interval
  protected def underlyingIntervalWithVersion(
    interval: Interval[D],
    version: VersionInterval
  ): Interval[Versioned[D]] = interval withHead version

  // Construct an underlying interval from a public interval plus version selection (interval from boundary)
  protected def underlyingIntervalFromVersionBoundary(interval: Interval[D])(using
    versionSelection: VersionSelection
  ): Interval[Versioned[D]] =
    underlyingIntervalWithVersion(interval, versionSelection.intervalFrom)

  // Construct an underlying interval from a public interval plus version selection (interval at boundary)
  protected def underlyingIntervalAtVersionBoundary(interval: Interval[D])(using
    versionSelection: VersionSelection
  ): Interval[Versioned[D]] =
    underlyingIntervalWithVersion(interval, versionSelection.intervalAt)

  // Construct underlying valid data from public valid data plus version selection (interval from boundary)
  protected def underlyingValidDataFromVersionBoundary(data: ValidData[V, D])(using
    VersionSelection
  ): ValidData[V, Versioned[D]] =
    underlyingIntervalFromVersionBoundary(data.interval) -> data.value

  // Extract the version interval from some underlying valid data
  protected def versionInterval(data: ValidData[V, Versioned[D]]): VersionInterval =
    data.interval.headInterval1D

  // Construct new underlying valid data with an updated version interval
  protected def withVersionUpdate(
    data: ValidData[V, Versioned[D]],
    update: VersionInterval => VersionInterval
  ): ValidData[V, Versioned[D]] =
    data.copy(interval = data.interval.withHeadUpdate(update))

  // Extract public valid data from underlying data
  protected def publicValidData(data: ValidData[V, Versioned[D]]): ValidData[V, D] =
    data.interval.tailInterval -> data.value

  // special handling of versioned data because the public head is in dimension two.
  protected def getByHeadDimensionData[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]],
    DomainLike[Versioned[Domain.NonEmptyTail[D]]]
  ): Iterable[ValidData[V, Versioned[Domain.NonEmptyTail[D]]]] =
    val filteredUnderlyingData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        underlying.getAll.filter(_.interval.tailInterval.headInterval1D[H] contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withHeadUpdate[H](_ => Interval1D.intervalAt(domain))
        underlying.getIntersecting(lookup.withHead(Interval1D.unbounded))
    filteredUnderlyingData.map: data =>
      (data.interval.tailInterval.tailInterval withHead versionInterval(data)) -> data.value

  // special handling of versioned data because the public dimension n is in dimension n + 1.
  protected def getByDimensionData[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    DomainLike[Versioned[R]]
  ): Iterable[ValidData[V, Versioned[R]]] =
    val filteredUnderlyingData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        underlying.getAll.filter(_.interval.tailInterval(dimensionIndex) contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withDimensionUpdate[H](dimensionIndex, _ => Interval1D.intervalAt(domain))
        underlying.getIntersecting(lookup.withHead(Interval1D.unbounded))
    filteredUnderlyingData.map: data =>
      (data.interval.tailInterval.dropDimension(dimensionIndex) withHead versionInterval(data)) -> data.value

  // ---------- API methods that are not similar to those in DimensionalBase ----------

  /**
    * Returns the current version
    */
  def getCurrentVersion: VersionDomainValue = currentVersion

  /**
    * Given some version selection context, gets all the data (uncompressed, mutable).
    *
    * Useful when you don't need the overhead of creating an immutable structure and compressing the data, e.g., if just
    * doing a `getAt` after version selection, which gives the same answer if compressed or uncompressed, mutable or
    * immutable.
    *
    * @return
    *   new, uncompressed, mutable structure meeting version selection criteria.
    */
  def getSelectedDataMutable(using
    versionSelection: VersionSelection
  ): MutableData[V, D] =
    underlying.getByHeadDimension(versionSelection.boundary)

  /**
    * Given some version selection context, gets all the data (compressed, immutable).
    *
    * @return
    *   new, compressed, immutable structure meeting version selection criteria.
    */
  def getSelectedData(using
    VersionSelection
  ): immutable.Data[V, D] =
    getSelectedDataMutable.toImmutable.compressAll()

  /**
    * Gets all the data in all versions as an n+1 dimensional structure (immutable) where the version is represented in
    * the head dimension.
    *
    * @return
    *   new, immutable structure with n+1 dimensions.
    */
  def getVersionedData: immutable.Data[V, Versioned[D]] = underlying.toImmutable

  /**
    * Checks if a value is valid at the domain element given some version selection context. This is similar to
    * PartialFunction.isDefinedAt, but with a version selection context.
    *
    * @param key
    *   the domain element to test.
    * @return
    *   true if data is valid in the specified domain, false otherwise.
    */
  infix def isValidAt(key: D)(using VersionSelection): Boolean =
    underlying.isDefinedAt(underlyingDomain(key))

  extension (versionSelection: VersionSelection)
    /**
      * Returns the boundary of this version selection as a domain element
      */
    protected def boundary: VersionDomain = versionSelection match
      case VersionSelection.Current           => Domain1D.domain(getCurrentVersion)
      case VersionSelection.Unapproved        => Domain1D.domain(unapprovedStartVersion)
      case VersionSelection.Specific(version) => Domain1D.domain(version)

    /**
      * Returns the interval to the boundary of this version selection
      */
    def intervalTo: VersionInterval = Interval1D.intervalTo(boundary)

    /**
      * Returns the interval at the boundary of this version selection
      */
    def intervalAt: VersionInterval = Interval1D.intervalAt(boundary)

    /**
      * Returns the interval from the boundary of this version selection
      */
    def intervalFrom: VersionInterval = Interval1D.intervalFrom(boundary)

  // ---------- Implement methods like those in DimensionalBase ----------

  // from Object - print the current version and a uniform grid representing the underlying versioned data.
  override def toString: String = s"current version = $getCurrentVersion\n$underlying"

  // from PartialFunction
  override def isDefinedAt(key: Versioned[D]): Boolean = underlying.isDefinedAt(key)

  // from PartialFunction
  override def apply(key: Versioned[D]): V = underlying(key)

  /**
    * Tests if there are no valid data in this structure given some version selection criteria.
    * @return
    *   true if there are no valid data, false otherwise.
    */
  def isEmpty(using VersionSelection): Boolean = getSelectedDataMutable.isEmpty

  /**
    * The number of valid data entries. $noVersionSelection
    */
  def size: Int = underlying.size

  /**
    * Returns the value if a single, unbounded valid value exists given some version selection context, otherwise throws
    * an exception.
    *
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals), given the version selection provided.
    */
  def get(using VersionSelection): V = getSelectedData.get

  /**
    * Returns Some value if a single, unbounded valid value exists given some version selection context, otherwise
    * returns None.
    */
  def getOption(using VersionSelection): Option[V] = getSelectedData.getOption

  /**
    * Returns all valid data in interval start order given some version selection context.
    */
  def getAll(using VersionSelection): Iterable[ValidData[V, D]] = getSelectedData.getAll

  /**
    * Returns valid data at the specified domain element given some version selection context. That is, where the
    * specified domain element is a member of some valid data interval. If no such valid data exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value and corresponding interval if valid at the specified domain element, otherwise None.
    */
  def getDataAt(domainIndex: D)(using VersionSelection): Option[ValidData[V, D]] =
    underlying.getDataAt(underlyingDomain(domainIndex)).map(publicValidData)

  /**
    * Returns a value that is valid at the specified domain element given some version selection context. That is, where
    * the specified domain element is a member of some valid data interval. If no such valid value exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D)(using VersionSelection): Option[V] =
    underlying.getAt(underlyingDomain(domainIndex))

  /**
    * Returns all data that are valid on some or all of the provided interval given some version selection context.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: Interval[D])(using VersionSelection): Iterable[ValidData[V, D]] =
    underlying.getIntersecting(underlyingIntervalAtVersionBoundary(interval)).map(publicValidData)

  /**
    * Are there values that are valid on some or all of the provided interval given some version selection context?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: Interval[D])(using VersionSelection): Boolean =
    underlying.intersects(underlyingIntervalAtVersionBoundary(interval))

  /**
    * Returns all the intervals (compressed) in which there are valid values given some version selection context. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]].
    */
  def domain(using VersionSelection): Iterable[Interval[D]] = getSelectedDataMutable.domain

  /**
    * Returns all the intervals (compressed) in which there are no valid values given some version selection context.
    * That is, all intervals that are not in the [[domain]] given the same version selection context. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]] and
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    */
  def domainComplement(using VersionSelection): Iterable[Interval[D]] = getSelectedDataMutable.domainComplement

  /**
    * Returns the distinct values that are valid in some interval given some version selection context.
    */
  def values(using VersionSelection): Iterable[V] = getSelectedDataMutable.values

  /**
    * Returns the intervals in which this value is valid given some version selection context.
    *
    * @param value
    *   the value to look up
    */
  def intervals(value: V)(using VersionSelection): Iterable[Interval[D]] =
    Interval.compress(getSelectedDataMutable.intervals(value))

  /**
    * Returns the intervals in which any values are valid given some version selection context.
    */
  def allIntervals(using VersionSelection): Iterable[Interval[D]] =
    getSelectedData.allIntervals

  /**
    * Applies a binary operator to a start value and all valid data, going left to right. $noVersionSelectionFunction
    * can operate on the underlying version information as well as the valid interval/value.
    *
    * @param z
    *   the start value.
    * @param op
    *   the binary operator.
    * @tparam B
    *   the result type of the binary operator.
    * @return
    *   the result of inserting op between consecutive valid data elements, going left to right with the start value z
    *   on the left. Returns z if there are no valid data elements.
    */
  def foldLeft[B](z: B)(op: (B, ValidData[V, Versioned[D]]) => B): B = underlying.foldLeft(z)(op)

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    * $noVersionSelection
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DimensionalVersionedBase[V, D]): Iterable[DiffAction[V, Versioned[D]]] =
    underlying.diffActionsFrom(old.underlying)

  /**
    * Constructs a sequence of diff actions that, if applied to an old version of this structure, would synchronize it
    * with the newer version.
    *
    * @param olderVersion
    *   version selection for the older version of this structure.
    * @param newerVersion
    *   version selection for the newer version of this structure.
    * @return
    *   a sequence of diff actions that would synchronize the older version with the newer one.
    */
  def diffActionsBetween(
    olderVersion: VersionSelection,
    newerVersion: VersionSelection
  ): Iterable[DiffAction[V, Versioned[D]]] = resetTo(newerVersion).diffActionsFrom(resetTo(olderVersion))

  // ---------- To be implemented by inheritor ----------

  /**
    * Used internally (by [[diffActionsBetween]]) to select a specific versions.
    * @param selection
    *   version selection
    * @return
    *   this structure reset using the provided version selection
    */
  protected def resetTo(selection: VersionSelection): DimensionalVersionedBase[V, D]

  /**
    * Used internally (by [[resetToVersion]]) to extend intervals ending on or after the selection boundary.
    * @param keep
    *   version selection for kept data
    * @param d
    *   valid data to map
    * @return
    *   valid data with an updated version interval end based on the version selection
    */
  protected def extendInterval(keep: VersionSelection)(d: ValidData[V, Versioned[D]]): ValidData[V, Versioned[D]] =
    if versionInterval(d).end >= keep.boundary
    then withVersionUpdate(d, _.toTop)
    else d

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data and initial/current version.
    */
  def copy: DimensionalVersionedBase[V, D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intersections) in a pair. The other structure can have a different value type but must have the same interval
    * type. (Version timestamps of this and that are merged.)
    *
    * @param that
    *   the structure which is going to be zipped.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zip[B](that: DimensionalVersionedBase[B, D]): DimensionalVersionedBase[(V, B), D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intervals in both this and that) in a pair. If one of the two collections has a valid value in an interval
    * where the other one doesn't, default elements are used in the result. The other structure can have a different
    * value type but must have the same interval type. (Version timestamps of this and that are merged.)
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisDefault
    *   default element used in intervals where data are valid in that but not this.
    * @param thatDefault
    *   default element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](
    that: DimensionalVersionedBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): DimensionalVersionedBase[(V, B), D]

  /**
    * Project as data in n-1 dimensions based on a lookup in the head dimension -- version information is preserved.
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
    DomainLike[Domain.NonEmptyTail[D]],
    DomainLike[Versioned[Domain.NonEmptyTail[D]]]
  ): DimensionalVersionedBase[V, Domain.NonEmptyTail[D]]

  /**
    * Project as data in n-1 dimensions based on a lookup in the specified dimension -- version information is
    * preserved.
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
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
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
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    DomainLike[Versioned[R]]
  ): DimensionalVersionedBase[V, R]

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: intervalidus.mutable.DataVersioned[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: intervalidus.immutable.DataVersioned[V, D]
