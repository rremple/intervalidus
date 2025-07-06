package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue

import scala.language.implicitConversions

/**
  * Common definitions used in all versioned dimensional data (with a hidden version dimension).
  */
object DimensionalVersionedBase:
  type In1D[V, R1] = DimensionalVersionedBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalVersionedBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalVersionedBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalVersionedBase[V, Domain.In4D[R1, R2, R3, R4]]

  /**
    * The domain type of the version.
    */
  type VersionDomain = Domain1D[Int]

  /**
    * The domain type for intervals used in the underlying data. It is one dimension higher than the provided
    * domain-like non-empty tuple, where the head dimension is the version domain.
    */
  type Versioned[X <: NonEmptyTuple] = VersionDomain *: X

  /**
    * Context parameter for selecting the version interval on which to operate in selection and mutation. Typically,
    * "Current" is the right thing to use (and is provided as the default given) unless you are operating only on
    * potentially unapproved data or delving into version history by operating on a specific past version.
    */
  enum VersionSelection:
    case Current
    case Unapproved // potentially: when used in selection, returns unapproved plus approved not occluded by unapproved
    case Specific(version: VersionDomain)

  /**
    * Default version selection context when not overridden locally or specified explicitly
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
    def apply(version: VersionDomain): VersionSelection = VersionSelection.Specific(version)

import DimensionalVersionedBase.{Versioned, VersionDomain, VersionSelection}

/**
  * Constructs data in multidimensional intervals that are also versioned (hidden extra dimension).
  */
trait DimensionalVersionedBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific interval starting at the initial
    * version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam D
    *   the type of domain value used in the interval assigned to each value
    * @param data
    *   data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataVersioned structure with a single valid value
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D],
    initialVersion: Int
  )(using Experimental, DomainLike[Versioned[D]]): DimensionalVersionedBase[V, D]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval starting at the initial
    * version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam D
    *   the type of domain value used in the interval assigned to each value
    * @param value
    *   value to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataVersioned structure with a single valid value
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V,
    initialVersion: Int
  )(using Experimental, DomainLike[Versioned[D]]): DimensionalVersionedBase[V, D]

  /**
    * Shorthand constructor for a collection of initial valid values starting at the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam D
    *   the type of domain value used in the interval assigned to each value
    * @param initialData
    *   valid data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataVersioned structure with the provided initial values
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]],
    initialVersion: Int // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental, DomainLike[Versioned[D]]): DimensionalVersionedBase[V, D]

/**
  * Base for all versioned dimensional data, both mutable and immutable, in any dimension.
  *
  * Interface is similar to [[DimensionalBase]], but it operates on an underlying [[mutable.Data]] using an extra
  * integer-valued head dimension to version data. One use case would be versioned data that are valid in two dimensions
  * of time, so the underlying data actually vary in terms of version and two dimensions of time (three dimensions).
  * Most methods require some generic version selection criteria rather than specific integer intervals, therefore this
  * does not extend [[DimensionalBase]].
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too.
  * @note
  *   Updates starting with "current" also update unapproved changes (since intervalFrom goes to the Top).
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be [[DomainLike]].
  */
trait DimensionalVersionedBase[
  V,
  D <: NonEmptyTuple: DomainLike
](
  initialData: Iterable[ValidData[V, Versioned[D]]],
  initialVersion: Int,
  withCurrentVersion: Option[VersionDomain]
)(using
  Experimental,
  DomainLike[Versioned[D]]
) extends PartialFunction[Versioned[D], V]:

  private type VersionValue = DiscreteValue[Int]
  private type VersionInterval = Interval1D[Int]
  private val VersionInterval = intervalidus.Interval1D

  // --- definitions unique to this "versioned" variant

  // The current version, mutable via access methods only
  protected var currentVersion: VersionDomain =
    withCurrentVersion.getOrElse(Domain1D.Point(initialVersion))

  // Underlying n+1 dimensional representation of versioned n dimensional data (mutable)
  protected val underlying: mutable.Data[V, Versioned[D]] = mutable.Data(initialData)

  // Construct underlying interval from public interval plus version selection
  protected def underlyingDomain(domain: D)(using versionSelection: VersionSelection): Versioned[D] =
    domain withHead versionSelection.boundary

  // Construct underlying interval from public interval plus version selection (intervalFrom)
  protected def underlyingIntervalFrom(interval: Interval[D])(using
    versionSelection: VersionSelection
  ): Interval[Versioned[D]] =
    interval withHead versionSelection.intervalFrom

  // Construct underlying interval from public interval plus version selection (at boundary)
  protected def underlyingIntervalAt(interval: Interval[D])(using
    versionSelection: VersionSelection
  ): Interval[Versioned[D]] =
    interval withHead versionSelection.intervalAt

  // Construct underlying data from public data plus version selection
  protected def underlyingValidData(data: ValidData[V, D])(using VersionSelection): ValidData[V, Versioned[D]] =
    underlyingIntervalFrom(data.interval) -> data.value

  // Construct new underlying interval from a public interval with a version interval
  protected def underlyingIntervalWithVersion(
    interval: Interval[D],
    version: VersionInterval
  ): Interval[Versioned[D]] = interval withHead version

  // Extract version interval from underlying data
  protected def versionInterval(data: ValidData[V, Versioned[D]]): VersionInterval =
    data.interval.headInterval1D[Int]

  // Construct new underlying data with an updated version interval
  protected def withVersionUpdate(
    data: ValidData[V, Versioned[D]],
    update: VersionInterval => VersionInterval
  ): ValidData[V, Versioned[D]] =
    data.copy(interval = data.interval.withHeadUpdate(update))

  // Extract public data from underlying data
  protected def publicValidData(data: ValidData[V, Versioned[D]]): ValidData[V, D] =
    data.interval.tailInterval[D] -> data.value

  /**
    * Returns the current version
    */
  def getCurrentVersion: VersionDomain = currentVersion

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
  ): mutable.Data[V, D] =
    underlying.getByHeadIndex(versionSelection.boundary)

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

  // ---------- Implement methods not similar to those in DimensionalBase ----------

  // Special version at which we place (or delete) unapproved stuff (fixed)
  protected val unapprovedStartVersion: VersionDomain = summon[VersionValue].maxValue

  extension (versionSelection: VersionSelection)
    /**
      * Returns the boundary of this version selection as a domain element
      */
    protected def boundary: VersionDomain = versionSelection match
      case VersionSelection.Current           => getCurrentVersion
      case VersionSelection.Unapproved        => unapprovedStartVersion
      case VersionSelection.Specific(version) => version

    /**
      * Returns the interval to the boundary of this version selection
      */
    def intervalTo: VersionInterval = VersionInterval.intervalTo(boundary)

    /**
      * Returns the interval at the boundary of this version selection
      */
    def intervalAt: VersionInterval = VersionInterval.intervalAt(boundary)

    /**
      * Returns the interval from the boundary of this version selection
      */
    def intervalFrom: VersionInterval = VersionInterval.intervalFrom(boundary)

  // from PartialFunction
  override def isDefinedAt(key: Versioned[D]): Boolean = underlying.isDefinedAt(key)

  // from PartialFunction
  override def apply(key: Versioned[D]): V = underlying(key)

  // from Object - print the current version and a uniform grid representing the underlying versioned data.
  override def toString: String = s"current version = $getCurrentVersion\n$underlying"

  // ---------- Implement methods like those in DimensionalBase ----------

  // --- API methods similar to those in DataIn*D (can't use common super because of types)

  /**
    * Gets all the data in all versions as an n+1 dimensional structure (immutable)
    *
    * @return
    *   new, immutable structure with n+1 dimensions.
    */
  def getVersionedData: immutable.Data[V, Versioned[D]] = underlying.toImmutable

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    * Does not use a version selection context -- operates on full underlying structure.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DimensionalVersionedBase[V, D]): Iterable[DiffAction[V, Versioned[D]]] =
    underlying.diffActionsFrom(old.underlying)

  /**
    * Tests if there are no valid data in this structure given some version selection criteria.
    * @return
    *   true if there are no valid data, false otherwise.
    */
  def isEmpty(using VersionSelection): Boolean = getSelectedDataMutable.isEmpty

  /**
    * Returns the value if there is a single, unbounded valid value given some version selection context, otherwise
    * throws an exception.
    *
    * @return
    *   a single valid value.
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals), given the version selection provided.
    */
  def get(using VersionSelection): V = getSelectedData.get

  /**
    * Returns a Some value if there is a single, unbounded valid value given some version selection context, otherwise
    * returns None.
    */
  def getOption(using VersionSelection): Option[V] = getSelectedData.getOption

  /**
    * Get all valid data given some version selection context.
    *
    * @return
    *   all valid data given some version selection context in interval.start order
    */
  def getAll(using VersionSelection): Iterable[ValidData[V, D]] = getSelectedData.getAll

  /**
    * Returns a value that is valid at the specified domain element given some version selection context. That is, where
    * the specified domain element is a member of some valid data interval. If no such valid data exists, returns None.
    *
    * @param domain
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domain: D)(using VersionSelection): Option[V] =
    underlying.getAt(underlyingDomain(domain))

  /**
    * Checks if a value is valid at the domain element given some version selection context.
    *
    * @param key
    *   the domain element to test.
    * @return
    *   true if data is valid in the specified domain, false otherwise.
    */
  infix def isValidAt(key: D)(using VersionSelection): Boolean =
    underlying.isDefinedAt(underlyingDomain(key))

  /**
    * Returns all data that are valid on some or all of the provided interval given some version selection context.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: Interval[D])(using VersionSelection): Iterable[ValidData[V, D]] =
    underlying.getIntersecting(underlyingIntervalAt(interval)).map(publicValidData)

  /**
    * Are there values that are valid on some or all of the provided interval given some version selection context?
    *
    * @param interval
    *   the interval to check
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: Interval[D])(using VersionSelection): Boolean =
    underlying.intersects(underlyingIntervalAt(interval))

  /**
    * Returns all the intervals (compressed) in which there are valid values given some version selection context.
    */
  def domain(using VersionSelection): Iterable[Interval[D]] = getSelectedDataMutable.domain

  /**
    * Returns all the intervals (compressed) in which there are no valid values given some version selection context.
    */
  def domainComplement(using VersionSelection): Iterable[Interval[D]] = getSelectedDataMutable.domainComplement

  /**
    * Applies a binary operator to a start value and all valid data, going left to right. Does not use a version
    * selection context -- the function is applied to the underlying data, so it can operate on the underlying version
    * information as well as the valid interval/value.
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

  // ---------- To be implemented by inheritor ----------

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: intervalidus.mutable.DataVersioned[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: intervalidus.immutable.DataVersioned[V, D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intersections) in a pair. The other structure can have a different value type but must have the same interval
    * type.
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
    * (all intersections) in a pair. If one of the two collections has a valid value in an interval where the other one
    * doesn't, placeholder elements are used in the result. The other structure can have a different value type but must
    * have the same interval type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisElem
    *   placeholder element used in intervals where data are valid in that but not this.
    * @param thatElem
    *   placeholder element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](that: DimensionalVersionedBase[B, D], thisElem: V, thatElem: B): DimensionalVersionedBase[(V, B), D]
