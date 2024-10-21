package intervalidus

import intervalidus.DimensionalBase.DiffActionLike

import scala.language.implicitConversions

/**
  * Common definitions used in all versioned dimensional data (with a hidden version dimension).
  */
object DimensionalVersionedBase:

  type VersionDomain = DiscreteDomain1D[Int]

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

import DimensionalBase.{DataLike, DomainLike, IntervalLike}

/**
  * Base for all versioned dimensional data, both mutable and immutable, both 1D and 2D (with underlying data in both 2D
  * and 3D).
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be `DomainLike` and have an `Ordering`.
  * @tparam I
  *   the interval type in the public interface. Must be `IntervalLike` based on [[D]].
  * @tparam ValidData
  *   the valid data type in the public interface. Must be `DataLike` based on [[V]], [[D]], and [[I]].
  * @tparam D2
  *   the domain type for intervals used in the underlying data. Must be `DomainLike` and have an `Ordering`, and should
  *   be one dimension higher than [[D]], where the last dimension is `Int`.
  * @tparam I2
  *   the interval type of the underlying data. Must be `IntervalLike` based on [[D2]], and should be one dimension
  *   higher than [[I]], where the last dimension is `Int`.
  * @tparam ValidData2
  *   the valid data type of the underlying data. Must be `DataLike` based on [[V]], [[D2]], and [[I2]].
  * @tparam Self
  *   F-bounded self type.
  */
trait DimensionalVersionedBase[
  V,
  D <: DomainLike[D]: Ordering,
  I <: IntervalLike[D, I],
  ValidData <: DataLike[V, D, I, ValidData],
  DiffAction <: DiffActionLike[V, D, I, ValidData, DiffAction],
  D2 <: DomainLike[D2]: Ordering,
  I2 <: IntervalLike[D2, I2],
  ValidData2 <: DataLike[V, D2, I2, ValidData2],
  DiffAction2 <: DiffActionLike[V, D2, I2, ValidData2, DiffAction2],
  Self <: DimensionalVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, Self]
](using
  Experimental
) extends PartialFunction[D2, V]:

  import DimensionalVersionedBase.{VersionDomain, VersionSelection}

  // ---------- To be implemented by inheritor ----------

  type PublicSelf <: DimensionalBase[V, D, I, ValidData, DiffAction, PublicSelf]
  type UnderlyingMutable <: mutable.MutableBase[
    V,
    D2,
    I2,
    ValidData2,
    DiffAction2,
    UnderlyingMutable
  ] with DimensionalBase[
    V,
    D2,
    I2,
    ValidData2,
    DiffAction2,
    _
  ]
  type UnderlyingImmutable <: immutable.ImmutableBase[
    V,
    D2,
    I2,
    ValidData2,
    DiffAction2,
    UnderlyingImmutable
  ] with DimensionalBase[
    V,
    D2,
    I2,
    ValidData2,
    DiffAction2,
    _
  ]

  // Underlying n+1 dimensional representation of versioned n dimensional data (mutable)
  protected def underlying: UnderlyingMutable

  // Construct underlying interval from public interval plus version selection
  protected def underlyingDomain(domain: D)(using VersionSelection): D2

  // Construct underlying interval from public interval plus version selection (intervalFrom)
  protected def underlyingIntervalFrom(interval: I)(using VersionSelection): I2

  // Construct underlying interval from public interval plus version selection (at boundary)
  protected def underlyingIntervalAt(interval: I)(using VersionSelection): I2

  // Construct underlying data from public data plus version selection
  protected def underlyingValidData(data: ValidData)(using VersionSelection): ValidData2

  // Extract version interval from underlying data
  protected def versionInterval(data: ValidData2): DiscreteInterval1D[Int]

  // Construct new underlying interval from a public interval with a version interval
  protected def underlyingIntervalWithVersion(
    interval: I,
    version: DiscreteInterval1D[Int]
  ): I2

  // Construct new underlying data with an updated version interval
  protected def withVersionUpdate(
    data: ValidData2,
    update: DiscreteInterval1D[Int] => DiscreteInterval1D[Int]
  ): ValidData2

  // Extract public data from underlying data
  protected def publicValidData(data: ValidData2): ValidData

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: Self

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: Self

  /**
    * Returns the current version
    */
  def getCurrentVersion: VersionDomain

  /**
    * Based on the given version selection context, gets all the data (uncompressed, mutable).
    *
    * Useful when you don't need the overhead of creating an immutable structure and compressing the data, e.g., if just
    * doing a `getAt` after version selection, which will give the same answer compressed or uncompressed, mutable or
    * immutable.
    *
    * @param versionSelection
    *   version selection context for this operation.
    * @return
    *   new, uncompressed, mutable structure meeting version selection criteria.
    */
  def getSelectedDataMutable(using
    versionSelection: VersionSelection
  ): PublicSelf with mutable.MutableBase[V, D, I, ValidData, DiffAction, _]

  /**
    * Based on the given version selection context, gets all the data (compressed, immutable).
    *
    * @param versionSelection
    *   version selection context for this operation.
    * @return
    *   new, compressed, immutable structure meeting version selection criteria.
    */
  def getSelectedData(using
    versionSelection: VersionSelection
  ): PublicSelf with immutable.ImmutableBase[V, D, I, ValidData, DiffAction, _]

  // ---------- Implement methods not similar to those in DimensionalBase ----------

  // Special version at which we place (or delete) unapproved stuff (fixed)
  protected val unapprovedStartVersion: VersionDomain = summon[DiscreteValue[Int]].maxValue

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
    def intervalTo: DiscreteInterval1D[Int] = DiscreteInterval1D.intervalTo(boundary)

    /**
      * Returns the interval at the boundary of this version selection
      */
    def intervalAt: DiscreteInterval1D[Int] = DiscreteInterval1D.intervalAt(boundary)

    /**
      * Returns the interval from the boundary of this version selection
      */
    def intervalFrom: DiscreteInterval1D[Int] = DiscreteInterval1D.intervalFrom(boundary)

  // from PartialFunction
  override def isDefinedAt(key: D2): Boolean = underlying.isDefinedAt(key)

  // from PartialFunction
  override def apply(key: D2): V = underlying(key)

  // from Object - print the current version and a uniform grid representing the underlying versioned data.
  override def toString: String = s"current version = $getCurrentVersion\n$underlying"

  // ---------- Implement methods like those in DimensionalBase ----------

  /**
    * Returns true when there are no valid data in this structure meeting the version selection criteria, otherwise
    * false.
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
  def getAll(using VersionSelection): Iterable[ValidData] = getSelectedData.getAll

  /**
    * Returns a value that is valid in the specified interval domain element given some version selection context. That
    * is, where the specified domain element is a member of some valid data interval. If no such valid data exists,
    * returns None.
    *
    * @param domain
    *   the domain element where data may be valid. Note that the domain element can be a specific data point or the
    *   special notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domain: D)(using VersionSelection): Option[V] =
    underlying.getAt(underlyingDomain(domain))

  /**
    * Is a value defined at a part of the domain given some version selection context.
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
  def getIntersecting(interval: I)(using VersionSelection): Iterable[ValidData] =
    underlying.getIntersecting(underlyingIntervalAt(interval)).map(publicValidData)

  /**
    * Are there values that are valid on some or all of the provided interval given some version selection context?
    *
    * @param interval
    *   the interval to check
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: I)(using VersionSelection): Boolean =
    underlying.intersects(underlyingIntervalAt(interval))

  /**
    * Returns all the intervals (compressed) in which there are valid values given some version selection context.
    */
  def domain(using VersionSelection): Iterable[I] = getSelectedDataMutable.domain

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
  def foldLeft[B](z: B)(op: (B, ValidData2) => B): B = underlying.foldLeft(z)(op)
