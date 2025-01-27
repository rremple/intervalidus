package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue

import scala.language.implicitConversions

import DimensionalVersionedBase.{VersionDomain, VersionSelection}

/**
  * Constructs data in one-dimensional intervals that are also versioned (hidden second dimension).
  */
trait DataIn1DVersionedBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific one-dimensional interval starting at
    * the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of domain value used in the interval assigned to each value
    * @param data
    *   valid data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn1DVersioned structure with a single valid value
    */
  def of[V, R: DomainValueLike](
    data: ValidData1D[V, R],
    initialVersion: Int
  )(using Experimental): DataIn1DVersionedBase[V, R]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain starting at the initial
    * version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of domain value used in the interval assigned to each value
    * @param value
    *   value to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn1DVersioned structure with a single valid value
    */
  def of[V, R: DomainValueLike](
    value: V,
    initialVersion: Int
  )(using Experimental): DataIn1DVersionedBase[V, R]

  /**
    * Shorthand constructor for a collection of initial one-dimensional valid values starting at the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of domain value used in the interval assigned to each value
    * @param initialData
    *   valid data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn1DVersioned structure with the provided initial values
    */
  def from[V, R: DomainValueLike](
    initialData: Iterable[ValidData1D[V, R]],
    initialVersion: Int // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental): DataIn1DVersionedBase[V, R]

/**
  * Interface is similar to [[DataIn1DBase]], but it operates on an underlying [[mutable.DataIn2D]] using an
  * integer-valued vertical dimension to version data. One use case would be that R = LocalDate, so data values may vary
  * in terms of both version and time. Most methods require some generic version selection criteria rather than specific
  * integer intervals, therefore this does not extend [[DimensionalBase]].
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
  *   the type of the value managed as data
  * @tparam R
  *   the type of domain value used in the interval assigned to each valid value
  * @param initialData
  *   a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   two-dimensional intervals must be disjoint
  * @param initialVersion
  *   the version to start with, typically zero. (Could, for example, use `IntDiscreteValue.minValue` instead of zero to
  *   extend the version range.)
  * @param withCurrentVersion
  *   the version to use as current if different form the initial version, e.g., when making a copy, typically None
  */
trait DataIn1DVersionedBase[V, R: DomainValueLike](
  initialData: Iterable[ValidData2D[V, R, Int]],
  val initialVersion: Int,
  withCurrentVersion: Option[VersionDomain]
)(using Experimental)
  extends DimensionalVersionedBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R],
    Domain2D[R, Int],
    Interval2D[R, Int],
    ValidData2D[V, R, Int],
    DiffAction2D[V, R, Int],
    DataIn1DVersionedBase[V, R]
  ]:

  override type PublicSelf = DataIn1DBase[V, R]
  override type UnderlyingMutable = mutable.DataIn2D[V, R, Int]
  override type UnderlyingImmutable = immutable.DataIn2D[V, R, Int]

  // --- definitions unique to this "versioned" variant

  // The current version, mutable via access methods only
  protected var currentVersion: VersionDomain =
    withCurrentVersion.getOrElse(Domain1D.Point(initialVersion))

  // -- implement methods from DimensionalVersionedBase
  override protected def underlyingDomain(
    domain: Domain1D[R]
  )(using versionSelection: VersionSelection): Domain2D[R, Int] =
    domain x versionSelection.boundary

  override protected def underlyingIntervalFrom(
    interval: Interval1D[R]
  )(using versionSelection: VersionSelection): Interval2D[R, Int] =
    interval x versionSelection.intervalFrom

  override protected def underlyingIntervalAt(
    interval: Interval1D[R]
  )(using versionSelection: VersionSelection): Interval2D[R, Int] =
    interval x versionSelection.intervalAt

  override protected def underlyingIntervalWithVersion(
    interval: Interval1D[R],
    version: Interval1D[Int]
  ): Interval2D[R, Int] = interval x version

  override protected def underlyingValidData(
    data: ValidData1D[V, R]
  )(using VersionSelection): ValidData2D[V, R, Int] =
    underlyingIntervalFrom(data.interval) -> data.value

  override protected def publicValidData(
    data: ValidData2D[V, R, Int]
  ): ValidData1D[V, R] =
    data.interval.horizontal -> data.value

  override protected def versionInterval(
    data: ValidData2D[V, R, Int]
  ): Interval1D[Int] = data.interval.vertical

  override protected def withVersionUpdate(
    data: ValidData2D[V, R, Int],
    update: Interval1D[Int] => Interval1D[Int]
  ): ValidData2D[V, R, Int] =
    data.copy(interval = data.interval.withVertical(update(versionInterval(data))))

  // Underlying 2D representation of versioned 1D data (mutable)
  override protected val underlying: UnderlyingMutable = mutable.DataIn2D(initialData)

  override def getCurrentVersion: VersionDomain = currentVersion

  override def getSelectedDataMutable(using versionSelection: VersionSelection): mutable.DataIn1D[V, R] =
    underlying.getByVerticalIndex(versionSelection.boundary)

  override def getSelectedData(using VersionSelection): immutable.DataIn1D[V, R] =
    getSelectedDataMutable.toImmutable.compressAll()

  // --- API methods similar to those in DataIn*D (can't use common super because of types)

  /**
    * Gets all the data in all versions as a 2D structure (immutable)
    *
    * @return
    *   new 2D structure.
    */
  def getDataIn2D: UnderlyingImmutable = underlying.toImmutable

  /**
    * Constructs a sequence of 2D diff actions that, if applied to the old structure, would synchronize it with this
    * one. Does not use a version selection context -- operates on full underlying 2D structure.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn1DVersionedBase[V, R]): Iterable[DiffAction2D[V, R, Int]] =
    underlying.diffActionsFrom(old.underlying)

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
  def zip[B](that: DataIn1DVersionedBase[B, R]): DataIn1DVersionedBase[(V, B), R]

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
  def zipAll[B](that: DataIn1DVersionedBase[B, R], thisElem: V, thatElem: B): DataIn1DVersionedBase[(V, B), R]
