package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue

import scala.language.implicitConversions

import DimensionalVersionedBase.{VersionDomain, VersionSelection}

/**
  * Constructs data in two-dimensional intervals that are also versioned (hidden third dimension).
  */
trait DataIn3DVersionedBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific interval starting at the initial
    * version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value
    * @param data
    *   data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn3DVersioned structure with a single valid value
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    data: ValidData3D[V, R1, R2, R3],
    initialVersion: Int
  )(using Experimental): DataIn3DVersionedBase[V, R1, R2, R3]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval starting at the initial
    * version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value
    * @param value
    *   value to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn3DVersioned structure with a single valid value
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    value: V,
    initialVersion: Int
  )(using Experimental): DataIn3DVersionedBase[V, R1, R2, R3]

  /**
    * Shorthand constructor for a collection of initial two-dimensional valid values starting at the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value
    * @param initialData
    *   valid data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn3DVersioned structure with the provided initial values
    */
  def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]],
    initialVersion: Int // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental): DataIn3DVersionedBase[V, R1, R2, R3]

/**
  * Interface is similar to [[DataIn3DBase]], but it operates on an underlying [[mutable.DataIn4D]] using an
  * integer-valued depth dimension to version data. One use case would be that R1 and R2 = LocalDate, so data values may
  * vary in terms of both version and two dimensions of time. Most methods require some generic version selection
  * criteria rather than specific integer intervals, therefore this does not extend [[DimensionalBase]].
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
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value
  * @param initialData
  *   a collection of valid data in three dimensions (the fourth dimension is the version) to start with --
  *   three-dimensional intervals must be disjoint
  * @param initialVersion
  *   the version to start with, typically zero. (Could, for example, use `IntDiscreteValue.minValue` instead of zero to
  *   extend the version range.)
  * @param withCurrentVersion
  *   the version to use as current if different form the initial version, e.g., when making a copy, typically None
  */
trait DataIn3DVersionedBase[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
  initialData: Iterable[ValidData4D[V, R1, R2, R3, Int]],
  val initialVersion: Int,
  withCurrentVersion: Option[VersionDomain]
)(using Experimental)
  extends DimensionalVersionedBase[
    V,
    Domain3D[R1, R2, R3],
    Interval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3],
    DiffAction3D[V, R1, R2, R3],
    Domain4D[R1, R2, R3, Int],
    Interval4D[R1, R2, R3, Int],
    ValidData4D[V, R1, R2, R3, Int],
    DiffAction4D[V, R1, R2, R3, Int],
    DataIn3DVersionedBase[V, R1, R2, R3]
  ]:

  override type PublicSelf = DataIn3DBase[V, R1, R2, R3]
  override type UnderlyingMutable = mutable.DataIn4D[V, R1, R2, R3, Int]
  override type UnderlyingImmutable = immutable.DataIn4D[V, R1, R2, R3, Int]

  // --- definitions unique to this "versioned" variant

  // The current version, mutable via access methods only
  protected var currentVersion: VersionDomain =
    withCurrentVersion.getOrElse(Domain1D.Point(initialVersion))

  // -- implement methods from DimensionalVersionedBase
  override protected def underlyingDomain(
    domain: Domain3D[R1, R2, R3]
  )(using versionSelection: VersionSelection): Domain4D[R1, R2, R3, Int] =
    domain x versionSelection.boundary

  override protected def underlyingIntervalFrom(
    interval: Interval3D[R1, R2, R3]
  )(using versionSelection: VersionSelection): Interval4D[R1, R2, R3, Int] =
    interval x versionSelection.intervalFrom

  override protected def underlyingIntervalAt(
    interval: Interval3D[R1, R2, R3]
  )(using versionSelection: VersionSelection): Interval4D[R1, R2, R3, Int] =
    interval x versionSelection.intervalAt

  override protected def underlyingIntervalWithVersion(
    interval: Interval3D[R1, R2, R3],
    version: Interval1D[Int]
  ): Interval4D[R1, R2, R3, Int] = interval x version

  override protected def underlyingValidData(
    data: ValidData3D[V, R1, R2, R3]
  )(using VersionSelection): ValidData4D[V, R1, R2, R3, Int] =
    underlyingIntervalFrom(data.interval) -> data.value

  override protected def publicValidData(
    data: ValidData4D[V, R1, R2, R3, Int]
  ): ValidData3D[V, R1, R2, R3] =
    (data.interval.horizontal x data.interval.vertical x data.interval.depth) -> data.value

  override protected def versionInterval(
    data: ValidData4D[V, R1, R2, R3, Int]
  ): Interval1D[Int] = data.interval.fourth

  override protected def withVersionUpdate(
    data: ValidData4D[V, R1, R2, R3, Int],
    update: Interval1D[Int] => Interval1D[Int]
  ): ValidData4D[V, R1, R2, R3, Int] =
    data.copy(interval = data.interval.withFourth(update(versionInterval(data))))

  // Underlying 4D representation of versioned 3D data (mutable)
  override protected val underlying: UnderlyingMutable = mutable.DataIn4D(initialData)

  override def getCurrentVersion: VersionDomain = currentVersion

  override def getSelectedDataMutable(using versionSelection: VersionSelection): mutable.DataIn3D[V, R1, R2, R3] =
    underlying.getByFourthIndex(versionSelection.boundary)

  override def getSelectedData(using VersionSelection): immutable.DataIn3D[V, R1, R2, R3] =
    getSelectedDataMutable.toImmutable.compressAll()

  // --- API methods similar to those in DataIn*D (can't use common super because of types)

  /**
    * Gets all the data in all versions as a 4D structure (immutable)
    *
    * @return
    *   new 4D structure.
    */
  def getDataIn4D: UnderlyingImmutable = underlying.toImmutable

  /**
    * Constructs a sequence of 4D diff actions that, if applied to the old structure, would synchronize it with this
    * one. Does not use a version selection context -- operates on full underlying 4D structure.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn3DVersionedBase[V, R1, R2, R3]): Iterable[DiffAction4D[V, R1, R2, R3, Int]] =
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
  def zip[B](that: DataIn3DVersionedBase[B, R1, R2, R3]): DataIn3DVersionedBase[(V, B), R1, R2, R3]

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
  def zipAll[B](
    that: DataIn3DVersionedBase[B, R1, R2, R3],
    thisElem: V,
    thatElem: B
  ): DataIn3DVersionedBase[(V, B), R1, R2, R3]
