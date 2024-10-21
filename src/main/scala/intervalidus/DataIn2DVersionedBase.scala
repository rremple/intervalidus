package intervalidus

import scala.language.implicitConversions

import DimensionalVersionedBase.{VersionDomain, VersionSelection}

/**
  * Constructs data in one-dimensional intervals that are also versioned (hidden second dimension).
  */
trait DataIn2DVersionedBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific discrete interval starting at the
    * initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value
    * @param data
    *   data to start with
    * @param initialVersion
    *   the version to start with, typically zero
    * @return
    *   DataIn2DVersioned structure with a single valid value
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2],
    initialVersion: Int
  )(using Experimental): DataIn2DVersionedBase[V, R1, R2]

  /**
    * Shorthand constructor for a single initial value that is valid in the full discrete interval starting at the
    * initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value
    * @param value
    *   value to start with
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataIn2DVersioned structure with a single valid value
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V,
    initialVersion: Int
  )(using Experimental): DataIn2DVersionedBase[V, R1, R2]

  /**
    * Shorthand constructor for a collection of initial valid values starting at the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value
    * @param initialData
    *   valid data to start with
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataIn2DVersioned structure with the provided initial values
    */
  def from[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[V, R1, R2]],
    initialVersion: Int // could use summon[DiscreteValue[Int]].minValue to extend range
  )(using Experimental): DataIn2DVersionedBase[V, R1, R2]

/**
  * Interface is similar to [[DataIn2DBase]], but it operates on an underlying [[mutable.DataIn3D]] using an
  * integer-valued vertical dimension to version data. One use case would be that R1 and R2 = LocalDate, so data values
  * may vary in terms of both version and two dimensions of time. Most methods require some generic version selection
  * criteria rather than specific integer intervals, therefore this does not extend [[DimensionalBase]].
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too. Note that updates starting with
  * "current" will update unapproved changes as well (since intervalFrom goes to the Top).
  *
  * @tparam V
  *   the type of the value managed as data
  * @tparam R1
  *   the type of discrete value used in the horizontal discrete interval assigned to each value
  * @tparam R2
  *   the type of discrete value used in the vertical discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   note that two-dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero. (Could, for example, use `IntDiscreteValue.minValue` instead
  *   of zero to extend the version range.)
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
trait DataIn2DVersionedBase[V, R1: DiscreteValue, R2: DiscreteValue](
  initialData: Iterable[ValidData3D[V, R1, R2, Int]],
  val initialVersion: Int,
  withCurrentVersion: Option[VersionDomain]
)(using Experimental)
  extends DimensionalVersionedBase[
    V,
    DiscreteDomain2D[R1, R2],
    DiscreteInterval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2],
    DiscreteDomain3D[R1, R2, Int],
    DiscreteInterval3D[R1, R2, Int],
    ValidData3D[V, R1, R2, Int],
    DiffAction3D[V, R1, R2, Int],
    DataIn2DVersionedBase[V, R1, R2]
  ]:

  override type PublicSelf = DataIn2DBase[V, R1, R2]
  override type UnderlyingMutable = mutable.DataIn3D[V, R1, R2, Int]
  override type UnderlyingImmutable = immutable.DataIn3D[V, R1, R2, Int]

  // --- definitions unique to this "versioned" variant

  // The current version, mutable via access methods only
  protected var currentVersion: VersionDomain =
    withCurrentVersion.getOrElse(DiscreteDomain1D.Point(initialVersion))

  // -- implement methods from DimensionalVersionedBase
  override protected def underlyingDomain(
    domain: DiscreteDomain2D[R1, R2]
  )(using versionSelection: VersionSelection): DiscreteDomain3D[R1, R2, Int] =
    domain x versionSelection.boundary

  override protected def underlyingIntervalFrom(
    interval: DiscreteInterval2D[R1, R2]
  )(using versionSelection: VersionSelection): DiscreteInterval3D[R1, R2, Int] =
    interval x versionSelection.intervalFrom

  override protected def underlyingIntervalAt(
    interval: DiscreteInterval2D[R1, R2]
  )(using versionSelection: VersionSelection): DiscreteInterval3D[R1, R2, Int] =
    interval x versionSelection.intervalAt

  override protected def underlyingIntervalWithVersion(
    interval: DiscreteInterval2D[R1, R2],
    version: DiscreteInterval1D[Int]
  ): DiscreteInterval3D[R1, R2, Int] = interval x version

  override protected def underlyingValidData(
    data: ValidData2D[V, R1, R2]
  )(using VersionSelection): ValidData3D[V, R1, R2, Int] =
    underlyingIntervalFrom(data.interval) -> data.value

  override protected def publicValidData(
    data: ValidData3D[V, R1, R2, Int]
  ): ValidData2D[V, R1, R2] =
    (data.interval.horizontal x data.interval.vertical) -> data.value

  override protected def versionInterval(
    data: ValidData3D[V, R1, R2, Int]
  ): DiscreteInterval1D[Int] = data.interval.depth

  override protected def withVersionUpdate(
    data: ValidData3D[V, R1, R2, Int],
    update: DiscreteInterval1D[Int] => DiscreteInterval1D[Int]
  ): ValidData3D[V, R1, R2, Int] =
    data.copy(interval = data.interval.withDepth(update(versionInterval(data))))

  // Underlying 3D representation of versioned 2D data (mutable)
  override protected val underlying: UnderlyingMutable = mutable.DataIn3D(initialData)

  override def getCurrentVersion: VersionDomain = currentVersion

  override def getSelectedDataMutable(using versionSelection: VersionSelection): mutable.DataIn2D[V, R1, R2] =
    underlying.getByDepthIndex(versionSelection.boundary)

  override def getSelectedData(using VersionSelection): immutable.DataIn2D[V, R1, R2] =
    getSelectedDataMutable.toImmutable.compressAll()

  // --- API methods similar to those in DataIn*D (can't use common super because of types)

  /**
    * Gets all the data in all versions as a 3D structure (immutable)
    *
    * @return
    *   new 3D structure.
    */
  def getDataIn3D: UnderlyingImmutable = underlying.toImmutable

  /**
    * Constructs a sequence of 3D diff actions that, if applied to the old structure, would synchronize it with this
    * one. Does not use a version selection context -- operates on full underlying 3D structure.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn2DVersionedBase[V, R1, R2]): Iterable[DiffAction3D[V, R1, R2, Int]] =
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
  def zip[B](that: DataIn2DVersionedBase[B, R1, R2]): DataIn2DVersionedBase[(V, B), R1, R2]

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
  def zipAll[B](that: DataIn2DVersionedBase[B, R1, R2], thisElem: V, thatElem: B): DataIn2DVersionedBase[(V, B), R1, R2]
