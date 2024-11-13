package intervalidus.mutable

import intervalidus.*
import intervalidus.immutable.DataIn2DVersioned as DataIn2DVersionedImmutable
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataIn2DVersioned extends DataIn2DVersionedBaseObject:
  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2],
    initialVersion: Int
  )(using Experimental): DataIn2DVersioned[V, R1, R2] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V,
    initialVersion: Int = 0
  )(using Experimental): DataIn2DVersioned[V, R1, R2] =
    of(DiscreteInterval2D.unbounded[R1, R2] -> value, initialVersion)

  override def from[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[V, R1, R2]],
    initialVersion: Int = 0 // could use summon[DiscreteValue[Int]].minValue to extend range
  )(using Experimental): DataIn2DVersioned[V, R1, R2] = DataIn2DVersioned[V, R1, R2](
    initialData.map(d => (d.interval x DiscreteInterval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

/**
  * Interface is similar to [[DataIn2D]], but it operates on an underlying [[DataIn3D]] using an integer-valued
  * interval2 to version data. One use case would be that R = LocalDate, so data values may vary in terms of both
  * version and date. Most methods require some generic version selection criteria rather than specific integer
  * intervals.
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too. Note that updates starting with
  * "current" will update unapproved changes as well (since intervalFrom goes to the Top)
  *
  * @tparam V
  *   the type of the value managed as data
  * @tparam R1
  *   the type of discrete value used in the horizontal discrete interval assigned to each value
  * @tparam R2
  *   the type of discrete value used in the vertical discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in three dimensions (the depth dimension is the version) to start with --
  *   note that three-dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
class DataIn2DVersioned[V, R1: DiscreteValue, R2: DiscreteValue](
  initialData: Iterable[ValidData3D[V, R1, R2, Int]] = Iterable.empty[ValidData3D[V, R1, R2, Int]],
  initialVersion: Int = 0, // could use summon[DiscreteValue[Int]].minValue to extend range
  withCurrentVersion: Option[VersionDomain] = None
)(using Experimental)
  extends DataIn2DVersionedBase[V, R1, R2](initialData, initialVersion, withCurrentVersion)
  with MutableVersionedBase[
    V,
    DiscreteDomain2D[R1, R2],
    DiscreteInterval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2],
    DiscreteDomain3D[R1, R2, Int],
    DiscreteInterval3D[R1, R2, Int],
    ValidData3D[V, R1, R2, Int],
    DiffAction3D[V, R1, R2, Int],
    DataIn2DVersioned[V, R1, R2]
  ]:

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataIn2DVersionedImmutable[V, R1, R2] = DataIn2DVersionedImmutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def toMutable: DataIn2DVersioned[V, R1, R2] = this

  // ---------- Implement methods from MutableVersionedBase ----------

  override def copy: DataIn2DVersioned[V, R1, R2] = DataIn2DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv DiscreteDomain1D.Bottom then throw Exception("version too small")
    else currentVersion = version

  override def incrementCurrentVersion(): Unit = synchronized:
    if currentVersion.successor equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else currentVersion = currentVersion.successor

  override def resetToVersion(version: VersionDomain): Unit =
    val keep = VersionSelection(version)
    filter(versionInterval(_) intersects keep.intervalTo)
    map(d =>
      if versionInterval(d).end >= keep.boundary
      then withVersionUpdate(d, _.toTop)
      else d
    )
    setCurrentVersion(version)
    compressAll()

  override def collapseVersionHistory(using versionSelection: VersionSelection): Unit = synchronized:
    filter(versionInterval(_) contains versionSelection.boundary)
    map(d => withVersionUpdate(d, _ => DiscreteInterval1D.intervalFrom(initialVersion)))
    compressAll()
    setCurrentVersion(initialVersion)

  override def flatMap(f: ValidData3D[V, R1, R2, Int] => DataIn2DVersioned[V, R1, R2]): Unit =
    underlying.flatMap(f(_).underlying)

  override def applyDiffActions(diffActions: Iterable[DiffAction3D[V, R1, R2, Int]]): Unit =
    underlying.applyDiffActions(diffActions)

  override def syncWith(that: DataIn2DVersioned[V, R1, R2]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DataIn2DVersionedBase ----------

  override def zip[B](that: DataIn2DVersionedBase[B, R1, R2]): DataIn2DVersioned[(V, B), R1, R2] =
    DataIn2DVersioned(
      underlying.zip(that.getDataIn3D).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](
    that: DataIn2DVersionedBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): DataIn2DVersioned[(V, B), R1, R2] =
    DataIn2DVersioned(
      underlying.zipAll(that.getDataIn3D, thisElem, thatElem).getAll,
      initialVersion,
      Some(currentVersion)
    )
