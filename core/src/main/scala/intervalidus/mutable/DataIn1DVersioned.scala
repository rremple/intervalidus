package intervalidus.mutable

import intervalidus.*
import intervalidus.immutable.DataIn1DVersioned as DataIn1DVersionedImmutable
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataIn1DVersioned extends DataIn1DVersionedBaseObject:
  override def of[V, R: DiscreteValue](
    data: ValidData1D[V, R],
    initialVersion: Int
  )(using Experimental): DataIn1DVersioned[V, R] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, R: DiscreteValue](
    value: V,
    initialVersion: Int = 0
  )(using Experimental): DataIn1DVersioned[V, R] = of(Interval1D.unbounded -> value, initialVersion)

  override def from[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]],
    initialVersion: Int = 0 // could use summon[DiscreteValue[Int]].minValue to extend range
  )(using Experimental): DataIn1DVersioned[V, R] = DataIn1DVersioned[V, R](
    initialData.map(d => (d.interval x Interval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

/**
  * @inheritdoc
  *
  * @tparam V
  *   the type of the value managed as data
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   two-dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
class DataIn1DVersioned[V, R: DiscreteValue](
  initialData: Iterable[ValidData2D[V, R, Int]] = Iterable.empty[ValidData2D[V, R, Int]],
  initialVersion: Int = 0, // could use summon[DiscreteValue[Int]].minValue to extend range
  withCurrentVersion: Option[VersionDomain] = None
)(using Experimental)
  extends DataIn1DVersionedBase[V, R](initialData, initialVersion, withCurrentVersion)
  with MutableVersionedBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R],
    Domain2D[R, Int],
    Interval2D[R, Int],
    ValidData2D[V, R, Int],
    DiffAction2D[V, R, Int],
    DataIn1DVersioned[V, R]
  ]:

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataIn1DVersionedImmutable[V, R] = DataIn1DVersionedImmutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def toMutable: DataIn1DVersioned[V, R] = this

  // ---------- Implement methods from MutableVersionedBase ----------

  override def copy: DataIn1DVersioned[V, R] = DataIn1DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
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
    map(d => withVersionUpdate(d, _ => Interval1D.intervalFrom(initialVersion)))
    compressAll()
    setCurrentVersion(initialVersion)

  override def flatMap(f: ValidData2D[V, R, Int] => DataIn1DVersioned[V, R]): Unit =
    underlying.flatMap(f(_).underlying)

  override def syncWith(that: DataIn1DVersioned[V, R]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DataIn1DVersionedBase ----------

  override def zip[B](that: DataIn1DVersionedBase[B, R]): DataIn1DVersioned[(V, B), R] =
    DataIn1DVersioned(
      underlying.zip(that.getDataIn2D).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](that: DataIn1DVersionedBase[B, R], thisElem: V, thatElem: B): DataIn1DVersioned[(V, B), R] =
    DataIn1DVersioned(
      underlying.zipAll(that.getDataIn2D, thisElem, thatElem).getAll,
      initialVersion,
      Some(currentVersion)
    )
