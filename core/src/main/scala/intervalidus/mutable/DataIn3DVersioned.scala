package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}
import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.immutable.DataIn3DVersioned as DataIn3DVersionedImmutable

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataIn3DVersioned extends DataIn3DVersionedBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    data: ValidData3D[V, R1, R2, R3],
    initialVersion: Int
  )(using Experimental): DataIn3DVersioned[V, R1, R2, R3] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    value: V,
    initialVersion: Int = 0
  )(using Experimental): DataIn3DVersioned[V, R1, R2, R3] =
    of(Interval3D.unbounded[R1, R2, R3] -> value, initialVersion)

  override def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]],
    initialVersion: Int = 0 // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental): DataIn3DVersioned[V, R1, R2, R3] = DataIn3DVersioned[V, R1, R2, R3](
    initialData.map(d => (d.interval x Interval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

/**
  * @inheritdoc
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
  *   (optional) a collection of valid data in four dimensions (the fourth dimension is the version) to start with --
  *   four-dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
class DataIn3DVersioned[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
  initialData: Iterable[ValidData4D[V, R1, R2, R3, Int]] = Iterable.empty[ValidData4D[V, R1, R2, R3, Int]],
  initialVersion: Int = 0, // could use summon[DomainValueLike[Int]].minValue to extend range
  withCurrentVersion: Option[VersionDomain] = None
)(using Experimental)
  extends DataIn3DVersionedBase[V, R1, R2, R3](initialData, initialVersion, withCurrentVersion)
  with MutableVersionedBase[
    V,
    Domain3D[R1, R2, R3],
    Interval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3],
    DiffAction3D[V, R1, R2, R3],
    Domain4D[R1, R2, R3, Int],
    Interval4D[R1, R2, R3, Int],
    ValidData4D[V, R1, R2, R3, Int],
    DiffAction4D[V, R1, R2, R3, Int],
    DataIn3DVersioned[V, R1, R2, R3]
  ]:

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataIn3DVersionedImmutable[V, R1, R2, R3] = DataIn3DVersionedImmutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def toMutable: DataIn3DVersioned[V, R1, R2, R3] = this

  // ---------- Implement methods from MutableVersionedBase ----------

  override def copy: DataIn3DVersioned[V, R1, R2, R3] = DataIn3DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else currentVersion = version

  override def incrementCurrentVersion(): Unit = synchronized:
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else currentVersion = currentVersion.rightAdjacent

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

  override def flatMap(f: ValidData4D[V, R1, R2, R3, Int] => DataIn3DVersioned[V, R1, R2, R3]): Unit =
    underlying.flatMap(f(_).underlying)

  override def applyDiffActions(diffActions: Iterable[DiffAction4D[V, R1, R2, R3, Int]]): Unit =
    underlying.applyDiffActions(diffActions)

  override def syncWith(that: DataIn3DVersioned[V, R1, R2, R3]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DataIn3DVersionedBase ----------

  override def zip[B](that: DataIn3DVersionedBase[B, R1, R2, R3]): DataIn3DVersioned[(V, B), R1, R2, R3] =
    DataIn3DVersioned(
      underlying.zip(that.getDataIn4D).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](
    that: DataIn3DVersionedBase[B, R1, R2, R3],
    thisElem: V,
    thatElem: B
  ): DataIn3DVersioned[(V, B), R1, R2, R3] =
    DataIn3DVersioned(
      underlying.zipAll(that.getDataIn4D, thisElem, thatElem).getAll,
      initialVersion,
      Some(currentVersion)
    )
