package intervalidus.immutable

import intervalidus.*
import intervalidus.mutable.DataIn2DVersioned as DataIn2DVersionedMutable
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}
import intervalidus.DiscreteValue.IntDiscreteValue

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataIn2DVersioned extends DataIn2DVersionedBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike](
    data: ValidData2D[V, R1, R2],
    initialVersion: Int
  )(using Experimental): DataIn2DVersioned[V, R1, R2] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, R1: DomainValueLike, R2: DomainValueLike](
    value: V,
    initialVersion: Int = 0
  )(using Experimental): DataIn2DVersioned[V, R1, R2] =
    of(Interval2D.unbounded[R1, R2] -> value, initialVersion)

  override def from[V, R1: DomainValueLike, R2: DomainValueLike](
    initialData: Iterable[ValidData2D[V, R1, R2]],
    initialVersion: Int = 0 // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental): DataIn2DVersioned[V, R1, R2] = DataIn2DVersioned[V, R1, R2](
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
  * @param initialData
  *   (optional) a collection of valid data in three dimensions (the depth dimension is the version) to start with --
  *   three-dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
class DataIn2DVersioned[V, R1: DomainValueLike, R2: DomainValueLike](
  initialData: Iterable[ValidData3D[V, R1, R2, Int]] = Iterable.empty[ValidData3D[V, R1, R2, Int]],
  initialVersion: Int = 0, // could use summon[DomainValueLike[Int]].minValue to extend range
  withCurrentVersion: Option[VersionDomain] = None
)(using Experimental)
  extends DataIn2DVersionedBase[V, R1, R2](initialData, initialVersion, withCurrentVersion)
  with ImmutableVersionedBase[
    V,
    Domain2D[R1, R2],
    Interval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2],
    Domain3D[R1, R2, Int],
    Interval3D[R1, R2, Int],
    ValidData3D[V, R1, R2, Int],
    DiffAction3D[V, R1, R2, Int],
    DataIn2DVersioned[V, R1, R2]
  ]:

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataIn2DVersioned[V, R1, R2] = this

  override def toMutable: DataIn2DVersionedMutable[V, R1, R2] = DataIn2DVersionedMutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  // ---------- Implement methods from ImmutableVersionedBase ----------

  override protected def copyAndModify(f: DataIn2DVersioned[V, R1, R2] => Unit): DataIn2DVersioned[V, R1, R2] =
    val result = copy
    f(result)
    result

  override def copy: DataIn2DVersioned[V, R1, R2] = DataIn2DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): DataIn2DVersioned[V, R1, R2] =
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else copyAndModify(_.currentVersion = version)

  override def incrementCurrentVersion(): DataIn2DVersioned[V, R1, R2] =
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else copyAndModify(_.currentVersion = currentVersion.rightAdjacent)

  override def resetToVersion(version: VersionDomain): DataIn2DVersioned[V, R1, R2] =
    val keep = VersionSelection(version)
    DataIn2DVersioned(
      underlying.getAll
        .filter(versionInterval(_) intersects keep.intervalTo)
        .map(d =>
          if versionInterval(d).end >= keep.boundary
          then withVersionUpdate(d, _.toTop)
          else d
        ),
      initialVersion,
      Some(version)
    ).compressAll()

  override def collapseVersionHistory(using VersionSelection): DataIn2DVersioned[V, R1, R2] =
    DataIn2DVersioned.from(getAll, initialVersion)

  override def filter(p: ValidData3D[V, R1, R2, Int] => Boolean): DataIn2DVersioned[V, R1, R2] = DataIn2DVersioned(
    underlying.getAll.filter(p),
    initialVersion,
    Some(currentVersion)
  )

  override def syncWith(that: DataIn2DVersioned[V, R1, R2]): DataIn2DVersioned[V, R1, R2] =
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

  // --- API methods unique to this "versioned" variant

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    * Does not use a version selection context -- the function is applied to the underlying 3D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S1
    *   the valid data horizontal interval type of the returned structure.
    * @tparam S2
    *   the valid data vertical interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def map[B, S1: DomainValueLike, S2: DomainValueLike](
    f: ValidData3D[V, R1, R2, Int] => ValidData3D[B, S1, S2, Int]
  ): DataIn2DVersioned[B, S1, S2] =
    DataIn2DVersioned(
      underlying.getAll.map(f),
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping. Does
    * not use a version selection context -- the function is applied to the underlying 3D data, so it maps all values in
    * all versions. To only map values meeting specific version criteria, use [[map]] instead.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def mapValues[B](f: V => B): DataIn2DVersioned[B, R1, R2] = DataIn2DVersioned(
    underlying.getAll.map(d => d.copy(value = f(d.value))),
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures. Does not use a version selection context -- the function is applied to the underlying 3D
    * data, so it can operate on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S1
    *   the valid data horizontal interval type of the returned structure.
    * @tparam S2
    *   the valid data vertical interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure and concatenating the results.
    */
  def flatMap[B, S1: DomainValueLike, S2: DomainValueLike](
    f: ValidData3D[V, R1, R2, Int] => DataIn2DVersioned[B, S1, S2]
  ): DataIn2DVersioned[B, S1, S2] =
    DataIn2DVersioned(
      underlying.getAll.flatMap(f(_).underlying.getAll),
      initialVersion,
      Some(currentVersion)
    )
