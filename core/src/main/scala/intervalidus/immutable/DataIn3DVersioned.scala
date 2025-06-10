package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}
import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.mutable.DataIn3DVersioned as DataIn3DVersionedMutable

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
  with ImmutableVersionedBase[
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

  override def toImmutable: DataIn3DVersioned[V, R1, R2, R3] = this

  override def toMutable: DataIn3DVersionedMutable[V, R1, R2, R3] = DataIn3DVersionedMutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

// ---------- Implement methods from ImmutableVersionedBase ----------

  override protected def copyAndModify(f: DataIn3DVersioned[V, R1, R2, R3] => Unit): DataIn3DVersioned[V, R1, R2, R3] =
    val result = copy
    f(result)
    result

  override def copy: DataIn3DVersioned[V, R1, R2, R3] = DataIn3DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): DataIn3DVersioned[V, R1, R2, R3] =
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else copyAndModify(_.currentVersion = version)

  override def incrementCurrentVersion(): DataIn3DVersioned[V, R1, R2, R3] =
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else copyAndModify(_.currentVersion = currentVersion.rightAdjacent)

  override def resetToVersion(version: VersionDomain): DataIn3DVersioned[V, R1, R2, R3] =
    val keep = VersionSelection(version)
    DataIn3DVersioned(
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

  override def collapseVersionHistory(using VersionSelection): DataIn3DVersioned[V, R1, R2, R3] =
    DataIn3DVersioned.from(getAll, initialVersion)

  override def filter(p: ValidData4D[V, R1, R2, R3, Int] => Boolean): DataIn3DVersioned[V, R1, R2, R3] =
    DataIn3DVersioned(
      underlying.getAll.filter(p),
      initialVersion,
      Some(currentVersion)
    )

  override def syncWith(that: DataIn3DVersioned[V, R1, R2, R3]): DataIn3DVersioned[V, R1, R2, R3] =
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

  // --- API methods unique to this "versioned" variant

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    * Does not use a version selection context -- the function is applied to the underlying 4D data, so it can operate
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
    * @tparam S3
    *   the valid data depth interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def map[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike](
    f: ValidData4D[V, R1, R2, R3, Int] => ValidData4D[B, S1, S2, S3, Int]
  ): DataIn3DVersioned[B, S1, S2, S3] =
    DataIn3DVersioned(
      underlying.getAll.map(f),
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping. Does
    * not use a version selection context -- the function is applied to the underlying 4D data, so it maps all values in
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
  def mapValues[B](f: V => B): DataIn3DVersioned[B, R1, R2, R3] = DataIn3DVersioned(
    underlying.getAll.map(d => d.copy(value = f(d.value))),
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures. Does not use a version selection context -- the function is applied to the underlying 4D
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
    * @tparam S3
    *   the valid data depth interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure and concatenating the results.
    */
  def flatMap[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike](
    f: ValidData4D[V, R1, R2, R3, Int] => DataIn3DVersioned[B, S1, S2, S3]
  ): DataIn3DVersioned[B, S1, S2, S3] =
    DataIn3DVersioned(
      underlying.getAll.flatMap(f(_).underlying.getAll),
      initialVersion,
      Some(currentVersion)
    )
