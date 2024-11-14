package intervalidus.immutable

import intervalidus.*
import intervalidus.mutable.DataIn1DVersioned as DataIn1DVersionedMutable
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
  )(using Experimental): DataIn1DVersioned[V, R] = of(DiscreteInterval1D.unbounded -> value, initialVersion)

  override def from[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]],
    initialVersion: Int = 0 // could use summon[DiscreteValue[Int]].minValue to extend range
  )(using Experimental): DataIn1DVersioned[V, R] = DataIn1DVersioned[V, R](
    initialData.map(d => (d.interval x DiscreteInterval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

/**
  * Interface is similar to [[DataIn1D]], but it operates on an underlying [[mutable.DataIn2D]] using an integer-valued
  * vertical dimension to version data. One use case would be that R = LocalDate, so data values may vary in terms of
  * both version and time. Most methods require some generic version selection criteria rather than specific integer
  * intervals, therefore this does not extend [[DimensionalBase]].
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
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   note that two-dimensional intervals must be disjoint
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
  with ImmutableVersionedBase[
    V,
    DiscreteDomain1D[R],
    DiscreteInterval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R],
    DiscreteDomain2D[R, Int],
    DiscreteInterval2D[R, Int],
    ValidData2D[V, R, Int],
    DiffAction2D[V, R, Int],
    DataIn1DVersioned[V, R]
  ]:

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataIn1DVersioned[V, R] = this

  override def toMutable: DataIn1DVersionedMutable[V, R] = DataIn1DVersionedMutable(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  // ---------- Implement methods from ImmutableVersionedBase ----------

  override protected def copyAndModify(f: DataIn1DVersioned[V, R] => Unit): DataIn1DVersioned[V, R] =
    val result = copy
    f(result)
    result

  override def copy: DataIn1DVersioned[V, R] = DataIn1DVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def setCurrentVersion(version: VersionDomain): DataIn1DVersioned[V, R] =
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv DiscreteDomain1D.Bottom then throw Exception("version too small")
    else copyAndModify(_.currentVersion = version)

  override def incrementCurrentVersion(): DataIn1DVersioned[V, R] =
    if currentVersion.successor equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else copyAndModify(_.currentVersion = currentVersion.successor)

  override def resetToVersion(version: VersionDomain): DataIn1DVersioned[V, R] =
    val keep = VersionSelection(version)
    DataIn1DVersioned(
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

  override def collapseVersionHistory(using VersionSelection): DataIn1DVersioned[V, R] =
    DataIn1DVersioned.from(getAll, initialVersion)

  override def filter(p: ValidData2D[V, R, Int] => Boolean): DataIn1DVersioned[V, R] = DataIn1DVersioned(
    underlying.getAll.filter(p),
    initialVersion,
    Some(currentVersion)
  )

  override def syncWith(that: DataIn1DVersioned[V, R]): DataIn1DVersioned[V, R] =
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

  // --- API methods unique to this "versioned" variant

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def map[B, S: DiscreteValue](f: ValidData2D[V, R, Int] => ValidData2D[B, S, Int]): DataIn1DVersioned[B, S] =
    DataIn1DVersioned(
      underlying.getAll.map(f),
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping. Does
    * not use a version selection context -- the function is applied to the underlying 2D data, so it maps all values in
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
  def mapValues[B](f: V => B): DataIn1DVersioned[B, R] = DataIn1DVersioned(
    underlying.getAll.map(d => d.copy(value = f(d.value))),
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures. Does not use a version selection context -- the function is applied to the underlying 2D
    * data, so it can operate on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure and concatenating the results.
    */
  def flatMap[B, S: DiscreteValue](f: ValidData2D[V, R, Int] => DataIn1DVersioned[B, S]): DataIn1DVersioned[B, S] =
    DataIn1DVersioned(
      underlying.getAll.flatMap(f(_).underlying.getAll),
      initialVersion,
      Some(currentVersion)
    )
