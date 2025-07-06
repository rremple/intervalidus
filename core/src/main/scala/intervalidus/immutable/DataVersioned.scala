package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{Versioned, VersionDomain, VersionSelection}
import intervalidus.DiscreteValue.IntDiscreteValue

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataVersioned extends DimensionalVersionedBaseObject:
  override def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D],
    initialVersion: Int
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, D <: NonEmptyTuple: DomainLike](
    value: V,
    initialVersion: Int = 0
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] =
    of(Interval.unbounded[D] -> value, initialVersion)

  override def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]],
    initialVersion: Int = 0 // could use summon[DomainValueLike[Int]].minValue to extend range
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] = DataVersioned[V, D](
    initialData.map(d => (d.interval withHead Interval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

/**
  * Base for all immutable dimensional versioned data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be [[DomainLike]].
  */
class DataVersioned[V, D <: NonEmptyTuple: DomainLike](
  initialData: Iterable[ValidData[V, Versioned[D]]] = Iterable.empty[ValidData[V, Versioned[D]]],
  initialVersion: Int = 0, // could use summon[DomainValueLike[Int]].minValue to extend range
  withCurrentVersion: Option[VersionDomain] = None
)(using
  Experimental,
  DomainLike[Versioned[D]]
) extends DimensionalVersionedBase[V, D](initialData, initialVersion, withCurrentVersion):

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def toImmutable: DataVersioned[V, D] = this

  override def toMutable: intervalidus.mutable.DataVersioned[V, D] = intervalidus.mutable.DataVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  protected def copyAndModify(f: DataVersioned[V, D] => Unit): DataVersioned[V, D] =
    val result = copy
    f(result)
    result

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data and current version.
    */
  def copy: DataVersioned[V, D] = DataVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Sets the current version. No version history is rewritten, which may cause some unexpected results (especially if
    * the version is set to something from the past). Use with caution.
    *
    * @param version
    *   the new current version
    * @return
    *   a new structure with the current version set.
    */
  def setCurrentVersion(version: VersionDomain): DataVersioned[V, D] =
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else copyAndModify(_.currentVersion = version)

  /**
    * Increments the current version.
    * @return
    *   a new structure with the current version incremented.
    */
  def incrementCurrentVersion(): DataVersioned[V, D] =
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else copyAndModify(_.currentVersion = currentVersion.rightAdjacent)

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    * @return
    *   a new structure reset to the specified version, with the current version set to the same.
    */
  def resetToVersion(version: VersionDomain): DataVersioned[V, D] =
    val keep = VersionSelection(version)
    DataVersioned(
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

  /**
    * Creates a new structure based on the version selection context, but without any version history.
    * @return
    *   a new structure with the version history collapsed.
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): DataVersioned[V, D] =
    DataVersioned.from(getAll, initialVersion)

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * Does not use a version selection context -- the predicate is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure with the same current version consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData[V, Versioned[D]] => Boolean): DataVersioned[V, D] = DataVersioned(
    underlying.getAll.filter(p),
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this is synchronized.
    * @return
    *   a new, updated structure
    */
  def syncWith(that: DataVersioned[V, D]): DataVersioned[V, D] =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def zip[B](that: DimensionalVersionedBase[B, D]): DataVersioned[(V, B), D] =
    DataVersioned(
      underlying.zip(that.getVersionedData).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](
    that: DimensionalVersionedBase[B, D],
    thisElem: V,
    thatElem: B
  ): DataVersioned[(V, B), D] =
    DataVersioned(
      underlying.zipAll(that.getVersionedData, thisElem, thatElem).getAll,
      initialVersion,
      Some(currentVersion)
    )

  // --- API methods unique to this "versioned" variant

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, Versioned[D]] => ValidData[B, Versioned[S]]
  )(using DomainLike[Versioned[S]]): DataVersioned[B, S] =
    DataVersioned(
      underlying.getAll.map(f),
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping. Does
    * not use a version selection context -- the function is applied to the underlying data, so it maps all values in
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
  def mapValues[B](f: V => B): DataVersioned[B, D] = DataVersioned(
    underlying.getAll.map(d => d.copy(value = f(d.value))),
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures. Does not use a version selection context -- the function is applied to the underlying
    * data, so it can operate on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure and concatenating the results.
    */
  def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, Versioned[D]] => DataVersioned[B, S]
  )(using DomainLike[Versioned[S]]): DataVersioned[B, S] =
    DataVersioned(
      underlying.getAll.flatMap(f(_).underlying.getAll),
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   some new structure if unapproved version was found and approved, None otherwise
    */
  def approve(data: ValidData[V, D]): Option[DataVersioned[V, D]] =
    val allUnapproved = underlying
      .getIntersecting(underlyingIntervalWithVersion(data.interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
    allUnapproved.headOption match
      case Some(d) if publicValidData(d) == data =>
        Some(set(data)(using VersionSelection.Current))
      case _ =>
        None

  /**
    * Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
    *
    * @param interval
    *   interval in which all changes (updates and deletes) are approved
    * @return
    *   a new structure with all unapproved changes approved in the interval.
    */
  def approveAll(interval: Interval[D]): DataVersioned[V, D] =
    val approved = underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
      .map(publicValidData)
      .foldLeft(this): (prev, d) =>
        prev.approve(d).getOrElse(prev)
    approved.underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv unapprovedStartVersion.leftAdjacent) // only related to unapproved removes
      .flatMap(publicValidData(_).interval intersectionWith interval)
      .foldLeft(approved): (prev, i) =>
        prev.remove(i)(using VersionSelection.Current)

  /**
    * Applies a sequence of diff actions to this structure. Does not use a version selection context -- operates on full
    * underlying structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, Versioned[D]]]): DataVersioned[V, D] =
    copyAndModify(_.underlying.applyDiffActions(diffActions))

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid data are compressed.
    * @return
    *   a new, updated structure.
    */
  def compress(value: V): DataVersioned[V, D] = copyAndModify(_.underlying.compress(value))

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    * @return
    *   a new, updated structure.
    */
  def compressAll(): DataVersioned[V, D] = copyAndModify(_.underlying.compressAll())

  /**
    * Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
    * unique physical representation. Does not use a version selection context -- operates on full underlying structure.
    * (Shouldn't ever need to do this.)
    * @return
    *   a new, updated structure.
    */
  def recompressAll(): DataVersioned[V, D] = copyAndModify(_.underlying.recompressAll())

  // ---------- Implement methods similar to those in DimensionalBase, but many with version selection ----------

  /**
    * Set new valid data. Any data previously valid in this interval and the given version selection context are
    * replaced by this data.
    *
    * @param data
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  def set(data: ValidData[V, D])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.set(underlyingValidData(data)))

  /**
    * Set new valid data, but only if there are no previously valid values in its interval and given the version
    * selection context.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   Some new structure data if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D])(using VersionSelection): Option[DataVersioned[V, D]] =
    val result = copy
    val updated = result.underlying.setIfNoConflict(underlyingValidData(newData))
    if updated then Some(result) else None

  /**
    * Remove valid values on the interval and the given version selection context. If there are values valid on portions
    * of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @param versionSelection
    *   version used for removing data -- default is the current version.
    * @return
    *   a new, updated structure.
    */
  def remove(interval: Interval[D])(using versionSelection: VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.remove(underlyingIntervalFrom(interval)))

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. No new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    * @param versionSelection
    *   version used for updating data -- default is the current version.
    * @return
    *   a new, updated structure.
    */
  def update(data: ValidData[V, D])(using versionSelection: VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.update(underlyingValidData(data)))

  /**
    * Given the version selection context, adds a value as valid in portions of the interval where there aren't already
    * valid values.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    * @return
    *   a new, updated structure.
    */
  def fill(data: ValidData[V, D])(using versionSelection: VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.fill(underlyingValidData(data)))

//TODO: these may be problematic/misunderstood in the versioned space, so leaving them out for now.
//  /**
//    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data
//    with
//    * overlaps with the new data interval are adjusted accordingly.
//    *
//    * @param oldData
//    *   the old data to be replaced.
//    * @param newData
//    *   the new data replacing the old data
//    * @param versionSelection
//    *   version used for updating data -- default is the current version.
//    * @return
//    *   a new, updated structure.
//    */
//  def replace(
//    oldData: ValidData[V, D],
//    newData: ValidData[V, D]
//  )(using versionSelection: VersionSelection): DataVersioned[V, D] =
//    copyAndModify(_.underlying.replace(underlyingValidData(oldData), underlyingValidData(newData)))
//
//  /**
//    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data
//    with
//    * overlaps with the new data interval are adjusted accordingly.
//    *
//    * @param key
//    *   key of the old data to be replaced (the interval start).
//    * @param newData
//    *   the new data replacing the old data
//    * @param versionSelection
//    *   version used for updating data -- default is the current version.
//    * @return
//    *   a new, updated structure.
//    */
//  def replaceByKey(key: D, newData: ValidData[V, D]): DataVersioned[V, D] =
//    copyAndModify(_.underlying.replaceByKey(underlyingDomain(key), underlyingValidData(newData)))
