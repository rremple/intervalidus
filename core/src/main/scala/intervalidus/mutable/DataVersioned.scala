package intervalidus.mutable

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
  * Base for all mutable dimensional versioned data.
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

  override def toImmutable: intervalidus.immutable.DataVersioned[V, D] = intervalidus.immutable.DataVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def toMutable: DataVersioned[V, D] = this

  // ---------- Implement methods from MutableVersionedBase ----------

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
    */
  def setCurrentVersion(version: VersionDomain): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else currentVersion = version

  /**
    * Increments the current version.
    */
  def incrementCurrentVersion(): Unit = synchronized:
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else currentVersion = currentVersion.rightAdjacent

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    */
  def resetToVersion(version: VersionDomain): Unit =
    val keep = VersionSelection(version)
    filter(versionInterval(_) intersects keep.intervalTo)
    map(d =>
      if versionInterval(d).end >= keep.boundary
      then withVersionUpdate(d, _.toTop)
      else d
    )
    setCurrentVersion(version)
    compressAll()

  /**
    * Creates a new structure based on the version selection context, but without any version history.
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): Unit = synchronized:
    filter(versionInterval(_) contains versionSelection.boundary)
    map(d => withVersionUpdate(d, _ => Interval1D.intervalFrom(initialVersion)))
    compressAll()
    setCurrentVersion(initialVersion)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this is synchronized.
    */
  def syncWith(that: DataVersioned[V, D]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implemented methods based on the above definitions ----------

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   true if unapproved version was found and approved, false otherwise
    */
  def approve(data: ValidData[V, D]): Boolean =
    val allUnapproved = underlying
      .getIntersecting(underlyingIntervalWithVersion(data.interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
    allUnapproved.headOption match
      case Some(d) if publicValidData(d) == data =>
        set(data)(using VersionSelection.Current)
        true
      case other =>
        false

  /**
    * Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
    *
    * @param interval
    *   interval in which all changes (updates and deletes) are approved
    */
  def approveAll(interval: Interval[D]): Unit =
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
      .map(publicValidData)
      .foreach(approve)
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv unapprovedStartVersion.leftAdjacent) // only related to unapproved removes
      .flatMap(publicValidData(_).interval intersectionWith interval)
      .foreach(remove(_)(using VersionSelection.Current))

  /**
    * Applies a sequence of diff actions to this structure. Does not use a version selection context -- operates on full
    * underlying structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, Versioned[D]]]): Unit =
    underlying.applyDiffActions(diffActions)

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid data are compressed.
    */
  def compress(value: V): Unit = underlying.compress(value)

  /**
    * Compress out adjacent intervals with the same value for all values. (Shouldn't ever need to do this.)
    */
  def compressAll(): Unit = underlying.compressAll()

  /**
    * Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
    * unique physical representation. Does not use a version selection context -- operates on full underlying structure.
    * (Shouldn't ever need to do this.)
    */
  def recompressAll(): Unit = underlying.recompressAll()

  // ---------- Implement methods similar to those in DimensionalBase, but many with version selection ----------

  /**
    * Set new valid data. Any data previously valid in this interval and the given version selection context are
    * replaced by this data.
    *
    * @param data
    *   the valid data to set.
    */
  def set(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.set(underlyingValidData(data))

  /**
    * Set new valid data, but only if there are no previously valid values in its interval and given the version
    * selection context.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D])(using VersionSelection): Boolean =
    underlying.setIfNoConflict(underlyingValidData(newData))

  /**
    * Remove valid values on the interval and the given version selection context. If there are values valid on portions
    * of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @param versionSelection
    *   version used for removing data -- default is the current version.
    */
  def remove(interval: Interval[D])(using versionSelection: VersionSelection): Unit =
    underlying.remove(underlyingIntervalFrom(interval))

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. No new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    * @param versionSelection
    *   version used for updating data -- default is the current version.
    */
  def update(data: ValidData[V, D])(using versionSelection: VersionSelection): Unit =
    underlying.update(underlyingValidData(data))

  /**
    * Adds a value as valid in an interval and the given version selection context wherever no values are valid.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    */
  def fill(data: ValidData[V, D])(using versionSelection: VersionSelection): Unit =
    underlying.fill(underlyingValidData(data))

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * Does not use a version selection context -- the predicate is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData[V, Versioned[D]] => Boolean): Unit = underlying.filter(p)

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    */
  def map(f: ValidData[V, Versioned[D]] => ValidData[V, Versioned[D]]): Unit = underlying.map(f)

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it maps all values
    * in all versions. To only map values meeting specific version criteria, use [[map]] instead
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit = underlying.mapValues(f)

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    */
  def flatMap(f: ValidData[V, Versioned[D]] => DataVersioned[V, D]): Unit =
    underlying.flatMap(f(_).underlying)

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
//    */
//  def replace(oldData: ValidData[V, D], newData: ValidData[V, D])(using versionSelection: VersionSelection): Unit =
//    underlying.replace(underlyingValidData(oldData), underlyingValidData(newData))
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
//    */
//  def replaceByKey(key: D, newData: ValidData[V, D])(using versionSelection: VersionSelection): Unit =
//    underlying.replaceByKey(underlyingDomain(key), underlyingValidData(newData))
