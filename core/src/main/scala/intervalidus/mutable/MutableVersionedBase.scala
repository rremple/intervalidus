package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Base for all mutable dimensional versioned data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be [[DiscreteDomainLike]].
  * @tparam I
  *   the interval type in the public interface. Must be [[DiscreteIntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type in the public interface. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  * @tparam D2
  *   the domain type for intervals used in the underlying data. Must be [[DiscreteDomainLike]] and should be one
  *   dimension higher than [[D]], where the last dimension is `Int`.
  * @tparam I2
  *   the interval type of the underlying data. Must be [[DiscreteIntervalLike]] based on [[D2]].
  * @tparam ValidData2
  *   the valid data type of the underlying data. Must be [[ValidDataLike]] based on [[V]], [[D2]], and [[I2]].
  * @tparam DiffAction2
  *   the diff action type of the underlying data. Must be [[DiffActionLike]] based on [[V]], [[D2]], and [[I2]].
  * @tparam Self
  *   F-bounded self type.
  */
trait MutableVersionedBase[
  V,
  D <: DiscreteDomainLike[D],
  I <: DiscreteIntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  DiffAction <: DiffActionLike[V, D, I, ValidData, DiffAction],
  D2 <: DiscreteDomainLike[D2],
  I2 <: DiscreteIntervalLike[D2, I2],
  ValidData2 <: ValidDataLike[V, D2, I2, ValidData2],
  DiffAction2 <: DiffActionLike[V, D2, I2, ValidData2, DiffAction2],
  Self <: MutableVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, Self] &
    DimensionalVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, ?]
]:
  this: Self =>

  // ---------- To be implemented by inheritor ----------

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data and current version.
    */
  def copy: Self

  /**
    * Sets the current version. Note that no version history is rewritten, which may cause some unexpected results
    * (especially if the version is set to something from the past). Use with caution.
    *
    * @param version
    *   the new current version
    */
  def setCurrentVersion(version: VersionDomain): Unit

  /**
    * Increments the current version.
    */
  def incrementCurrentVersion(): Unit

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    */
  def resetToVersion(version: VersionDomain): Unit

  /**
    * Creates a new structure based on the version selection context, but without any version history.
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): Unit

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: Self): Unit

  // ---------- Implemented methods based on the above definitions ----------

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   true if unapproved version was found and approved, false otherwise
    */
  def approve(data: ValidData): Boolean =
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
  def approveAll(interval: I): Unit =
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
      .map(publicValidData)
      .foreach(approve)
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv unapprovedStartVersion.predecessor) // only related to unapproved removes
      .flatMap(publicValidData(_).interval intersectionWith interval)
      .foreach(remove(_)(using VersionSelection.Current))

  /**
    * Applies a sequence of diff actions to this structure. Does not use a version selection context -- operates on full
    * underlying structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction2]): Unit =
    underlying.applyDiffActions(diffActions)

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid data will be compressed.
    */
  def compress(value: V): Unit = underlying.compress(value)

  /**
    * Compress out adjacent intervals with the same value for all values. (Shouldn't ever need to do this.)
    */
  def compressAll(): Unit = underlying.compressAll()

  /**
    * Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
    * unique physical representation. Does not use a version selection context -- operates on full underlying 2D
    * structure. (Shouldn't ever need to do this.)
    */
  def recompressAll(): Unit = underlying.recompressAll()

  // ---------- Implement methods similar to those in DimensionalBase, but many with version selection ----------

  /**
    * Set new valid data. Note that any data previously valid in this interval and the given version selection context
    * are replaced by this data.
    *
    * @param data
    *   the valid data to set.
    */
  def set(data: ValidData)(using VersionSelection): Unit =
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
  def setIfNoConflict(newData: ValidData)(using VersionSelection): Boolean =
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
  def remove(interval: I)(using versionSelection: VersionSelection): Unit =
    underlying.remove(underlyingIntervalFrom(interval))

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. Note that no new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    * @param versionSelection
    *   version used for updating data -- default is the current version.
    */
  def update(
    data: ValidData
  )(using versionSelection: VersionSelection): Unit =
    underlying.update(underlyingValidData(data))

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * Does not use a version selection context -- the predicate is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData2 => Boolean): Unit = underlying.filter(p)

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    */
  def map(f: ValidData2 => ValidData2): Unit = underlying.map(f)

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it maps all
    * values in all versions. To only map values meeting specific version criteria, use [[map]] instead
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit = underlying.mapValues(f)

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    */
  def flatMap(f: ValidData2 => Self): Unit

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
//  def replace(oldData: ValidData, newData: ValidData)(using versionSelection: VersionSelection): Unit =
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
//  def replaceByKey(key: D, newData: ValidData)(using versionSelection: VersionSelection): Unit =
//    underlying.replaceByKey(underlyingDomain(key), underlyingValidData(newData))
