package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}

/**
  * Base for all immutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be `DomainLike` and have an `Ordering`.
  * @tparam I
  *   the interval type in the public interface. Must be `IntervalLike` based on [[D]].
  * @tparam ValidData
  *   the valid data type in the public interface. Must be `DataLike` based on [[V]], [[D]], and [[I]].
  * @tparam D2
  *   the domain type for intervals used in the underlying data. Must be `DomainLike` and have an `Ordering`, and should
  *   be one dimension higher than [[D]], where the last dimension is `Int`.
  * @tparam I2
  *   the interval type of the underlying data. Must be `IntervalLike` based on [[D2]], and should be one dimension
  *   higher than [[I]], where the last dimension is `Int`.
  * @tparam ValidData2
  *   the valid data type of the underlying data. Must be `DataLike` based on [[V]], [[D2]], and [[I2]].
  * @tparam Self
  *   F-bounded self type.
  */
trait ImmutableVersionedBase[
  V,
  D <: DiscreteDomainLike[D],
  I <: DiscreteIntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  DiffAction <: DiffActionLike[V, D, I, ValidData, DiffAction],
  D2 <: DiscreteDomainLike[D2]: Ordering,
  I2 <: DiscreteIntervalLike[D2, I2],
  ValidData2 <: ValidDataLike[V, D2, I2, ValidData2],
  DiffAction2 <: DiffActionLike[V, D2, I2, ValidData2, DiffAction2],
  Self <: ImmutableVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, Self]
] extends DimensionalVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, _]:

  // ---------- To be implemented by inheritor ----------

  protected def copyAndModify(f: Self => Unit): Self

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
    * @return
    *   a new structure with the current version set.
    */
  def setCurrentVersion(version: VersionDomain): Self

  /**
    * Increments the current version.
    * @return
    *   a new structure with the current version incremented.
    */
  def incrementCurrentVersion(): Self

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    * @return
    *   a new structure reset to the specified version, with the current version set to the same.
    */
  def resetToVersion(version: VersionDomain): Self

  /**
    * Creates a new structure based on the version selection context, but without any version history.
    * @return
    *   a new structure with the version history collapsed.
    */
  def collapseVersionHistory(using VersionSelection): Self

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   some new structure if unapproved version was found and approved, none otherwise
    */
  def approve(data: ValidData): Option[Self]

  /**
    * Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
    *
    * @param interval
    *   interval in which all changes (updates and deletes) are approved
    * @return
    *   a new structure with all unapproved changes approved.
    */
  def approveAll(interval: I): Self

  /**
    * Selects all elements which satisfy a predicate. Does not use a version selection context -- the predicate is
    * applied to the underlying data, so it can operate on the underlying version information as well as the valid
    * interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure with the same current version consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData2 => Boolean): Self

  /**
    * Applies a sequence of diff actions to this structure. Does not use a version selection context -- operates on full
    * underlying structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction2]): Self

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: Self): Self

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluate
    * @return
    *   a new, updated structure.
    */
  def compress(value: V): Self = copyAndModify(_.underlying.compress(value))

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    * @return
    *   a new, updated structure.
    */
  def compressAll(): Self = copyAndModify(_.underlying.compressAll())

  /**
    * Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
    * unique physical representation. Does not use a version selection context -- operates on full underlying 2D
    * structure. (Shouldn't ever need to do this.)
    * @return
    *   a new, updated structure.
    */
  def recompressAll(): Self = copyAndModify(_.underlying.recompressAll())

  // ---------- Implement methods similar to those in DimensionalBase, but with version selection ----------

  /**
    * Set new valid data. Note that any data previously valid in this interval and the given version selection context
    * are replaced by this data.
    *
    * @param data
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  def set(data: ValidData)(using VersionSelection): Self =
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
  def setIfNoConflict(newData: ValidData)(using VersionSelection): Option[Self] =
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
  def remove(interval: I)(using versionSelection: VersionSelection): Self =
    copyAndModify(_.underlying.remove(underlyingIntervalFrom(interval)))

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. Note that no new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    * @param versionSelection
    *   version used for updating data -- default is the current version.
    * @return
    *   a new, updated structure.
    */
  def update(
    data: ValidData
  )(using versionSelection: VersionSelection): Self =
    copyAndModify(_.underlying.update(underlyingValidData(data)))

//TODO: these may be problematic/misunderstood in the versioned space, so leaving them out for now.
//  /**
//    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
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
//  def replace(oldData: ValidData, newData: ValidData)(using versionSelection: VersionSelection): Self =
//    copyAndModify(_.underlying.replace(underlyingValidData(oldData), underlyingValidData(newData)))
//
//  /**
//    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
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
//  def replaceByKey(key: D, newData: ValidData): Self =
//    copyAndModify(_.underlying.replaceByKey(underlyingDomain(key), underlyingValidData(newData)))
