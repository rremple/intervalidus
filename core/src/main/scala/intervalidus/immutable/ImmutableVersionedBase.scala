package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionSelection}

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Base for all immutable dimensional versioned data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface. Must be [[DomainLike]].
  * @tparam I
  *   the interval type in the public interface. Must be [[IntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type in the public interface. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  * @tparam D2
  *   the domain type for intervals used in the underlying data. Must be [[DomainLike]] and should be one dimension
  *   higher than [[D]], where the last dimension is `Int`.
  * @tparam I2
  *   the interval type of the underlying data. Must be [[IntervalLike]] based on [[D2]].
  * @tparam ValidData2
  *   the valid data type of the underlying data. Must be [[ValidDataLike]] based on [[V]], [[D2]], and [[I2]].
  * @tparam DiffAction2
  *   the diff action type of the underlying data. Must be [[DiffActionLike]] based on [[V]], [[D2]], and [[I2]].
  * @tparam Self
  *   F-bounded self type.
  */
trait ImmutableVersionedBase[
  V,
  D: DomainLike,
  I <: IntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  DiffAction: DiffActionLike,
  D2: DomainLike,
  I2 <: IntervalLike[D2, I2],
  ValidData2 <: ValidDataLike[V, D2, I2, ValidData2],
  DiffAction2: DiffActionLike,
  Self <: ImmutableVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, Self]
  // & DimensionalVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, ?] // bug?
] extends DimensionalVersionedBase[V, D, I, ValidData, DiffAction, D2, I2, ValidData2, DiffAction2, ?]:
  this: Self =>

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
    * Sets the current version. No version history is rewritten, which may cause some unexpected results (especially if
    * the version is set to something from the past). Use with caution.
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
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this is synchronized.
    * @return
    *   a new, updated structure
    */
  def syncWith(that: Self): Self

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

  // ---------- Implemented methods based on the above definitions ----------

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   some new structure if unapproved version was found and approved, None otherwise
    */
  def approve(data: ValidData): Option[Self] =
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
  def approveAll(interval: I): Self =
    val approved = underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
      .map(publicValidData)
      .foldLeft(this): (prev, d) =>
        prev.approve(d).getOrElse(prev)
    approved.underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv unapprovedStartVersion.predecessor) // only related to unapproved removes
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
  def applyDiffActions(diffActions: Iterable[DiffAction2]): Self =
    copyAndModify(_.underlying.applyDiffActions(diffActions))

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid data are compressed.
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
    * Set new valid data. Any data previously valid in this interval and the given version selection context are
    * replaced by this data.
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
  def update(data: ValidData)(using versionSelection: VersionSelection): Self =
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
  def fill(data: ValidData)(using versionSelection: VersionSelection): Self =
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
//  def replace(oldData: ValidData, newData: ValidData)(using versionSelection: VersionSelection): Self =
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
//  def replaceByKey(key: D, newData: ValidData): Self =
//    copyAndModify(_.underlying.replaceByKey(underlyingDomain(key), underlyingValidData(newData)))
