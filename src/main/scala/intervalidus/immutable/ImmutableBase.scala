package intervalidus.immutable

import intervalidus.DimensionalBase
import intervalidus.DimensionalBase.{DataLike, DomainLike, IntervalLike}

/**
  * Base for all immutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be `DomainLike` and have an `Ordering`.
  * @tparam I
  *   the interval type, based on the domain type. Must be `IntervalLike` based on D.
  * @tparam ValidData
  *   the valid data type. Must be `DataLike` based on V, D, and I.
  * @tparam Self
  *   F-bounded self type.
  */
trait ImmutableBase[
  V,
  D <: DomainLike[D],
  I <: IntervalLike[D, I],
  ValidData <: DataLike[V, D, I, ValidData],
  Self <: ImmutableBase[V, D, I, ValidData, Self]
] extends DimensionalBase[V, D, I, ValidData, _]:
  // Unlike MutableBase, we have to extend DimensionalBase here rather than reference it as the self type because
  // the results of copyAndModify operations need access to protected methods in that trait

  // ---------- To be implemented by inheritor ----------

  protected def copyAndModify(f: Self => Unit): Self

  /**
    * Unlike in 1D, there is no unique compression in 2D and 3D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could
    * also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  def recompressAll(): Self = copyAndModify(_.recompressInPlace())

  // ---------- Implement methods from DimensionalBase ----------

  /**
    * Selects all elements which satisfy a predicate.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData => Boolean): Self = copyAndModify: result =>
    getAll.filterNot(p).foreach(result.removeValidData)

  /**
    * Set new valid data. Note that any data previously valid in this interval are replace by this data.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  def set(newData: ValidData): Self = copyAndModify: result =>
    result.updateOrRemove(newData.interval, None)
    result.addValidData(newData)
    result.compressInPlace(newData.value)

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(newData: ValidData): Option[Self] =
    if intersects(newData.interval) then None
    else
      Some(copyAndModify: result =>
        result.addValidData(newData)
        compressInPlace(newData.value)
      )

  /**
    * Update everything valid in data's interval to have the data's value. Note that no new intervals of validity are
    * added as part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value existing data in the interval should take on
    * @return
    *   a new, updated structure.
    */
  def update(data: ValidData): Self =
    copyAndModify(_.updateOrRemove(data.interval, Some(data.value)))

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param oldData
    *   the old data to be replaced.
    * @param newData
    *   the new data replacing the old data
    * @return
    *   a new, updated structure.
    */
  def replace(oldData: ValidData, newData: ValidData): Self = copyAndModify: result =>
    result.removeValidData(oldData)
    result.updateOrRemove(newData.interval, None)
    result.addValidData(newData)
    result.compressInPlace(newData.value)

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param key
    *   key of the old data to be replaced (the interval start).
    * @param newData
    *   the new data replacing the old data
    * @return
    *   a new, updated structure.
    */
  def replaceByKey(key: D, newData: ValidData): Self =
    replace(dataByStartAsc(key), newData)

  /**
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @return
    *   a new, updated structure.
    */
  def remove(interval: I): Self =
    copyAndModify(_.updateOrRemove(interval, None))

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluate
    * @return
    *   a new, updated structure.
    */
  def compress(value: V): Self =
    copyAndModify(_.compressInPlace(value))

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    *
    * @return
    *   a new, updated structure.
    */
  def compressAll(): Self = copyAndModify: result =>
    dataByValue.keySet.foreach(result.compressInPlace)