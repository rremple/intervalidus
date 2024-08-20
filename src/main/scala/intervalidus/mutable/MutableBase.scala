package intervalidus.mutable

import intervalidus.DimensionalBase
import intervalidus.DimensionalBase.{DataLike, DomainLike, IntervalLike}

/**
  * Base for all mutable dimensional data, both 1D and 2D.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be `DomainLike` and have an `Ordering`.
  * @tparam I
  *   the interval type, based on the domain type. Must be `IntervalLike` based on D.
  * @tparam ValidData
  *   the valid data type. Must be `DataLike` based on V, D, and I.
  */
trait MutableBase[V, D <: DomainLike, I <: IntervalLike[D], ValidData <: DataLike[V, D, I]]:
  self: DimensionalBase[V, D, I, ValidData] =>

  // ---------- To be implemented by inheritor ----------

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluate
    * @return
    *   this structure once compressed (not a copy)
    */
  def compress(value: V): Unit

  // ---------- Implemented here based on the above (and DimensionalBase) ----------

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData => Boolean): Unit = synchronized:
    getAll
      .filterNot(p)
      .foreach: d =>
        removeValidDataByKey(d.key)
    ()

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def map(f: ValidData => ValidData): Unit = synchronized:
    getAll
      .map(d => d -> f(d))
      .foreach: (oldData, newData) =>
        if oldData.key == newData.key then updateValidData(newData)
        else
          removeValidDataByKey(oldData.key)
          addValidData(newData)

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit =
    getAll
      .map(d => newValidData(f(d.value), d.interval))
      .foreach: newData =>
        updateValidData(newData)

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def flatMap(f: ValidData => DimensionalBase[V, D, I, ValidData]): Unit = synchronized:
    val newData = getAll.flatMap(f(_).getAll)
    dataByStart.clear()
    newData.foreach(addValidData)

  /**
    * Set new valid data. Note that any data previously valid in this interval are replace by this data.
    *
    * @param newData
    *   the valid data to set.
    */
  def set(newData: ValidData): Unit = synchronized:
    remove(newData.interval)
    addValidData(newData)
    compress(newData.value)

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(newData: ValidData): Boolean = synchronized:
    if getIntersecting(newData.interval).isEmpty then
      addValidData(newData)
      compress(newData.value)
      true
    else false

  /**
    * Update everything valid in data's interval to have the data's value. Note that no new intervals of validity are
    * added as part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value existing data in the interval should take on
    */
  def update(data: ValidData): Unit = updateOrRemove(data.interval, Some(data.value))

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param oldData
    *   the old data to be replaced.
    * @param newData
    *   the new data replacing the old data
    */
  def replace(oldData: ValidData, newData: ValidData): Unit =
    removeValidDataByKey(oldData.key)
    set(newData)

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data with
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param key
    *   key of the old data to be replaced (the interval start).
    * @param newData
    *   the new data replacing the old data
    */
  def replace(key: D, newData: ValidData): Unit =
    removeValidDataByKey(key)
    set(newData)

  /**
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  def remove(interval: I): Unit = updateOrRemove(interval, None)

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    *
    * @return
    *   this structure once compressed (not a copy)
    */
  def compressAll(): Unit = synchronized:
    getAll.map(_.value).toSet.foreach(compress)
