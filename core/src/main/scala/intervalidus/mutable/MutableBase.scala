package intervalidus.mutable

import intervalidus.*

/**
  * Base for all mutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be [[DiscreteDomainLike]].
  * @tparam I
  *   the interval type, based on the domain type. Must be [[DiscreteIntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  * @tparam Self
  *   F-bounded self type.
  */
trait MutableBase[
  V,
  D: DiscreteDomainLike,
  I <: DiscreteIntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  DiffAction: DiffActionLike,
  Self <: MutableBase[V, D, I, ValidData, DiffAction, Self] & DimensionalBase[V, D, I, ValidData, DiffAction, ?]
]:
  this: Self =>

  // ---------- To be implemented by inheritor ----------

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction]): Unit

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this is synchronized.
    */
  def syncWith(that: Self): Unit

  // ---------- Implement methods from DimensionalBase ----------

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData => Boolean): Unit = synchronized:
    replaceValidData(getAll.filter(p))

  /**
    * Set new valid data. Any data previously valid in this interval are replace by this data.
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
    * Update everything valid in data's interval to have the data's value. No new intervals of validity are added as
    * part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value existing data in the interval should take on
    */
  def update(data: ValidData): Unit =
    updateOrRemove(data.interval, _ => Some(data.value))

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
    removeValidData(oldData)
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
  def replaceByKey(key: D, newData: ValidData): Unit =
    replace(dataByStartAsc(key), newData)

  /**
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  def remove(interval: I): Unit = updateOrRemove(interval, _ => None)

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluated
    * @return
    *   this structure once compressed (not a copy)
    */
  def compress(value: V): Unit = synchronized:
    compressInPlace(value)

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    *
    * @return
    *   this structure once compressed (not a copy)
    */
  def compressAll(): Unit = synchronized:
    dataByValue.keySet.foreach(compress)

  /**
    * Unlike in 1D, there is no unique compression in 2D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could also be
    * represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  def recompressAll(): Unit = synchronized:
    recompressInPlace()

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * @param f
    *   the function to apply to each valid data element.
    */
  def map(f: ValidData => ValidData): Unit = synchronized:
    replaceValidData(getAll.map(f))

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
    */
  def flatMap(f: ValidData => Self): Unit = synchronized:
    replaceValidData(getAll.flatMap(f(_).getAll))

  /**
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    */
  def fill(data: ValidData): Unit = synchronized:
    fillInPlace(data.interval, data.value)

  /**
    * Merges this structure with data from that structure. In intervals where both structures have valid values, the two
    * values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
    * are added (a fill operation).
    *
    * @param that
    *   structure to merge into this one
    * @param mergeValues
    *   function that merges values where both this and that have valid values, where the default merge operation is to
    *   give this data values priority and drop that data values
    */
  def merge(
    that: Self,
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = mergeInPlace(that.getAll, mergeValues)
