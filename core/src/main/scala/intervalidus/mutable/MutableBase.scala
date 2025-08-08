package intervalidus.mutable

import intervalidus.*

/**
  * Mutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type -- [[DomainLike]] non-empty tuples.
  */
trait MutableBase[V, D <: NonEmptyTuple: DomainLike](using Experimental) extends DimensionalBase[V, D]:

  // ---------- Implement methods not in DimensionalBase that have mutable signatures ----------

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * @param f
    *   the function to apply to each valid data element.
    */
  def map(f: ValidData[V, D] => ValidData[V, D]): Unit = synchronized:
    replaceValidData(getAll.map(f))

  /**
    * Applies a partial function to all valid data on which it is defined. Data are mutated in place.
    *
    * @param pf
    *   the partial function to apply to each data element.
    */
  def collect(pf: PartialFunction[ValidData[V, D], ValidData[V, D]]): Unit = synchronized:
    replaceValidData(getAll.collect(pf))

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit =
    getAll
      .map(d => d.copy(value = f(d.value)))
      .foreach: newData =>
        updateValidData(newData)

  /**
    * Applies a function to all valid data intervals. Data are mutated in place.
    *
    * @param f
    *   the function to apply to the interval part of each valid data element.
    */
  def mapIntervals(f: Interval[D] => Interval[D]): Unit = synchronized:
    replaceValidData(getAll.map(d => d.copy(interval = f(d.interval))))

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    */
  def flatMap(f: ValidData[V, D] => DimensionalBase[V, D]): Unit = synchronized:
    replaceValidData(getAll.flatMap(f(_).getAll))

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData[V, D] => Boolean): Unit = synchronized:
    replaceValidData(getAll.filter(p))

  /**
    * Set new valid data. Replaces any data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    */
  def set(newData: ValidData[V, D]): Unit = synchronized:
    remove(newData.interval)
    addValidData(newData)
    compress(newData.value)

  /**
    * Set a collection of new valid data. Replaces any data previously valid in this interval.
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    *
    * @param newData
    *   collection of valid data to set.
    */
  def setMany(newData: Iterable[ValidData[V, D]]): Unit = synchronized:
    val values = newData.map(_.value).toSet
    newData.foreach: data =>
      updateOrRemove(data.interval, _ => None)
      addValidData(data)
    values.foreach: value =>
      compressInPlace(value)

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D]): Boolean = synchronized:
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
  def update(data: ValidData[V, D]): Unit =
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
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit =
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
  def replaceByKey(key: D, newData: ValidData[V, D]): Unit =
    replace(dataByStartAsc(key), newData)

  /**
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  def remove(interval: Interval[D]): Unit = updateOrRemove(interval, _ => None)

  /**
    * Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
    * intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    */
  def removeMany(intervals: Iterable[Interval[D]]): Unit =
    intervals.foreach: interval =>
      updateOrRemove(interval, _ => None)

  /**
    * Remove the value in all the intervals where it is valid.
    *
    * @param value
    *   the value that is removed.
    */
  def removeValue(value: V): Unit =
    intervals(value).foreach: interval =>
      updateOrRemove(interval, _ => None)

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluated
    */
  def compress(value: V): Unit = synchronized:
    compressInPlace(value)

  /**
    * Compress out adjacent intervals with the same value for all values.
    */
  def compressAll(): Unit = synchronized:
    dataByValue.keySet.foreach(compress)

  /**
    * Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  def recompressAll(): Unit =
    recompressInPlace()

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, D]]): Unit = synchronized:
    diffActions.foreach(applyDiffActionInPlace)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this is synchronized.
    */
  def syncWith(that: DimensionalBase[V, D]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    */
  def fill(data: ValidData[V, D]): Unit = synchronized:
    fillInPlace(data)

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
    that: DimensionalBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = mergeInPlace(that.getAll, mergeValues)

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * Set new valid data. Replaces any data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    */
  infix def +(newData: ValidData[V, D]): Unit = set(newData)

  /**
    * Same as [[setMany]]
    *
    * Set a collection of new valid data. Replaces any data previously valid in this interval.
    *
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    * @param newData
    *   collection of valid data to set.
    */
  infix def ++(newData: Iterable[ValidData[V, D]]): Unit = setMany(newData)

  /**
    * Same as [[remove]]
    *
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  infix def -(interval: Interval[D]): Unit = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
    * intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    */
  infix def --(intervals: Iterable[Interval[D]]): Unit = removeMany(intervals)
