package intervalidus.immutable

import intervalidus.*

/**
  * Immutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type -- [[DomainLike]] non-empty tuples.
  * @tparam Self
  *   F-bounded self-type.
  */
trait ImmutableBase[V, D <: NonEmptyTuple: DomainLike, Self <: ImmutableBase[V, D, Self]](using
  Experimental
) extends DimensionalBase[V, D]:

  protected def copyAndModify(f: Self => Unit): Self =
    val result = copy
    f(result)
    result

  // ---------- To be implemented by inheritor ----------

  override def copy: Self // refine the result type for `copyAndModify`

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  ): DimensionalBase[B, S]

  /**
    * Applies a partial function to all valid data on which it is defined. Both the valid data value and interval types
    * can be changed in the mapping.
    *
    * @param pf
    *   the partial function to apply to each data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure on which it is
    *   defined.
    */
  def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  ): DimensionalBase[B, S]

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def mapValues[B](
    f: V => B
  ): DimensionalBase[B, D]

  /**
    * Applies a function to all valid data intervals. The interval type can be changed in the mapping.
    *
    * @param f
    *   the function to apply to the interval part of each valid data element.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each interval.
    */
  def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): DimensionalBase[V, S]

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    */
  def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  ): DimensionalBase[B, S]

  // ---------- Implement methods not in DimensionalBase that have immutable signatures ----------

  /**
    * Selects all elements that satisfy a predicate.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData[V, D] => Boolean): Self = copyAndModify: result =>
    getAll.filterNot(p).foreach(result.removeValidData)

  /**
    * Set new valid data. Replaces any data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  def set(newData: ValidData[V, D]): Self = copyAndModify: result =>
    result.updateOrRemove(newData.interval, _ => None)
    result.addValidData(newData)
    result.compressInPlace(newData.value)

  /**
    * Set a collection of new valid data. Replaces any data previously valid in this interval.
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    *
    * @param newData
    *   collection of valid data to set.
    * @return
    *   a new, updated structure.
    */
  def setMany(newData: Iterable[ValidData[V, D]]): Self = copyAndModify: result =>
    val values = newData.map(_.value).toSet
    newData.foreach: data =>
      result.updateOrRemove(data.interval, _ => None)
      result.addValidData(data)
    values.foreach: value =>
      result.compressInPlace(value)

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D]): Option[Self] =
    if intersects(newData.interval) then None
    else
      Some(copyAndModify: result =>
        result.addValidData(newData)
        compressInPlace(newData.value)
      )

  /**
    * Update everything valid in data's interval to have the data's value. No new intervals of validity are added as
    * part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value existing data in the interval should take on
    * @return
    *   a new, updated structure.
    */
  def update(data: ValidData[V, D]): Self =
    copyAndModify(_.updateOrRemove(data.interval, _ => Some(data.value)))

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
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Self = copyAndModify: result =>
    result.removeValidData(oldData)
    result.updateOrRemove(newData.interval, _ => None)
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
  def replaceByKey(key: D, newData: ValidData[V, D]): Self =
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
  def remove(interval: Interval[D]): Self =
    copyAndModify(_.updateOrRemove(interval, _ => None))

  /**
    * Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
    * intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    * @return
    *   a new, updated structure.
    */
  def removeMany(intervals: Iterable[Interval[D]]): Self = copyAndModify: result =>
    intervals.foreach: interval =>
      result.updateOrRemove(interval, _ => None)

  /**
    * Remove data in all the intervals where the specified value is valid.
    *
    * @param value
    *   the value that is removed.
    * @return
    *   a new, updated structure.
    */
  def removeValue(value: V): Self = copyAndModify: result =>
    intervals(value).foreach: interval =>
      result.updateOrRemove(interval, _ => None)

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value to be evaluated
    * @return
    *   a new, updated structure.
    */
  def compress(value: V): Self =
    copyAndModify(_.compressInPlace(value))

  /**
    * Compress out adjacent intervals with the same value for all values. (Shouldn't ever need to do this.)
    *
    * @return
    *   a new, updated structure.
    */
  def compressAll(): Self = copyAndModify: result =>
    dataByValue.keySet.foreach(result.compressInPlace)

  /**
    * Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    *
    * @return
    *   a new, updated structure.
    */
  def recompressAll(): Self = copyAndModify(_.recompressInPlace())

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    * @return
    *   a new, updated structure.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, D]]): Self =
    copyAndModify: result =>
      diffActions.foreach:
        case DiffAction.Create(data: ValidData[V, D]) => result.addValidData(data)
        case DiffAction.Update(data: ValidData[V, D]) => result.updateValidData(data)
        case DiffAction.Delete(key)                   => result.removeValidDataByKey(key)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this is synchronized.
    * @return
    *   a new, updated structure.
    */
  def syncWith(that: Self): Self =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    * @return
    *   a new, updated structure.
    */
  def fill(data: ValidData[V, D]): Self = copyAndModify: result =>
    result.fillInPlace(data)

  /**
    * Merges this structure with data from that structure. In intervals where both structures have valid values, the two
    * values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
    * are added (a fill operation).
    *
    * @param that
    *   structure to merge with this one
    * @param mergeValues
    *   function that merges values where both this and that have valid values, where the default merge operation is to
    *   give this data values priority and drop that data values
    * @return
    *   a new, updated structure.
    */
  def merge(
    that: Self,
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Self = copyAndModify: result =>
    result.mergeInPlace(that.getAll, mergeValues)

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * Set new valid data. Replaces any data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  infix def +(newData: ValidData[V, D]): Self = set(newData)

  /**
    * Same as [[setMany]]
    *
    * Set a collection of new valid data. Replaces any data previously valid in this interval.
    *
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    * @param newData
    *   collection of valid data to set.
    * @return
    *   a new, updated structure.
    */
  infix def ++(newData: Iterable[ValidData[V, D]]): Self = setMany(newData)

  /**
    * Same as [[remove]]
    *
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @return
    *   a new, updated structure.
    */
  infix def -(interval: Interval[D]): Self = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
    * intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    * @return
    *   a new, updated structure.
    */
  infix def --(intervals: Iterable[Interval[D]]): Self = removeMany(intervals)
