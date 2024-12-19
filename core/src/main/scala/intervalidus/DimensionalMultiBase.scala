package intervalidus

/**
  * Base for all dimensional multi data, both mutable and immutable, in 1D, 2D, and 3D. Extends [[DimensionalBase]]
  * where the valid value type is a set of values rather than a single value.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be [[DiscreteDomainLike]] and have an [[Ordering]].
  * @tparam I
  *   the interval type, based on the domain type. Must be [[DiscreteIntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type for a set of values. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type for a set of values. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  */
trait DimensionalMultiBase[
  V,
  D: DiscreteDomainLike,
  I <: DiscreteIntervalLike[D, I],
  ValidData <: ValidDataLike[Set[V], D, I, ValidData],
  DiffAction: DiffActionLike
]:
  this: DimensionalBase[Set[V], D, I, ValidData, DiffAction, ?] =>

  /**
    * Updates all the valid value sets in the interval to include the new value. Fills any remaining portions of the
    * interval without any valid values to just have the new value as valid.
    *
    * @param interval
    *   new value will be valid in this interval (along with other existing values)
    * @param value
    *   the new value to make valid in the interval
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def addOneInPlace[B <: V](interval: I, value: B): Unit =
    updateOrRemove(interval, existingValues => Some(existingValues + value))
    fillInPlace(interval, Set(value))

  /**
    * Updates all the valid value sets in the interval to exclude the new value. For any interval that only contains the
    * new value, remove the interval completely.
    *
    * @param interval
    *   new value will no longer be valid in this interval
    * @param value
    *   the new value to make no longer valid in the interval
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def removeOneInPlace[B <: V](interval: I, value: B): Unit =
    updateOrRemove(
      interval,
      existingValues =>
        val newValues = existingValues - value
        if newValues.isEmpty then None else Some(newValues)
    )

  /**
    * Does an element-wise merge of this structure with that data.
    * @param thatData
    *   data to be merged
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def mergeInPlace[B <: V](thatData: Iterable[(I, Set[B])]): Unit =
    thatData.foreach: (interval, thoseValues) =>
      thoseValues.foreach: b =>
        addOneInPlace(interval, b)
