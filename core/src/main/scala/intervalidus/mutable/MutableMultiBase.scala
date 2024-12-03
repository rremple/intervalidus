package intervalidus.mutable

import intervalidus.*

/**
  * Base for all mutable dimensional multi data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be [[DiscreteDomainLike]].
  * @tparam I
  *   the interval type, based on the domain type. Must be [[DiscreteIntervalLike]] based on [[D]].
  * @tparam ValidDataOne
  *   the valid data type for a single value. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam ValidData
  *   the valid data type for a set of values. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type for a set of values. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  * @tparam Self
  *   F-bounded self type.
  */
trait MutableMultiBase[
  V,
  D <: DiscreteDomainLike[D],
  I <: DiscreteIntervalLike[D, I],
  ValidDataOne <: ValidDataLike[V, D, I, ValidDataOne],
  ValidData <: ValidDataLike[Set[V], D, I, ValidData],
  DiffAction <: DiffActionLike[Set[V], D, I, ValidData, DiffAction],
  Self <: MutableMultiBase[V, D, I, ValidDataOne, ValidData, DiffAction, Self] &
    DimensionalBase[Set[V], D, I, ValidData, DiffAction, ?]
] extends MutableBase[Set[V], D, I, ValidData, DiffAction, Self]
  with DimensionalMultiBase[V, D, I, ValidData, DiffAction]:
  this: Self =>

  /**
    * Merges all valid data in that structure into this one.
    *
    * @param that
    *   the structure which is going to be merged.
    */
  def merge(that: Self): Unit =
    mergeInPlace(that.getAll.map(d => (d.interval, d.value)))

  /**
    * Update everything valid in data's interval to have the data's value. New intervals of validity are added where no
    * data in the interval are valid. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to add
    */
  def addOne(data: ValidDataOne): Unit =
    addOneInPlace(data.interval, data.value)

  /**
    * Remove valid values on the interval. Intervals of validity are removed where only this value is valid. Data with
    * overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to remove
    */
  def removeOne(data: ValidDataOne): Unit =
    removeOneInPlace(data.interval, data.value)

  /**
    * Add all the values following the logic in [[addOne]]
    *
    * @param allData
    *   the data to add
    */
  def addAll(allData: Iterable[ValidDataOne]): Unit =
    allData.foreach(addOne)

  /**
    * Remove all the values following the logic in [[removeOne]].
    *
    * @param allData
    *   the data to remove
    */
  def removeAll(allData: Iterable[ValidDataOne]): Unit =
    allData.foreach(removeOne)
