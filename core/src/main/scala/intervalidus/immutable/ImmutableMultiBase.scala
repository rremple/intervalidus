package intervalidus.immutable

import intervalidus.*

/**
  * Base for all immutable dimensional multi data.
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
trait ImmutableMultiBase[
  V,
  D: DiscreteDomainLike,
  I <: DiscreteIntervalLike[D, I],
  ValidDataOne <: ValidDataLike[V, D, I, ValidDataOne],
  ValidData <: ValidDataLike[Set[V], D, I, ValidData],
  DiffAction: DiffActionLike,
  Self <: ImmutableMultiBase[V, D, I, ValidDataOne, ValidData, DiffAction, Self]
] extends ImmutableBase[Set[V], D, I, ValidData, DiffAction, Self]
  with DimensionalMultiBase[V, D, I, ValidData, DiffAction]:
  this: Self =>

  /**
    * Merges all valid data in that structure into this one.
    *
    * @param that
    *   the structure which is going to be merged.
    * @return
    *   a new, updated structure.
    */
  def merge(that: Self): Self = copyAndModify: result =>
    result.mergeInPlace(that.getAll.map(d => (d.interval, d.value)))

  /**
    * Update everything valid in data's interval to have the data's value. New intervals of validity are added where no
    * data in the interval are valid. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to add
    * @return
    *   a new, updated structure
    */
  def addOne(data: ValidDataOne): Self =
    copyAndModify(_.addOneInPlace(data.interval, data.value))

  /**
    * Remove valid values on the interval. Intervals of validity are removed where only this value is valid. Data with
    * overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to remove
    * @return
    *   a new, updated structure
    */
  def removeOne(data: ValidDataOne): Self =
    copyAndModify(_.removeOneInPlace(data.interval, data.value))

  /**
    * Add all the values following the logic in [[addOne]].
    *
    * @param allData
    *   the data to add
    * @return
    *   a new, updated structure
    */
  def addAll(allData: Iterable[ValidDataOne]): Self = copyAndModify: result =>
    allData.foreach(d => result.addOneInPlace(d.interval, d.value))

  /**
    * Remove all the values following the logic in [[removeOne]].
    *
    * @param allData
    *   the data to remove
    * @return
    *   a new, updated structure
    */
  def removeAll(allData: Iterable[ValidDataOne]): Self = copyAndModify: result =>
    allData.foreach(d => result.removeOneInPlace(d.interval, d.value))
