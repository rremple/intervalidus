package intervalidus.immutable

import intervalidus.DimensionalBase
import intervalidus.DimensionalBase.{DataLike, DomainLike, IntervalLike}

/**
  * Base for all immutable dimensional data, both 1D and 2D.
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
trait ImmutableBase[V, D <: DomainLike, I <: IntervalLike[D], ValidData <: DataLike[V, D, I]]:
  self: DimensionalBase[V, D, I, ValidData] =>

  // ---------- To be implemented by inheritor ----------

  /**
    * Selects all elements which satisfy a predicate.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData => Boolean): DimensionalBase[V, D, I, ValidData]

  /**
    * Set new valid data. Note that any data previously valid in this interval are replace by this data.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   a new, updated structure.
    */
  def set(newData: ValidData): DimensionalBase[V, D, I, ValidData]

  /**
    * Set new valid data. Note that any value previously valid in this interval are replace by this value.
    *
    * @param value
    *   the valid data value to set.
    * @param interval
    *   the valid data interval to set.
    * @return
    *   a new, updated structure.
    */
  def set(value: V, interval: I): DimensionalBase[V, D, I, ValidData]

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(newData: ValidData): Option[DimensionalBase[V, D, I, ValidData]]

  /**
    * Set new valid data, but only if there are no data previously valid in this interval.
    *
    * @param value
    *   the valid data value to set.
    * @param interval
    *   the valid data interval to set.
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(value: V, interval: I): Option[DimensionalBase[V, D, I, ValidData]]

  /**
    * Update everything valid in data's interval to have the data's value. Note that no new intervals of validity are
    * added as part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value existing data in the interval should take on
    * @return
    *   a new, updated structure.
    */
  def update(data: ValidData): DimensionalBase[V, D, I, ValidData]

  /**
    * Update everything valid in the specified interval to have the specified value. Note that no new intervals of
    * validity are added as part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param value
    *   the new value existing data should take on.
    * @param interval
    *   the interval in which the update should be applied.
    * @return
    *   a new, updated structure.
    */
  def update(value: V, interval: I): DimensionalBase[V, D, I, ValidData]

  /**
    * Remove valid values on the interval. If there are values valid on portions of the interval, those values have
    * their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @return
    *   a new, updated structure.
    */
  def remove(interval: I): DimensionalBase[V, D, I, ValidData]

  /**
    * Compress out adjacent intervals with the same value
    *
    * @param value
    *   value to be evaluate
    * @return
    *   a new, updated structure.
    */
  def compress(value: V): DimensionalBase[V, D, I, ValidData]

  /**
    * Compress out adjacent intervals with the same value for all values (Shouldn't ever need to do this.)
    *
    * @return
    *   a new, updated structure.
    */
  def compressAll(): DimensionalBase[V, D, I, ValidData]
