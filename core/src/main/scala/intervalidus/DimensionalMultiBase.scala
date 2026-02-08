package intervalidus

import scala.collection.mutable

/**
  * Common definitions used in all dimensional multivalued data.
  */
object DimensionalMultiBase:
  type In1D[V, R1] = DimensionalMultiBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalMultiBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalMultiBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalMultiBase[V, Domain.In4D[R1, R2, R3, R4]]

/**
  * Constructs multivalued data in multidimensional intervals.
  */
trait DimensionalMultiBaseObject extends DimensionalBaseConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param data
    *   value valid within an interval.
    * @return
    *   [[DimensionalMultiBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param value
    *   value that is valid in the full domain (`Interval.unbounded[D]`).
    * @return
    *   [[DimensionalMultiBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of values valid within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalMultiBase]] structure with zero or more valid values.
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Creates a muti-value structure from a non-multi structure managing sets of values.
    *
    * @param that
    *   dimensional data of value sets valid within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalMultiBase]] structure with the same valid values as that data structure.
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    that: DimensionalBase[Set[V], D]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Constructor for multiple (or no) initial value sets that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data sets within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalMultiBase]] structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[Set[V], D]]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], DimensionalMultiBase[V, D]]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  *
  * @define classDesc
  *   Data may have multiple values valid in different intervals of arbitrary dimensions, conceptually similar to a
  *   multimap. When queried, values are returned as a set. In addition to the standard methods that operate on these
  *   sets of values. there are methods that operate on individual set members. For example, `addOne` and `removeOne`
  *   allow mutation of individual values across intervals, and `mergeOne` combines two structures (a merge where
  *   overlaps are concatenated).
  * @define mergeOneDesc
  *   Concatenates all valid data in this and that structure into a new one.
  * @define mergeOneParamThat
  *   the structure which is going to be concatenated.
  * @define addOneDesc
  *   Update everything valid in data's interval to have the data's value. New intervals of validity are added where no
  *   data in the interval are valid. Data with overlaps are adjusted accordingly.
  * @define addOneParamData
  *   the data to add
  * @define removeOneDesc
  *   Remove valid values on the interval. Intervals of validity are removed where only this value is valid. Data with
  *   overlaps are adjusted accordingly.
  * @define removeOneParamData
  *   the data to remove
  * @define addOneManyDesc
  *   Add all the values following the logic in
  * @define addOneManyParamAllData
  *   the data to add
  * @define removeOneManyDesc
  *   Remove all the values following the logic in
  * @define removeOneManyParamAllData
  *   the data to remove
  */
trait DimensionalMultiBase[V, D <: NonEmptyTuple: DomainLike](using Experimental) extends DimensionalBase[Set[V], D]:

  /**
    * Internal mutator to update all the valid value sets in the interval to include the new value, and to fill any
    * remaining portions of the interval without any valid values to just have the new value as valid.
    *
    * @param data
    *   new value that should be valid in this interval (along with any other existing values)
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def addOneInPlace[B <: V](data: ValidData[B, D]): Unit =
    updateOrRemove(data.interval, existingValues => Some(existingValues + data.value))
    fillInPlace(data.interval -> Set(data.value))

  /**
    * Internal mutator to update all the valid value sets in the interval to exclude the new value, and for any interval
    * that only contains the new value, remove the interval completely.
    *
    * @param data
    *   the value to make no longer valid in the interval
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def removeOneInPlace[B <: V](data: ValidData[B, D]): Unit =
    updateOrRemove(
      data.interval,
      existingValues =>
        val newValues = existingValues - data.value
        if newValues.isEmpty then None else Some(newValues)
    )

  /**
    * Returns the distinct individual values that are valid in some interval.
    */
  def valuesOne: Iterable[V] = values.flatten.toSet

  /**
    * Returns the intervals in which this individual value is valid.
    *
    * @param value
    *   the value to look up
    */
  def intervalsOne(value: V): Iterable[Interval[D]] = Interval.compress(
    values.filter(_.contains(value)).flatMap(intervals)
  )
