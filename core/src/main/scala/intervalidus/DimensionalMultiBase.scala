package intervalidus

/**
  * Constructs multi-valued data in multidimensional intervals.
  */
trait DimensionalMultiBaseObject extends DimensionalBaseConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain used in the interval assigned to each value.
    * @param data
    *   valid data to start with.
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
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain used in the interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DimensionalMultiBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Constructor for multiple initial single values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain used in the interval assigned to each value.
    * @return
    *   [[DimensionalMultiBase]] structure with zero or more valid values.
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Creates a muti-data structure from non-multi structure managing sets of values.
    *
    * @param that
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain used in the interval assigned to each value.
    * @return
    *   [[DimensionalMultiBase]] structure with the same valid values.
    */
  def from[V, D <: NonEmptyTuple: DomainLike](
    that: DimensionalBase[Set[V], D]
  )(using Experimental): DimensionalMultiBase[V, D]

  /**
    * Constructor for multiple (or no) initial value sets that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data sets to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain used in the interval assigned to each value.
    * @return
    *   [[DimensionalMultiBase]] structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[Set[V], D]]
  )(using Experimental): DimensionalMultiBase[V, D]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals, must be [[DomainLike]].
  */
trait DimensionalMultiBase[V, D <: NonEmptyTuple: DomainLike] extends DimensionalBase[Set[V], D]:

  /**
    * Updates all the valid value sets in the interval to include the new value. Fills any remaining portions of the
    * interval without any valid values to just have the new value as valid.
    *
    * @param data
    *   new value that should be valid in this interval (along with other existing values)
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def addOneInPlace[B <: V](data: ValidData[B, D]): Unit =
    updateOrRemove(data.interval, existingValues => Some(existingValues + data.value))
    fillInPlace(data.interval, Set(data.value))

  /**
    * Updates all the valid value sets in the interval to exclude the new value. For any interval that only contains the
    * new value, remove the interval completely.
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
    * Does an element-wise merge of this structure with that data.
    * @param thoseData
    *   data to be merged
    * @tparam B
    *   type of value to be merged (subtype of [[V]])
    */
  protected def mergeInPlace[B <: V](thoseData: Iterable[ValidData[Set[B], D]]): Unit =
    thoseData.foreach: validData =>
      validData.value.foreach: b =>
        addOneInPlace(validData.interval -> b)

object DimensionalMultiBase:
  type In1D[V, R1] = DimensionalMultiBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalMultiBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalMultiBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalMultiBase[V, Domain.In4D[R1, R2, R3, R4]]
