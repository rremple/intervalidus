package intervalidus

/**
  * Constructs multi-data in four-dimensional intervals.
  */
trait DataIn4DMultiBaseObject extends DataIn4DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @param data
    *   valid data to start with.
    * @return
    *   [[DataIn4DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    data: ValidData4D[V, R1, R2, R3, R4]
  )(using Experimental): DataIn4DMultiBase[V, R1, R2, R3, R4]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn4DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    value: V
  )(using Experimental): DataIn4DMultiBase[V, R1, R2, R3, R4]

  /**
    * Constructor for multiple initial single values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @return
    *   [[DataIn4DMultiBase]] structure with zero or more valid values.
    */
  def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[V, R1, R2, R3, R4]]
  )(using Experimental): DataIn4DMultiBase[V, R1, R2, R3, R4]

  /**
    * Creates a muti-data structure from non-multi structure managing sets of values.
    *
    * @param that
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @return
    *   [[DataIn4DMultiBase]] structure with the same valid values.
    */
  def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    that: DataIn4DBase[Set[V], R1, R2, R3, R4]
  )(using Experimental): DataIn4DMultiBase[V, R1, R2, R3, R4]

  /**
    * Constructor for multiple (or no) initial value sets that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data sets to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @return
    *   [[DataIn4DMultiBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[Set[V], R1, R2, R3, R4]]
  )(using Experimental): DataIn4DMultiBase[V, R1, R2, R3, R4]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value.
  * @tparam R4
  *   the type of domain value used in the fourth interval assigned to each value.
  */
// Base for all 4D multi-data, both mutable and immutable
trait DataIn4DMultiBase[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike]
  extends DataIn4DBase[Set[V], R1, R2, R3, R4]:
  // from Object
  override def toString: String = toStringGrid(
    dataToString = v =>
      s"${v.value.map(_.toString).mkString("{", ",", "}")} ${v.interval.vertical} x ${v.interval.depth} x ${v.interval.fourth}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )
