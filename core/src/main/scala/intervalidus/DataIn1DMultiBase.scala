package intervalidus

/**
  * Constructs multi-data in one-dimensional intervals.
  */
trait DataIn1DMultiBaseObject extends DataIn1DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value.
    * @param data
    *   valid data to start with.
    * @return
    *   [[DataIn1DMultiBase]] structure with a single valid value.
    */
  def of[V, R: DiscreteValue](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1DMultiBase[V, R]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn1DMultiBase]] structure with a single valid value.
    */
  def of[V, R: DiscreteValue](
    value: V
  )(using Experimental): DataIn1DMultiBase[V, R]

  /**
    * Constructor for multiple initial single values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete domain used in the interval assigned to each value.
    * @return
    *   [[DataIn1DMultiBase]] structure with zero or more valid values.
    */
  def from[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]]
  )(using Experimental): DataIn1DMultiBase[V, R]

  /**
    * Creates a muti-data structure from non-multi structure managing sets of values.
    *
    * @param that
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete domain used in the interval assigned to each value.
    * @return
    *   [[DataIn1DMultiBase]] structure with the same valid values.
    */
  def from[V, R: DiscreteValue](
    that: DataIn1DBase[Set[V], R]
  )(using Experimental): DataIn1DMultiBase[V, R]

  /**
    * Constructor for multiple (or no) initial value sets that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data sets to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete domain used in the interval assigned to each value.
    * @return
    *   [[DataIn1DMultiBase]] structure with zero or more valid values.
    */
  def apply[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[Set[V], R]]
  )(using Experimental): DataIn1DMultiBase[V, R]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of discrete domain used in the interval assigned to each value.
  */
// Base for all 1D multi-data, both mutable and immutable
trait DataIn1DMultiBase[V, R: DiscreteValue] extends DataIn1DBase[Set[V], R]:
  // from Object
  override def toString: String = toStringGrid(
    dataToString = _.value.map(_.toString).mkString("{", ",", "}"),
    dataToInterval = _.interval,
    dataToSortBy = _.interval.end
  )
