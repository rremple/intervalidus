package intervalidus

/**
  * Constructs multi-data in one-dimensional intervals.
  */
trait DataIn2DMultiBaseObject extends DataIn2DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @param data
    *   valid data to start with.
    * @return
    *   [[DataIn2DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2]
  )(using Experimental): DataIn2DMultiBase[V, R1, R2]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn2DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V
  )(using Experimental): DataIn2DMultiBase[V, R1, R2]

  /**
    * Constructor for multiple initial single values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete domain used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of discrete domain used in the vertical interval assigned to each value.
    * @return
    *   [[DataIn2DMultiBase]] structure with zero or more valid values.
    */
  def from[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[V, R1, R2]]
  )(using Experimental): DataIn2DMultiBase[V, R1, R2]

  /**
    * Creates a muti-data structure from non-multi structure managing sets of values.
    *
    * @param that
    *   a collection of valid data to start with.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete domain used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of discrete domain used in the vertical interval assigned to each value.
    * @return
    *   [[DataIn2DMultiBase]] structure with the same valid values.
    */
  def from[V, R1: DiscreteValue, R2: DiscreteValue](
    that: DataIn2DBase[Set[V], R1, R2]
  )(using Experimental): DataIn2DMultiBase[V, R1, R2]

  /**
    * Constructor for multiple (or no) initial value sets that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data sets to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete domain used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of discrete domain used in the vertical interval assigned to each value.
    * @return
    *   [[DataIn2DMultiBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[Set[V], R1, R2]]
  )(using Experimental): DataIn2DMultiBase[V, R1, R2]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  */
// Base for all 2D multi-data, both mutable and immutable
trait DataIn2DMultiBase[V, R1: DiscreteValue, R2: DiscreteValue] extends DataIn2DBase[Set[V], R1, R2]:
  // from Object - use Visualize (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value.map(_.toString).mkString("{", ",", "}")} ${v.interval.vertical}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )
