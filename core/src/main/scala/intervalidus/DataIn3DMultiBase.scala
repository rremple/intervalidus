package intervalidus

/**
  * Constructs multi-data in one-dimensional intervals.
  */
trait DataIn3DMultiBaseObject extends DataIn3DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @tparam R3
    *   the type of discrete domain used in the depth interval assigned to each value.
    * @param data
    *   valid data to start with.
    * @return
    *   [[DataIn3DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    data: ValidData3D[V, R1, R2, R3]
  )(using Experimental): DataIn3DMultiBase[V, R1, R2, R3]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @tparam R3
    *   the type of discrete domain used in the depth interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn3DMultiBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    value: V
  )(using Experimental): DataIn3DMultiBase[V, R1, R2, R3]

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
    * @tparam R3
    *   the type of discrete domain used in the depth interval assigned to each value.
    * @return
    *   [[DataIn3DMultiBase]] structure with zero or more valid values.
    */
  def from[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]]
  )(using Experimental): DataIn3DMultiBase[V, R1, R2, R3]

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
    * @tparam R3
    *   the type of discrete domain used in the depth interval assigned to each value.
    * @return
    *   [[DataIn3DMultiBase]] structure with the same valid values.
    */
  def from[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    that: DataIn3DBase[Set[V], R1, R2, R3]
  )(using Experimental): DataIn3DMultiBase[V, R1, R2, R3]

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
    * @tparam R3
    *   the type of discrete domain used in the depth interval assigned to each value.
    * @return
    *   [[DataIn3DMultiBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    initialData: Iterable[ValidData3D[Set[V], R1, R2, R3]]
  )(using Experimental): DataIn3DMultiBase[V, R1, R2, R3]

/**
  * Data that may have multiple values (managed as sets of values) in different intervals.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of discrete domain used in the depth interval assigned to each value.
  */
// Base for all 3D multi-data, both mutable and immutable
trait DataIn3DMultiBase[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue]
  extends DataIn3DBase[Set[V], R1, R2, R3]:
  // from Object - use Visualize (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString =
      v => s"${v.value.map(_.toString).mkString("{", ",", "}")} ${v.interval.vertical} x ${v.interval.depth}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )