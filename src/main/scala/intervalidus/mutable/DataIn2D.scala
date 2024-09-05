package intervalidus.mutable

import intervalidus.*

object DataIn2D extends DataIn2DBaseObject:
  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2]
  ): DataIn2D[V, R1, R2] = DataIn2D(Iterable(data))

  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V
  ): DataIn2D[V, R1, R2] = of(DiscreteInterval2D.unbounded[R1, R2] -> value)

/**
  * Like [[DataIn1D]], data here have different values in different discrete intervals. But here data values vary in two
  * dimensions. For example, one may want to represent when the data are valid in time and over certain versions
  * simultaneously.
  *
  * We can capture the dependency between various values and related two-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in two dimensions. Note that visualizing
  * two-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt chart and there is
  * a simple Visualize tool provided (in the test package... though maybe this should be its own separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  * @param initialData
  *   (optional) a collection of valid data to start with -- intervals must be disjoint in two dimensions.
  */
class DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](
  initialData: Iterable[ValidData2D[V, R1, R2]] = Iterable.empty[ValidData2D[V, R1, R2]]
) extends DataIn2DBase[V, R1, R2](initialData)
  with MutableBase[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2], ValidData2D[V, R1, R2]]:

  override def toImmutable: immutable.DataIn2D[V, R1, R2] = immutable.DataIn2D(getAll)

  override def toMutable: DataIn2D[V, R1, R2] = this

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction2D[V, R1, R2]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction2D.Create(data) => addValidData(data)
      case DiffAction2D.Update(data) => updateValidData(data)
      case DiffAction2D.Delete(key)  => removeValidDataByKey(key)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: DataIn2D[V, R1, R2]): Unit = applyDiffActions(that.diffActionsFrom(this))

  /**
    * Unlike in 1D, there is no unique compression in 2D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could also be
    * represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  def recompressAll(): Unit = recompressInPlace()

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn2D[V, R1, R2] = DataIn2D(getAll)

  override def domain: Iterable[DiscreteInterval2D[R1, R2]] =
    // leverage compression logic by setting all the values as being the same (unit)
    val r = DataIn2D(getAll.map(_.copy(value = ())))
    r.recompressInPlace()
    r.getAll.map(_.interval)

  // ---------- Implement methods from DataIn2DBase ----------

  override def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(V, B), R1, R2] = DataIn2D(zipData(that))

  override def zipAll[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): DataIn2D[(V, B), R1, R2] = DataIn2D(zipAllData(that, thisElem, thatElem))

  override def flip: DataIn2D[V, R2, R1] = DataIn2D(getAll.map(d => d.copy(interval = d.interval.flip)))

  override def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn1D[V, R2] = DataIn1D[V, R2](
    getByHorizontalIndexData(horizontalIndex)
  )

  override def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1D[V, R1] = DataIn1D[V, R1](
    getByVerticalIndexData(verticalIndex)
  )

  // ---------- Implement methods from MutableBase ----------

  override def compress(value: V): Unit = synchronized:
    compressInPlace(this)(value)
