package intervalidus.mutable

import intervalidus.*
import intervalidus.DataIn2DBase.{DiffAction2D, ValidData2D}
import intervalidus.immutable.DataIn2D as DataIn2DImmutable

object DataIn2D:
  /**
    * Shorthand constructor for a single initial value that is valid in both full interval domains.
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
    *   DataIn2D structure with a single valid value
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V
  ): DataIn2D[V, R1, R2] = DataIn2D(Iterable(ValidData2D(value, DiscreteInterval2D.unbounded)))

/**
  * @inheritdoc
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  * @param initialData
  *   (optional) a collection of valid data to start with -- intervals must be disjoint in two dimensions.
  */
// Base for all 2D data, both mutable and immutable
class DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](
  initialData: Iterable[DataIn2DBase.ValidData2D[V, R1, R2]] = Iterable.empty[DataIn2DBase.ValidData2D[V, R1, R2]]
) extends DataIn2DBase[V, R1, R2](initialData)
  with MutableBase[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2], ValidData2D[V, R1, R2]]:

  /**
    * Returns this as a immutable structure.
    */
  def toImmutable: DataIn2DImmutable[V, R1, R2] = DataIn2DImmutable(getAll)

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
    * Project as 1-dimensional data based on a horizontal domain element
    *
    * @param horizontalIndex
    *   the horizontal domain element
    * @return
    *   a 1-dimensional projection
    */
  def getByHorizontalIndex(
    horizontalIndex: DiscreteDomain1D[R1]
  ): DataIn1D[V, R2] = DataIn1D[V, R2](
    getByHorizontalIndexData(horizontalIndex)
  )

  /**
    * Project as 1-dimensional data based on a vertical domain element
    *
    * @param verticalIndex
    *   the vertical domain element
    * @return
    *   a 1-dimensional projection
    */
  def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1D[V, R1] = DataIn1D[V, R1](
    getByVerticalIndexData(verticalIndex)
  )

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn2D[V, R1, R2] = DataIn2D(getAll)

  override def domain: Iterable[DiscreteInterval2D[R1, R2]] =
    // leverage compression logic by setting all the values as being the same (unit)
    val r = DataIn2D(getAll.map(_.copy(value = ())))
    r.compressInPlace(r)(())
    r.getAll.map(_.interval)

  // ---------- Implement methods from DataIn2DBase ----------

  override def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(V, B), R1, R2] = DataIn2D(zipData(that))

  override def zipAll[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): DataIn2D[(V, B), R1, R2] = DataIn2D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from MutableBase ----------

  override def mapValues(f: V => V): Unit = synchronized:
    getAll
      .map(d => d.copy(value = f(d.value)))
      .foreach: newData =>
        updateValidData(newData)

  override def set(value: V, interval: DiscreteInterval2D[R1, R2]): Unit = set(ValidData2D(value, interval))

  override def setIfNoConflict(value: V, interval: DiscreteInterval2D[R1, R2]): Boolean =
    setIfNoConflict(ValidData2D(value, interval))

  override def compress(value: V): Unit = synchronized:
    compressInPlace(this)(value)
