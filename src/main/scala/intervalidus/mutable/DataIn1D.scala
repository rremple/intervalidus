package intervalidus.mutable

import intervalidus.*
import intervalidus.DataIn1DBase.{DiffAction1D, ValidData1D}
import intervalidus.immutable.DataIn1D as DataIn1DImmutable

object DataIn1D:
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
    *   [[DataIn1D]] structure with a single valid value.
    */
  def of[V, R: DiscreteValue](value: V): DataIn1D[V, R] = DataIn1D(
    Iterable(ValidData1D(value, DiscreteInterval1D.unbounded[R]))
  )

/**
  * @inheritdoc
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of discrete domain used in the interval assigned to each value.
  * @param initialData
  *   (optional) a collection of valid data to start with -- intervals must be disjoint.
  */
class DataIn1D[V, R: DiscreteValue](
  initialData: Iterable[ValidData1D[V, R]] = Iterable.empty[ValidData1D[V, R]]
) extends DataIn1DBase[V, R](initialData)
  with MutableBase[V, DiscreteDomain1D[R], DiscreteInterval1D[R], ValidData1D[V, R]]:

  /**
    * Returns this as a immutable structure.
    */
  def toImmutable: DataIn1DImmutable[V, R] = DataIn1DImmutable(getAll)

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction1D[V, R]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction1D.Create(data) => addValidData(data)
      case DiffAction1D.Update(data) => updateValidData(data)
      case DiffAction1D.Delete(key)  => removeValidDataByKey(key)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: DataIn1D[V, R]): Unit = applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from Dimensional1DBase ----------

  override def zip[B](that: DataIn1DBase[B, R]): DataIn1D[(V, B), R] = DataIn1D(zipData(that))

  override def zipAll[B](
    that: DataIn1DBase[B, R],
    thisElem: V,
    thatElem: B
  ): DataIn1D[(V, B), R] = DataIn1D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from MutableBase ----------

  override def mapValues(f: V => V): Unit = synchronized:
    getAll
      .map(d => d.copy(value = f(d.value)))
      .foreach: newData =>
        updateValidData(newData)

  override def copy: DataIn1D[V, R] = DataIn1D(getAll)

  override def set(value: V, interval: DiscreteInterval1D[R]): Unit = set(ValidData1D(value, interval))

  override def setIfNoConflict(value: V, interval: DiscreteInterval1D[R]): Boolean =
    setIfNoConflict(ValidData1D(value, interval))

  override def compress(value: V): Unit = synchronized:
    compressInPlace(this)(value)
