package intervalidus.immutable

import intervalidus.*
import intervalidus.DataIn1DBase.{DiffAction1D, ValidData1D}
import intervalidus.mutable.DataIn1D as DataIn1DMutable

object DataIn1D extends DataIn1DBaseObject:
  override def of[V, R: DiscreteValue](
    data: ValidData1D[V, R]
  ): DataIn1D[V, R] = DataIn1D(Iterable(data))

  override def of[V, R: DiscreteValue](
    value: V
  ): DataIn1D[V, R] = of(DiscreteInterval1D.unbounded[R] -> value)

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
  with ImmutableBase[V, DiscreteDomain1D[R], DiscreteInterval1D[R], ValidData1D[V, R]]:

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: DataIn1DMutable[V, R] = DataIn1DMutable(getAll)

  private def copyAndModify(f: DataIn1D[V, R] => Unit): DataIn1D[V, R] =
    val result = copy
    f(result)
    result

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction1D[V, R]]): DataIn1D[V, R] = copyAndModify: result =>
    diffActions.foreach:
      case DiffAction1D.Create(data) => result.addValidData(data)
      case DiffAction1D.Update(data) => result.updateValidData(data)
      case DiffAction1D.Delete(key)  => result.removeValidDataByKey(key)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: DataIn1D[V, R]): DataIn1D[V, R] = applyDiffActions(that.diffActionsFrom(this))

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def map[B, S: DiscreteValue](f: ValidData1D[V, R] => ValidData1D[B, S]): DataIn1D[B, S] = DataIn1D(
    getAll.map(f)
  ).compressAll()

  /**
    * Applies a function to all valid data values. Only the valid data value type can be changed in the mapping.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def mapValues[B](f: V => B): DataIn1D[B, R] = DataIn1D(
    getAll.map(d => d.copy(value = f(d.value)))
  ).compressAll()

  /**
    * Builds a new structure by applying a function to all elements of this collection and concatenating the elements of
    * the resulting structures.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def flatMap[B, S: DiscreteValue](f: ValidData1D[V, R] => DataIn1D[B, S]): DataIn1D[B, S] = DataIn1D(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from Dimensional1DBase ----------

  override def zip[B](that: DataIn1DBase[B, R]): DataIn1D[(V, B), R] = DataIn1D(zipData(that))

  override def zipAll[B](
    that: DataIn1DBase[B, R],
    thisElem: V,
    thatElem: B
  ): DataIn1D[(V, B), R] = DataIn1D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn1D[V, R] = DataIn1D(getAll)

  // ---------- Implement methods from ImmutableBase ----------

  override def filter(p: ValidData1D[V, R] => Boolean): DataIn1D[V, R] = DataIn1D(getAll.filter(p))

  override def set(newData: ValidData1D[V, R]): DataIn1D[V, R] = copyAndModify: result =>
    result.updateOrRemove(newData.interval, None)
    result.addValidData(newData)
    result.compressInPlace(result)(newData.value)

  override def setIfNoConflict(newData: ValidData1D[V, R]): Option[DataIn1D[V, R]] =
    if getIntersecting(newData.interval).isEmpty
    then
      Some(copyAndModify: result =>
        result.addValidData(newData)
        compressInPlace(result)(newData.value)
      )
    else None

  override def update(data: ValidData1D[V, R]): DataIn1D[V, R] =
    copyAndModify(_.updateOrRemove(data.interval, Some(data.value)))

  override def replace(
    key: DiscreteDomain1D[R],
    newData: ValidData1D[V, R]
  ): DataIn1D[V, R] = copyAndModify: result =>
    result.removeValidDataByKey(key)
    result.updateOrRemove(newData.interval, None)
    result.addValidData(newData)
    result.compressInPlace(result)(newData.value)

  override def replace(
    oldData: ValidData1D[V, R],
    newData: ValidData1D[V, R]
  ): DataIn1D[V, R] = replace(oldData.key, newData)

  override def remove(interval: DiscreteInterval1D[R]): DataIn1D[V, R] = copyAndModify(_.updateOrRemove(interval, None))

  override def compress(value: V): DataIn1D[V, R] = copyAndModify: result =>
    result.compressInPlace(result)(value)

  override def compressAll(): DataIn1D[V, R] = copyAndModify: result =>
    getAll.map(_.value).toSet.foreach(result.compressInPlace(result))
