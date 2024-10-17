package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxBtree, MultiMapSorted}
import intervalidus.mutable.DataIn1D as DataIn1DMutable

import scala.collection.mutable

/**
  * Constructs data in one-dimensional intervals.
  */
object DataIn1D extends DataIn1DBaseObject:
  override def of[V, R: DiscreteValue](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1D[V, R] = DataIn1D(Iterable(data))

  override def of[V, R: DiscreteValue](
    value: V
  )(using Experimental): DataIn1D[V, R] = of(DiscreteInterval1D.unbounded[R] -> value)

  override def apply[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]] = Iterable.empty[ValidData1D[V, R]]
  )(using Experimental): DataIn1D[V, R] =
    val param = constructorParams(initialData)
    new DataIn1D(param._1, param._2, param._3, param._4)

/**
  * Data that may have different values in different intervals. These intervals may represent when the data are valid in
  * time or over certain versions ranges or whatever. But we can capture the dependency between various values and
  * related intervals cohesively in this structure rather than in separate data structures using distributed (and
  * potentially inconsistent) logic.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of discrete domain used in the interval assigned to each value.
  */
class DataIn1D[V, R: DiscreteValue] private (
  override val dataByStartAsc: mutable.TreeMap[DiscreteDomain1D[R], ValidData1D[V, R]],
  override val dataByStartDesc: mutable.TreeMap[DiscreteDomain1D[R], ValidData1D[V, R]],
  override val dataByValue: MultiMapSorted[V, ValidData1D[V, R]],
  override val dataInSearchTree: BoxBtree[ValidData1D[V, R]]
)(using Experimental)
  extends DataIn1DBase[V, R]
  with ImmutableBase[V, DiscreteDomain1D[R], DiscreteInterval1D[R], ValidData1D[V, R], DataIn1D[V, R]]:

  override def toMutable: DataIn1DMutable[V, R] = DataIn1DMutable(getAll)

  override def toImmutable: DataIn1D[V, R] = this

  override protected def copyAndModify(f: DataIn1D[V, R] => Unit): DataIn1D[V, R] =
    val result = copy
    f(result)
    result

  override def recompressAll(): DataIn1D[V, R] = this // nothing to do in 1D

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

  override def copy: DataIn1D[V, R] =
    new DataIn1D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)
