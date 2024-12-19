package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.mutable.DataIn2DMulti as DataIn2DMultiMutable

import scala.collection.mutable

/**
  * Constructs multi-data in two-dimensional intervals.
  */
object DataIn2DMulti extends DataIn2DMultiBaseObject:
  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2]
  )(using Experimental): DataIn2DMulti[V, R1, R2] = DataIn2DMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V
  )(using Experimental): DataIn2DMulti[V, R1, R2] = of(DiscreteInterval2D.unbounded[R1, R2] -> value)

  override def from[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[V, R1, R2]]
  )(using Experimental): DataIn2DMulti[V, R1, R2] =
    DataIn2DMulti[V, R1, R2]().addAll(initialData)

  override def from[V, R1: DiscreteValue, R2: DiscreteValue](
    that: DataIn2DBase[Set[V], R1, R2]
  )(using Experimental): DataIn2DMulti[V, R1, R2] = apply(that.getAll)

  override def apply[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[Set[V], R1, R2]] = Iterable.empty[ValidData2D[Set[V], R1, R2]]
  )(using Experimental): DataIn2DMulti[V, R1, R2] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn2DMulti(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1DMulti]], data may have multiple values valid in different intervals, conceptually similar to a
  * multimap. But here data values vary in two dimensions. For example, one may want to represent when the data are
  * valid in time and over certain versions, or in two dimensions of time, simultaneously. When queried, values are
  * returned as a set. The standard mutation methods operate on these sets of values. There are also add and remove
  * methods allow mutation of individual values across intervals, and a merge method for combining two structures
  * (conceptually similar to zip, but operating on individual values, and more appropriate for these multiple values
  * structures).
  *
  * @note
  *   Visualizing two-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt
  *   chart and there is a simple Visualize tool provided (in the test package... though maybe this should be its own
  *   separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  */
class DataIn2DMulti[V, R1: DiscreteValue, R2: DiscreteValue] private (
  override val dataByStartAsc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[Set[V], R1, R2]],
  override val dataByStartDesc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[Set[V], R1, R2]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData2D[Set[V], R1, R2]],
  override val dataInSearchTree: BoxTree[ValidData2D[Set[V], R1, R2]]
)(using Experimental)
  extends DataIn2DMultiBase[V, R1, R2]
  with ImmutableMultiBase[
    V,
    DiscreteDomain2D[R1, R2],
    DiscreteInterval2D[R1, R2],
    ValidData2D[V, R1, R2],
    ValidData2D[Set[V], R1, R2],
    DiffAction2D[Set[V], R1, R2],
    DataIn2DMulti[V, R1, R2]
  ]:

  /**
    * Applies a function to all valid data. Both the valid data value and interval types can be changed in the mapping.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S1
    *   the valid data horizontal interval type of the returned structure.
    * @tparam S2
    *   the valid data vertical interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def map[B, S1: DiscreteValue, S2: DiscreteValue](
    f: ValidData2D[Set[V], R1, R2] => ValidData2D[Set[B], S1, S2]
  ): DataIn2DMulti[B, S1, S2] = DataIn2DMulti(
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
  def mapValues[B](f: Set[V] => Set[B]): DataIn2DMulti[B, R1, R2] = DataIn2DMulti(
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
    * @tparam S1
    *   the valid data horizontal interval type of the returned structure.
    * @tparam S2
    *   the valid data vertical interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    */
  def flatMap[B, S1: DiscreteValue, S2: DiscreteValue](
    f: ValidData2D[Set[V], R1, R2] => DataIn2DMulti[B, S1, S2]
  ): DataIn2DMulti[B, S1, S2] = DataIn2DMulti(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DataIn1DBase - signatures are weird because B isn't a set type ----------

  override def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(Set[V], B), R1, R2] = DataIn2D(zipData(that))

  override def zipAll[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: Set[V],
    thatElem: B
  ): DataIn2DBase[(Set[V], B), R1, R2] = DataIn2D(zipAllData(that, thisElem, thatElem))

  override def flip: DataIn2DMulti[V, R2, R1] = map(d => d.copy(interval = d.interval.flip))

  override def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn1DMulti[V, R2] =
    DataIn1DMulti[V, R2](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1DMulti[V, R1] =
    DataIn1DMulti[V, R1](getByVerticalIndexData(verticalIndex))

  // ---------- Implement methods from ImmutableBase ----------

  override protected def copyAndModify(f: DataIn2DMulti[V, R1, R2] => Unit): DataIn2DMulti[V, R1, R2] =
    val result = copy
    f(result)
    result

  override def applyDiffActions(diffActions: Iterable[DiffAction2D[Set[V], R1, R2]]): DataIn2DMulti[V, R1, R2] =
    copyAndModify: result =>
      diffActions.foreach:
        case DiffAction2D.Create(data) => result.addValidData(data)
        case DiffAction2D.Update(data) => result.updateValidData(data)
        case DiffAction2D.Delete(key)  => result.removeValidDataByKey(key)

  override def syncWith(that: DataIn2DMulti[V, R1, R2]): DataIn2DMulti[V, R1, R2] =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn2DMulti[V, R1, R2] =
    new DataIn2DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn2DMultiMutable[V, R1, R2] = DataIn2DMultiMutable(getAll)

  override def toImmutable: DataIn2DMulti[V, R1, R2] = this
