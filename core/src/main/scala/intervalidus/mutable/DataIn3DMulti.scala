package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn3DMulti as DataIn3DMultiImmutable

import scala.collection.mutable

/**
  * Constructs multi-data in three-dimensional intervals.
  */
object DataIn3DMulti extends DataIn3DMultiBaseObject:
  override def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    data: ValidData3D[V, R1, R2, R3]
  )(using Experimental): DataIn3DMulti[V, R1, R2, R3] = DataIn3DMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    value: V
  )(using Experimental): DataIn3DMulti[V, R1, R2, R3] = of(Interval3D.unbounded[R1, R2, R3] -> value)

  override def from[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]]
  )(using Experimental): DataIn3DMulti[V, R1, R2, R3] =
    val result = DataIn3DMulti[V, R1, R2, R3]()
    result.addAll(initialData)
    result

  override def from[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    that: DataIn3DBase[Set[V], R1, R2, R3]
  )(using Experimental): DataIn3DMulti[V, R1, R2, R3] = apply(that.getAll)

  override def apply[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    initialData: Iterable[ValidData3D[Set[V], R1, R2, R3]] = Iterable.empty[ValidData3D[Set[V], R1, R2, R3]]
  )(using Experimental): DataIn3DMulti[V, R1, R2, R3] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn3DMulti(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1DMulti]] and [[DataIn2DMulti]], data may have multiple values valid in different intervals,
  * conceptually similar to a multimap. But here data values vary in two dimensions. For example, one may want to
  * represent when data are valid in two dimensions of time and over certain versions simultaneously. When queried,
  * values are returned as a set. The standard mutation methods operate on these sets of values. There are also add and
  * remove methods allow mutation of individual values across intervals, and a merge method for combining two structures
  * (conceptually similar to zip, but operating on individual values, and more appropriate for these multiple values
  * structures).
  *
  * @note
  *   Visualizing three-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt
  *   chart and there is a simple 2D Visualize tool provided where you can visualize 2D slices of the 3D structure (in
  *   the test package... though maybe this should be its own separate subproject).
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
class DataIn3DMulti[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue] private (
  override val dataByStartAsc: mutable.TreeMap[Domain3D[R1, R2, R3], ValidData3D[Set[V], R1, R2, R3]],
  override val dataByStartDesc: mutable.TreeMap[Domain3D[R1, R2, R3], ValidData3D[Set[V], R1, R2, R3]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData3D[Set[V], R1, R2, R3]],
  override val dataInSearchTree: BoxTree[ValidData3D[Set[V], R1, R2, R3]]
)(using Experimental)
  extends DataIn3DMultiBase[V, R1, R2, R3]
  with MutableMultiBase[
    V,
    Domain3D[R1, R2, R3],
    Interval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3],
    ValidData3D[Set[V], R1, R2, R3],
    DiffAction3D[Set[V], R1, R2, R3],
    DataIn3DMulti[V, R1, R2, R3]
  ]:

  // ---------- Implement methods from DataIn3DBase ----------

  override def zip[B](that: DataIn3DBase[B, R1, R2, R3]): DataIn3D[(Set[V], B), R1, R2, R3] = DataIn3D(zipData(that))

  override def zipAll[B](
    that: DataIn3DBase[B, R1, R2, R3],
    thisElem: Set[V],
    thatElem: B
  ): DataIn3D[(Set[V], B), R1, R2, R3] = DataIn3D(zipAllData(that, thisElem, thatElem))

  override def flipAboutHorizontal: DataIn3DMulti[V, R1, R3, R2] =
    DataIn3DMulti(getAll.map(d => d.copy(interval = d.interval.flipAboutHorizontal)))

  override def flipAboutVertical: DataIn3DMulti[V, R3, R2, R1] =
    DataIn3DMulti(getAll.map(d => d.copy(interval = d.interval.flipAboutVertical)))

  override def flipAboutDepth: DataIn3DMulti[V, R2, R1, R3] =
    DataIn3DMulti(getAll.map(d => d.copy(interval = d.interval.flipAboutDepth)))

  override def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn2DMulti[V, R2, R3] =
    DataIn2DMulti[V, R2, R3](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn2DMulti[V, R1, R3] =
    DataIn2DMulti[V, R1, R3](getByVerticalIndexData(verticalIndex))

  override def getByDepthIndex(depthIndex: Domain1D[R3]): DataIn2DMulti[V, R1, R2] =
    DataIn2DMulti[V, R1, R2](getByDepthIndexData(depthIndex))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction3D[Set[V], R1, R2, R3]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction3D.Create(data) => addValidData(data)
      case DiffAction3D.Update(data) => updateValidData(data)
      case DiffAction3D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn3DMulti[V, R1, R2, R3]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn3DMulti[V, R1, R2, R3] =
    new DataIn3DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn3DMulti[V, R1, R2, R3] = this

  override def toImmutable: DataIn3DMultiImmutable[V, R1, R2, R3] = DataIn3DMultiImmutable(getAll)
