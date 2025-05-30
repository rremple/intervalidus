package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn2D as DataIn2DImmutable

import scala.collection.mutable

/**
  * Constructs data in two-dimensional intervals.
  */
object DataIn2D extends DataIn2DBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike](
    data: ValidData2D[V, R1, R2]
  )(using Experimental): DataIn2D[V, R1, R2] = DataIn2D(Iterable(data))

  override def of[V, R1: DomainValueLike, R2: DomainValueLike](
    value: V
  )(using Experimental): DataIn2D[V, R1, R2] = of(Interval2D.unbounded[R1, R2] -> value)

  override def apply[V, R1: DomainValueLike, R2: DomainValueLike](
    initialData: Iterable[ValidData2D[V, R1, R2]] = Iterable.empty[ValidData2D[V, R1, R2]]
  )(using Experimental): DataIn2D[V, R1, R2] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn2D(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1D]], data here have different values in different intervals. But here data values vary in two
  * dimensions. For example, one may want to represent when the data are valid in time and over certain versions, or in
  * two dimensions of time, simultaneously.
  *
  * We can capture the dependency between various values and related two-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in two dimensions.
  * @note
  *   Visualizing two-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt
  *   chart and there is a simple Visualize2D tool provided (in the test package... though maybe this should be its own
  *   separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  */
class DataIn2D[V, R1: DomainValueLike, R2: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]],
  override val dataByStartDesc: mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]],
  override val dataByValue: MultiMapSorted[V, ValidData2D[V, R1, R2]],
  override val dataInSearchTree: BoxTree[ValidData2D[V, R1, R2]]
)(using Experimental)
  extends DataIn2DBase[V, R1, R2]
  with MutableBase[
    V,
    Domain2D[R1, R2],
    Interval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2],
    DataIn2D[V, R1, R2]
  ]:

  // ---------- Implement methods from DataIn2DBase ----------

  override def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(V, B), R1, R2] = DataIn2D(zipData(that))

  override def zipAll[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): DataIn2D[(V, B), R1, R2] = DataIn2D(zipAllData(that, thisElem, thatElem))

  override def flip: DataIn2D[V, R2, R1] = DataIn2D(getAll.map(d => d.copy(interval = d.interval.flip)))

  override def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn1D[V, R2] =
    DataIn1D[V, R2](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn1D[V, R1] =
    DataIn1D[V, R1](getByVerticalIndexData(verticalIndex))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction2D[V, R1, R2]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction2D.Create(data) => addValidData(data)
      case DiffAction2D.Update(data) => updateValidData(data)
      case DiffAction2D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn2D[V, R1, R2]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn2D[V, R1, R2] =
    new DataIn2D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn2D[V, R1, R2] = this

  override def toImmutable: DataIn2DImmutable[V, R1, R2] = DataIn2DImmutable(getAll)
