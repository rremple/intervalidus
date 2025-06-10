package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn4D as DataIn4DImmutable

import scala.collection.mutable

/**
  * Constructs data in four-dimensional intervals.
  */
object DataIn4D extends DataIn4DBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    data: ValidData4D[V, R1, R2, R3, R4]
  )(using Experimental): DataIn4D[V, R1, R2, R3, R4] = DataIn4D(Iterable(data))

  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    value: V
  )(using Experimental): DataIn4D[V, R1, R2, R3, R4] = of(Interval4D.unbounded[R1, R2, R3, R4] -> value)

  override def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[V, R1, R2, R3, R4]] = Iterable.empty[ValidData4D[V, R1, R2, R3, R4]]
  )(using Experimental): DataIn4D[V, R1, R2, R3, R4] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn4D(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1D]], [[DataIn2D]], and [[DataIn3D]], data here have different values in different intervals. But here
  * data values vary in four dimensions. For example, one may want to represent when data are valid in three dimensions
  * of space and one dimension of time simultaneously (i.e., in spacetime).
  *
  * We can capture the dependency between various values and related four-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in four dimensions.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value.
  * @tparam R4
  *   the type of domain value used in the fourth interval assigned to each value.
  */
class DataIn4D[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]],
  override val dataByStartDesc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]],
  override val dataByValue: MultiMapSorted[V, ValidData4D[V, R1, R2, R3, R4]],
  override val dataInSearchTree: BoxTree[ValidData4D[V, R1, R2, R3, R4]]
)(using Experimental)
  extends DataIn4DBase[V, R1, R2, R3, R4]
  with MutableBase[
    V,
    Domain4D[R1, R2, R3, R4],
    Interval4D[R1, R2, R3, R4],
    ValidData4D[V, R1, R2, R3, R4],
    DiffAction4D[V, R1, R2, R3, R4],
    DataIn4D[V, R1, R2, R3, R4]
  ]:

  // ---------- Implement methods from DataIn4DBase ----------

  override def zip[B](that: DataIn4DBase[B, R1, R2, R3, R4]): DataIn4D[(V, B), R1, R2, R3, R4] = DataIn4D(zipData(that))

  override def zipAll[B](
    that: DataIn4DBase[B, R1, R2, R3, R4],
    thisElem: V,
    thatElem: B
  ): DataIn4D[(V, B), R1, R2, R3, R4] = DataIn4D(zipAllData(that, thisElem, thatElem))

  override def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn3D[V, R2, R3, R4] =
    DataIn3D[V, R2, R3, R4](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn3D[V, R1, R3, R4] =
    DataIn3D[V, R1, R3, R4](getByVerticalIndexData(verticalIndex))

  override def getByDepthIndex(depthIndex: Domain1D[R3]): DataIn3D[V, R1, R2, R4] =
    DataIn3D[V, R1, R2, R4](getByDepthIndexData(depthIndex))

  override def getByFourthIndex(fourthIndex: Domain1D[R4]): DataIn3D[V, R1, R2, R3] =
    DataIn3D[V, R1, R2, R3](getByFourthIndexData(fourthIndex))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction4D[V, R1, R2, R3, R4]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction4D.Create(data) => addValidData(data)
      case DiffAction4D.Update(data) => updateValidData(data)
      case DiffAction4D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn4D[V, R1, R2, R3, R4]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn4D[V, R1, R2, R3, R4] =
    new DataIn4D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn4D[V, R1, R2, R3, R4] = this

  override def toImmutable: DataIn4DImmutable[V, R1, R2, R3, R4] = DataIn4DImmutable(getAll)
