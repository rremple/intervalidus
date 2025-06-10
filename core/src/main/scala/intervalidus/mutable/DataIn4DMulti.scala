package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn4DMulti as DataIn4DMultiImmutable

import scala.collection.mutable

/**
  * Constructs multi-data in four-dimensional intervals.
  */
object DataIn4DMulti extends DataIn4DMultiBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    data: ValidData4D[V, R1, R2, R3, R4]
  )(using Experimental): DataIn4DMulti[V, R1, R2, R3, R4] = DataIn4DMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    value: V
  )(using Experimental): DataIn4DMulti[V, R1, R2, R3, R4] = of(Interval4D.unbounded[R1, R2, R3, R4] -> value)

  override def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[V, R1, R2, R3, R4]]
  )(using Experimental): DataIn4DMulti[V, R1, R2, R3, R4] =
    val result = DataIn4DMulti[V, R1, R2, R3, R4]()
    result.addAll(initialData)
    result

  override def from[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    that: DataIn4DBase[Set[V], R1, R2, R3, R4]
  )(using Experimental): DataIn4DMulti[V, R1, R2, R3, R4] = apply(that.getAll)

  override def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[Set[V], R1, R2, R3, R4]] = Iterable.empty[ValidData4D[Set[V], R1, R2, R3, R4]]
  )(using Experimental): DataIn4DMulti[V, R1, R2, R3, R4] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn4DMulti(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1DMulti]], [[DataIn2DMulti]], and [[DataIn3DMulti]], data may have multiple values valid in different
  * intervals, conceptually similar to a multimap. But here data values vary in four dimensions. For example, one may
  * want to represent when data are valid in three dimensions of space and one dimension of time simultaneously (i.e.,
  * in spacetime). When queried, values are returned as a set. The standard mutation methods operate on these sets of
  * values. There are also add and remove methods allowing mutation of individual values across intervals, and a merge
  * method for combining two structures (conceptually similar to zip, but operating on individual values, and more
  * appropriate for these multi-value structures).
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
class DataIn4DMulti[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[Set[V], R1, R2, R3, R4]],
  override val dataByStartDesc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[Set[V], R1, R2, R3, R4]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData4D[Set[V], R1, R2, R3, R4]],
  override val dataInSearchTree: BoxTree[ValidData4D[Set[V], R1, R2, R3, R4]]
)(using Experimental)
  extends DataIn4DMultiBase[V, R1, R2, R3, R4]
  with MutableMultiBase[
    V,
    Domain4D[R1, R2, R3, R4],
    Interval4D[R1, R2, R3, R4],
    ValidData4D[V, R1, R2, R3, R4],
    ValidData4D[Set[V], R1, R2, R3, R4],
    DiffAction4D[Set[V], R1, R2, R3, R4],
    DataIn4DMulti[V, R1, R2, R3, R4]
  ]:

  // ---------- Implement methods from DataIn4DBase ----------

  override def zip[B](that: DataIn4DBase[B, R1, R2, R3, R4]): DataIn4D[(Set[V], B), R1, R2, R3, R4] = DataIn4D(
    zipData(that)
  )

  override def zipAll[B](
    that: DataIn4DBase[B, R1, R2, R3, R4],
    thisElem: Set[V],
    thatElem: B
  ): DataIn4D[(Set[V], B), R1, R2, R3, R4] = DataIn4D(zipAllData(that, thisElem, thatElem))

  override def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn3DMulti[V, R2, R3, R4] =
    DataIn3DMulti[V, R2, R3, R4](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn3DMulti[V, R1, R3, R4] =
    DataIn3DMulti[V, R1, R3, R4](getByVerticalIndexData(verticalIndex))

  override def getByDepthIndex(depthIndex: Domain1D[R3]): DataIn3DMulti[V, R1, R2, R4] =
    DataIn3DMulti[V, R1, R2, R4](getByDepthIndexData(depthIndex))

  override def getByFourthIndex(fourthIndex: Domain1D[R4]): DataIn3DMulti[V, R1, R2, R3] =
    DataIn3DMulti[V, R1, R2, R3](getByFourthIndexData(fourthIndex))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction4D[Set[V], R1, R2, R3, R4]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction4D.Create(data) => addValidData(data)
      case DiffAction4D.Update(data) => updateValidData(data)
      case DiffAction4D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn4DMulti[V, R1, R2, R3, R4]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn4DMulti[V, R1, R2, R3, R4] =
    new DataIn4DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn4DMulti[V, R1, R2, R3, R4] = this

  override def toImmutable: DataIn4DMultiImmutable[V, R1, R2, R3, R4] = DataIn4DMultiImmutable(getAll)
  // override def toImmutable: DataIn4DBase[Set[V], R1, R2, R3, R4] & ImmutableBase[Set[V], Domain4D[R1, R2, R3, R4], Interval4D[R1, R2, R3, R4], ValidData4D[Set[V], R1, R2, R3, R4], DiffAction4D[Set[V], R1, R2, R3, R4], _] = ???
