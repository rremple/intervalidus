package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn1DMulti as DataIn1DMultiImmutable

import scala.collection.mutable

/**
  * Constructs multi-data in one-dimensional intervals.
  */
object DataIn1DMulti extends DataIn1DMultiBaseObject:
  override def of[V, R: DomainValueLike](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1DMulti[V, R] = DataIn1DMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, R: DomainValueLike](
    value: V
  )(using Experimental): DataIn1DMulti[V, R] = of(Interval1D.unbounded[R] -> value)

  override def from[V, R: DomainValueLike](
    initialData: Iterable[ValidData1D[V, R]]
  )(using Experimental): DataIn1DMulti[V, R] =
    val result = DataIn1DMulti[V, R]()
    result.addAll(initialData)
    result

  override def from[V, R: DomainValueLike](
    that: DataIn1DBase[Set[V], R]
  )(using Experimental): DataIn1DMulti[V, R] = apply(that.getAll)

  override def apply[V, R: DomainValueLike](
    initialData: Iterable[ValidData1D[Set[V], R]] = Iterable.empty[ValidData1D[Set[V], R]]
  )(using Experimental): DataIn1DMulti[V, R] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn1DMulti(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Data that may have multiple values valid in different intervals, conceptually similar to a multimap. These intervals
  * may represent when data are valid in time or over certain versions ranges or whatever. When queried, values are
  * returned as a set. The standard mutation methods operate on these sets of values. There are also add and remove
  * methods allow mutation of individual values across intervals, and a merge method for combining two structures
  * (conceptually similar to zip, but operating on individual values, and more appropriate for these multiple values
  * structures).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of domain value used in the interval assigned to each value.
  */
class DataIn1DMulti[V, R: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain1D[R], ValidData1D[Set[V], R]],
  override val dataByStartDesc: mutable.TreeMap[Domain1D[R], ValidData1D[Set[V], R]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData1D[Set[V], R]],
  override val dataInSearchTree: BoxTree[ValidData1D[Set[V], R]]
)(using Experimental)
  extends DataIn1DMultiBase[V, R]
  with MutableMultiBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    ValidData1D[Set[V], R],
    DiffAction1D[Set[V], R],
    DataIn1DMulti[V, R]
  ]:

  // ---------- Implement methods from DataIn1DBase - signatures are weird because B isn't a set type ----------

  override def zip[B](that: DataIn1DBase[B, R]): DataIn1DBase[(Set[V], B), R] = DataIn1D(zipData(that))

  override def zipAll[B](
    that: DataIn1DBase[B, R],
    thisElem: Set[V],
    thatElem: B
  ): DataIn1DBase[(Set[V], B), R] = DataIn1D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction1D[Set[V], R]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction1D.Create(data) => addValidData(data)
      case DiffAction1D.Update(data) => updateValidData(data)
      case DiffAction1D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn1DMulti[V, R]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn1DMulti[V, R] =
    new DataIn1DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn1DMulti[V, R] = this

  override def toImmutable: DataIn1DMultiImmutable[V, R] = DataIn1DMultiImmutable(getAll)
