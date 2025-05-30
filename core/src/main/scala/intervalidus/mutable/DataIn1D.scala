package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.immutable.DataIn1D as DataIn1DImmutable

import scala.collection.mutable

/**
  * Constructs data in one-dimensional intervals.
  */
object DataIn1D extends DataIn1DBaseObject:
  override def of[V, R: DomainValueLike](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1D[V, R] = DataIn1D(Iterable(data))

  override def of[V, R: DomainValueLike](
    value: V
  )(using Experimental): DataIn1D[V, R] = of(Interval1D.unbounded[R] -> value)

  override def apply[V, R: DomainValueLike](
    initialData: Iterable[ValidData1D[V, R]] = Iterable.empty[ValidData1D[V, R]]
  )(using Experimental): DataIn1D[V, R] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn1D(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Data that may have different values in different intervals. These intervals may represent when the data are valid in
  * time or over certain versions ranges or whatever. But we can capture the dependency between various values and
  * related intervals cohesively in this structure rather than in separate data structures using distributed (and
  * potentially inconsistent) logic.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of domain value used in the interval assigned to each value.
  */
class DataIn1D[V, R: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain1D[R], ValidData1D[V, R]],
  override val dataByStartDesc: mutable.TreeMap[Domain1D[R], ValidData1D[V, R]],
  override val dataByValue: MultiMapSorted[V, ValidData1D[V, R]],
  override val dataInSearchTree: BoxTree[ValidData1D[V, R]]
)(using Experimental)
  extends DataIn1DBase[V, R]
  with MutableBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R],
    DataIn1D[V, R]
  ]:

  // ---------- Implement methods from DataIn1DBase ----------

  override def zip[B](that: DataIn1DBase[B, R]): DataIn1D[(V, B), R] = DataIn1D(zipData(that))

  override def zipAll[B](
    that: DataIn1DBase[B, R],
    thisElem: V,
    thatElem: B
  ): DataIn1D[(V, B), R] = DataIn1D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from MutableBase ----------

  override def applyDiffActions(diffActions: Iterable[DiffAction1D[V, R]]): Unit = synchronized:
    diffActions.foreach:
      case DiffAction1D.Create(data) => addValidData(data)
      case DiffAction1D.Update(data) => updateValidData(data)
      case DiffAction1D.Delete(key)  => removeValidDataByKey(key)

  override def syncWith(that: DataIn1D[V, R]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn1D[V, R] =
    new DataIn1D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn1D[V, R] = this

  override def toImmutable: DataIn1DImmutable[V, R] = DataIn1DImmutable(getAll)
