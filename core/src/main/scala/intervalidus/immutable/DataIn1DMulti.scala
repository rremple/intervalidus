package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.mutable.DataIn1DMulti as DataIn1DMultiMutable

import scala.collection.mutable

/**
  * Constructs multi-data in one-dimensional intervals.
  */
object DataIn1DMulti extends DataIn1DMultiBaseObject:
  override def of[V, R: DiscreteValue](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1DMulti[V, R] = DataIn1DMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, R: DiscreteValue](
    value: V
  )(using Experimental): DataIn1DMulti[V, R] = of(Interval1D.unbounded[R] -> value)

  override def from[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]]
  )(using Experimental): DataIn1DMulti[V, R] = DataIn1DMulti[V, R]().addAll(initialData)

  override def from[V, R: DiscreteValue](
    that: DataIn1DBase[Set[V], R]
  )(using Experimental): DataIn1DMulti[V, R] = apply(that.getAll)

  override def apply[V, R: DiscreteValue](
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
  *   the type of discrete domain used in the interval assigned to each value.
  */
class DataIn1DMulti[V, R: DiscreteValue] private (
  override val dataByStartAsc: mutable.TreeMap[Domain1D[R], ValidData1D[Set[V], R]],
  override val dataByStartDesc: mutable.TreeMap[Domain1D[R], ValidData1D[Set[V], R]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData1D[Set[V], R]],
  override val dataInSearchTree: BoxTree[ValidData1D[Set[V], R]]
)(using Experimental)
  extends DataIn1DMultiBase[V, R]
  with ImmutableMultiBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    ValidData1D[Set[V], R],
    DiffAction1D[Set[V], R],
    DataIn1DMulti[V, R]
  ]:

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
    */
  def map[B, S: DiscreteValue](
    f: ValidData1D[Set[V], R] => ValidData1D[Set[B], S]
  ): DataIn1DMulti[B, S] =
    DataIn1DMulti(
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
  def mapValues[B](f: Set[V] => Set[B]): DataIn1DMulti[B, R] = DataIn1DMulti(
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
    */
  def flatMap[B, S: DiscreteValue](
    f: ValidData1D[Set[V], R] => DataIn1DMulti[B, S]
  ): DataIn1DMulti[B, S] =
    DataIn1DMulti(
      getAll.flatMap(f(_).getAll)
    ).compressAll()

  // ---------- Implement methods from DataIn1DBase - signatures are weird because B isn't a set type ----------

  override def zip[B](that: DataIn1DBase[B, R]): DataIn1DBase[(Set[V], B), R] = DataIn1D(zipData(that))

  override def zipAll[B](
    that: DataIn1DBase[B, R],
    thisElem: Set[V],
    thatElem: B
  ): DataIn1DBase[(Set[V], B), R] = DataIn1D(zipAllData(that, thisElem, thatElem))

  // ---------- Implement methods from ImmutableBase ----------

  override protected def copyAndModify(f: DataIn1DMulti[V, R] => Unit): DataIn1DMulti[V, R] =
    val result = copy
    f(result)
    result

  override def applyDiffActions(
    diffActions: Iterable[DiffAction1D[Set[V], R]]
  ): DataIn1DMulti[V, R] = copyAndModify: result =>
    diffActions.foreach:
      case DiffAction1D.Create(data) => result.addValidData(data)
      case DiffAction1D.Update(data) => result.updateValidData(data)
      case DiffAction1D.Delete(key)  => result.removeValidDataByKey(key)

  override def syncWith(that: DataIn1DMulti[V, R]): DataIn1DMulti[V, R] =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn1DMulti[V, R] =
    new DataIn1DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn1DMultiMutable[V, R] = DataIn1DMultiMutable(getAll)

  override def toImmutable: DataIn1DMulti[V, R] = this
