package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.mutable.DataIn4DMulti as DataIn4DMultiMutable

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
    DataIn4DMulti[V, R1, R2, R3, R4]().addAll(initialData)

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
  with ImmutableMultiBase[
    V,
    Domain4D[R1, R2, R3, R4],
    Interval4D[R1, R2, R3, R4],
    ValidData4D[V, R1, R2, R3, R4],
    ValidData4D[Set[V], R1, R2, R3, R4],
    DiffAction4D[Set[V], R1, R2, R3, R4],
    DataIn4DMulti[V, R1, R2, R3, R4]
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
    * @tparam S3
    *   the valid data depth interval type of the returned structure.
    * @tparam S4
    *   the valid data fourth interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def map[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike, S4: DomainValueLike](
    f: ValidData4D[Set[V], R1, R2, R3, R4] => ValidData4D[Set[B], S1, S2, S3, S4]
  ): DataIn4DMulti[B, S1, S2, S3, S4] = DataIn4DMulti(
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
  def mapValues[B](f: Set[V] => Set[B]): DataIn4DMulti[B, R1, R2, R3, R4] = DataIn4DMulti(
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
    * @tparam S3
    *   the valid data depth interval type of the returned structure.
    * @tparam S4
    *   the valid data fourth interval type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    */
  def flatMap[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike, S4: DomainValueLike](
    f: ValidData4D[Set[V], R1, R2, R3, R4] => DataIn4DMulti[B, S1, S2, S3, S4]
  ): DataIn4DMulti[B, S1, S2, S3, S4] = DataIn4DMulti(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DataIn1DBase - signatures are weird because B isn't a set type ----------

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

  // ---------- Implement methods from ImmutableBase ----------

  override protected def copyAndModify(f: DataIn4DMulti[V, R1, R2, R3, R4] => Unit): DataIn4DMulti[V, R1, R2, R3, R4] =
    val result = copy
    f(result)
    result

  override def applyDiffActions(
    diffActions: Iterable[DiffAction4D[Set[V], R1, R2, R3, R4]]
  ): DataIn4DMulti[V, R1, R2, R3, R4] =
    copyAndModify: result =>
      diffActions.foreach:
        case DiffAction4D.Create(data) => result.addValidData(data)
        case DiffAction4D.Update(data) => result.updateValidData(data)
        case DiffAction4D.Delete(key)  => result.removeValidDataByKey(key)

  override def syncWith(that: DataIn4DMulti[V, R1, R2, R3, R4]): DataIn4DMulti[V, R1, R2, R3, R4] =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn4DMulti[V, R1, R2, R3, R4] =
    new DataIn4DMulti(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn4DMultiMutable[V, R1, R2, R3, R4] = DataIn4DMultiMutable(getAll)

  override def toImmutable: DataIn4DMulti[V, R1, R2, R3, R4] = this
