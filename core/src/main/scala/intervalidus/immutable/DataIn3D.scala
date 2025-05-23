package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.mutable.DataIn3D as DataIn3DMutable

import scala.collection.mutable

/**
  * Constructs data in three-dimensional intervals.
  */
object DataIn3D extends DataIn3DBaseObject:
  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    data: ValidData3D[V, R1, R2, R3]
  )(using Experimental): DataIn3D[V, R1, R2, R3] = DataIn3D(Iterable(data))

  override def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    value: V
  )(using Experimental): DataIn3D[V, R1, R2, R3] = of(Interval3D.unbounded[R1, R2, R3] -> value)

  override def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]] = Iterable.empty[ValidData3D[V, R1, R2, R3]]
  )(using Experimental): DataIn3D[V, R1, R2, R3] =
    val (byStartAsc, byStartDesc, byValue, inSearchTree) = constructorParams(initialData)
    new DataIn3D(byStartAsc, byStartDesc, byValue, inSearchTree)

/**
  * Like [[DataIn1D]] and [[DataIn2D]], data here have different values in different intervals. But here data values
  * vary in three dimensions. For example, one may want to represent when data are valid in two dimensions of time and
  * over certain versions simultaneously.
  *
  * We can capture the dependency between various values and related three-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in three dimensions.
  * @note
  *   Visualizing three-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt
  *   chart and there is a simple Visualize3D tool which shows a numerically boxed representation of data, as well as a
  *   Visualize2D tool provided where you can visualize 2D slices of the 3D structure (in the test package... though
  *   maybe this should be its own separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value.
  */
class DataIn3D[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike] private (
  override val dataByStartAsc: mutable.TreeMap[Domain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]],
  override val dataByStartDesc: mutable.TreeMap[Domain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]],
  override val dataByValue: MultiMapSorted[V, ValidData3D[V, R1, R2, R3]],
  override val dataInSearchTree: BoxTree[ValidData3D[V, R1, R2, R3]]
)(using Experimental)
  extends DataIn3DBase[V, R1, R2, R3]
  with ImmutableBase[
    V,
    Domain3D[R1, R2, R3],
    Interval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3],
    DiffAction3D[V, R1, R2, R3],
    DataIn3D[V, R1, R2, R3]
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
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def map[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike](
    f: ValidData3D[V, R1, R2, R3] => ValidData3D[B, S1, S2, S3]
  ): DataIn3D[B, S1, S2, S3] = DataIn3D(
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
  def mapValues[B](
    f: V => B
  ): DataIn3D[B, R1, R2, R3] = DataIn3D(
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
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    */
  def flatMap[B, S1: DomainValueLike, S2: DomainValueLike, S3: DomainValueLike](
    f: ValidData3D[V, R1, R2, R3] => DataIn3D[B, S1, S2, S3]
  ): DataIn3D[B, S1, S2, S3] = DataIn3D(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DataIn3DBase ----------

  override def zip[B](that: DataIn3DBase[B, R1, R2, R3]): DataIn3D[(V, B), R1, R2, R3] = DataIn3D(zipData(that))

  override def zipAll[B](
    that: DataIn3DBase[B, R1, R2, R3],
    thisElem: V,
    thatElem: B
  ): DataIn3D[(V, B), R1, R2, R3] = DataIn3D(zipAllData(that, thisElem, thatElem))

  override def flipAboutHorizontal: DataIn3D[V, R1, R3, R2] =
    map(d => d.copy(interval = d.interval.flipAboutHorizontal))

  override def flipAboutVertical: DataIn3D[V, R3, R2, R1] =
    map(d => d.copy(interval = d.interval.flipAboutVertical))

  override def flipAboutDepth: DataIn3D[V, R2, R1, R3] =
    map(d => d.copy(interval = d.interval.flipAboutDepth))

  override def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn2D[V, R2, R3] =
    DataIn2D[V, R2, R3](getByHorizontalIndexData(horizontalIndex))

  override def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn2D[V, R1, R3] =
    DataIn2D[V, R1, R3](getByVerticalIndexData(verticalIndex))

  override def getByDepthIndex(depthIndex: Domain1D[R3]): DataIn2D[V, R1, R2] =
    DataIn2D[V, R1, R2](getByDepthIndexData(depthIndex))

  // ---------- Implement methods from ImmutableBase ----------

  override protected def copyAndModify(f: DataIn3D[V, R1, R2, R3] => Unit): DataIn3D[V, R1, R2, R3] =
    val result = copy
    f(result)
    result

  override def applyDiffActions(diffActions: Iterable[DiffAction3D[V, R1, R2, R3]]): DataIn3D[V, R1, R2, R3] =
    copyAndModify: result =>
      diffActions.foreach:
        case DiffAction3D.Create(data) => result.addValidData(data)
        case DiffAction3D.Update(data) => result.updateValidData(data)
        case DiffAction3D.Delete(key)  => result.removeValidDataByKey(key)

  override def syncWith(that: DataIn3D[V, R1, R2, R3]): DataIn3D[V, R1, R2, R3] =
    applyDiffActions(that.diffActionsFrom(this))

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn3D[V, R1, R2, R3] =
    new DataIn3D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def toMutable: DataIn3DMutable[V, R1, R2, R3] = DataIn3DMutable(getAll)

  override def toImmutable: DataIn3D[V, R1, R2, R3] = this
