package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxQuadtree, MultiDictSorted}
import intervalidus.mutable.DataIn2D as DataIn2DMutable

import scala.collection.mutable

object DataIn2D extends DataIn2DBaseObject:
  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2]
  )(using Experimental): DataIn2D[V, R1, R2] = DataIn2D(Iterable(data))

  override def of[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V
  )(using Experimental): DataIn2D[V, R1, R2] = of(DiscreteInterval2D.unbounded[R1, R2] -> value)

  override def apply[V, R1: DiscreteValue, R2: DiscreteValue](
    initialData: Iterable[ValidData2D[V, R1, R2]] = Iterable.empty[ValidData2D[V, R1, R2]]
  )(using Experimental): DataIn2D[V, R1, R2] =
    val param = constructorParams(initialData)
    new DataIn2D(param._1, param._2, param._3, param._4)

/**
  * Like [[DataIn1D]], data here have different values in different discrete intervals. But here data values vary in two
  * dimensions. For example, one may want to represent when the data are valid in time and over certain versions
  * simultaneously.
  *
  * We can capture the dependency between various values and related two-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in two dimensions. Note that visualizing two
  * dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt chart and there is a
  * simple Visualize tool provided (in the test package... though maybe this should be its own separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  */
class DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue] private (
  override val dataByStartAsc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[V, R1, R2]],
  override val dataByStartDesc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[V, R1, R2]],
  override val dataByValue: MultiDictSorted[V, ValidData2D[V, R1, R2]],
  override val dataInSearchTree: BoxQuadtree[ValidData2D[V, R1, R2]]
)(using Experimental)
  extends DataIn2DBase[V, R1, R2]
  with ImmutableBase[
    V,
    DiscreteDomain2D[R1, R2],
    DiscreteInterval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DataIn2D[V, R1, R2]
  ]:

  override def toMutable: DataIn2DMutable[V, R1, R2] = DataIn2DMutable(getAll)

  override def toImmutable: DataIn2D[V, R1, R2] = this

  override protected def copyAndModify(f: DataIn2D[V, R1, R2] => Unit): DataIn2D[V, R1, R2] =
    val result = copy
    f(result)
    result

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction2D[V, R1, R2]]): DataIn2D[V, R1, R2] = copyAndModify: result =>
    diffActions.foreach:
      case DiffAction2D.Create(data) => result.addValidData(data)
      case DiffAction2D.Update(data) => result.updateValidData(data)
      case DiffAction2D.Delete(key)  => result.removeValidDataByKey(key)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: DataIn2D[V, R1, R2]): DataIn2D[V, R1, R2] = applyDiffActions(that.diffActionsFrom(this))

  /**
    * Unlike in 1D, there is no unique compression in 2D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could also be
    * represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  def recompressAll(): DataIn2D[V, R1, R2] = copyAndModify(_.recompressInPlace())

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
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def map[B, S1: DiscreteValue, S2: DiscreteValue](
    f: ValidData2D[V, R1, R2] => ValidData2D[B, S1, S2]
  ): DataIn2D[B, S1, S2] = DataIn2D(
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
  ): DataIn2D[B, R1, R2] = DataIn2D(
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
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def flatMap[B, S1: DiscreteValue, S2: DiscreteValue](
    f: ValidData2D[V, R1, R2] => DataIn2D[B, S1, S2]
  ): DataIn2D[B, S1, S2] = DataIn2D(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DimensionalBase ----------

  override def copy: DataIn2D[V, R1, R2] =
    new DataIn2D(dataByStartAsc.clone(), dataByStartDesc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def domain: Iterable[DiscreteInterval2D[R1, R2]] =
    // leverage compression logic by setting all the values as being the same thing
    val r = mapValues(_ => ())
    r.recompressInPlace()
    r.getAll.map(_.interval)

  // ---------- Implement methods from DataIn2DBase ----------

  override def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(V, B), R1, R2] = DataIn2D(zipData(that))

  override def zipAll[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): DataIn2D[(V, B), R1, R2] = DataIn2D(zipAllData(that, thisElem, thatElem))

  override def flip: DataIn2D[V, R2, R1] = map(d => d.copy(interval = d.interval.flip))

  override def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn1D[V, R2] = DataIn1D[V, R2](
    getByHorizontalIndexData(horizontalIndex)
  )

  override def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1D[V, R1] = DataIn1D[V, R1](
    getByVerticalIndexData(verticalIndex)
  )