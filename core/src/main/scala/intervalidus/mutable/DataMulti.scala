package intervalidus.mutable

import intervalidus.*
import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.Tuple.{Concat, Drop, Elem, Head, Tail, Take}
import scala.collection.mutable
import scala.compiletime.ops.int.S

/**
  * Constructs multi-data in multidimensional intervals.
  */
object DataMulti extends DimensionalMultiBaseObject:
  type In1D[V, R1] = DataMulti[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataMulti[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataMulti[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataMulti[V, Domain.In4D[R1, R2, R3, R4]]

  override def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): DataMulti[V, D] = DataMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DataMulti[V, D] = of(Interval.unbounded[D] -> value)

  override def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using Experimental): DataMulti[V, D] =
    val result = DataMulti[V, D]()
    result.addOneMany(initialData)
    result

  override def from[V, D <: NonEmptyTuple: DomainLike](
    that: DimensionalBase[Set[V], D]
  )(using Experimental): DataMulti[V, D] = apply(that.getAll)

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[Set[V], D]] = Iterable.empty[ValidData[Set[V], D]]
  )(using Experimental): DataMulti[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new DataMulti(byStartAsc, byValue, inSearchTree)

  override def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], DataMulti[V, D]] = DimensionalDataMultiBuilder[V, D, DataMulti[V, D]](from(_))

/**
  * Mutable, multivalued dimensional data.
  *
  * Data may have multiple values valid in different intervals of arbitrary dimensions, conceptually similar to a
  * multimap. For example, one may want to represent when the data are valid in time and over certain versions, or in
  * two dimensions of time, simultaneously. When queried, values are returned as a set. The standard mutation methods
  * operate on these sets of values. There are also add and remove methods allow mutation of individual values across
  * intervals, and a concat method for combining two structures (a merge where overlaps are concatenated).
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataMulti[V, D <: NonEmptyTuple: DomainLike] protected (
  override val dataByStartAsc: mutable.TreeMap[D, ValidData[Set[V], D]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData[Set[V], D]],
  override val dataInSearchTree: BoxTree[ValidData[Set[V], D]]
)(using experimental: Experimental)
  extends MutableBase[Set[V], D]
  with DimensionalMultiBase[V, D]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Specific multivalue methods that have mutable signatures ----------

  /**
    * $mergeOneDesc $mutableAction
    *
    * @param that
    *   $mergeOneParamThat
    */
  def mergeOne(that: DataMulti[V, D]): Unit =
    merge(that, _ ++ _)

  /**
    * $addOneDesc $mutableAction
    *
    * @param data
    *   $addOneParamData
    */
  def addOne(data: ValidData[V, D]): Unit =
    addOneInPlace(data)

  /**
    * $removeOneDesc $mutableAction
    *
    * @param data
    *   $removeOneParamData
    */
  def removeOne(data: ValidData[V, D]): Unit =
    removeOneInPlace(data)

  /**
    * $addOneManyDesc [[addOne]]. $mutableAction
    *
    * @param allData
    *   $addOneManyParamAllData
    */
  def addOneMany(allData: Iterable[ValidData[V, D]]): Unit =
    allData.foreach(addOneInPlace)

  /**
    * $removeOneManyDesc [[removeOne]]. $mutableAction
    *
    * @param allData
    *   $removeOneManyParamAllData
    */
  def removeOneMany(allData: Iterable[ValidData[V, D]]): Unit =
    allData.foreach(removeOneInPlace)

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: DataMulti[V, D] =
    new DataMulti(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(Set[V], B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: Set[V], thatDefault: B): Data[(Set[V], B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Head[D] =:= Domain1D[H],
    Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): DataMulti[V, NonEmptyTail[D]] =
    val result = DataMulti(getByHeadDimensionData(domain))
    result.compressAll()
    result

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int,
    domain: Domain1D[H]
  )(using
    Elem[D, dimensionIndex.type] =:= Domain1D[H],
    Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D,
    Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
  ): DataMulti[V, R] =
    val result = DataMulti(getByDimensionData(dimensionIndex, domain))
    result.compressAll()
    result

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    this

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] =
    intervalidus.immutable.DataMulti(getAll)
