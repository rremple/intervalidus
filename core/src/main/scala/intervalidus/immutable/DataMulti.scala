package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

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
    DataMulti[V, D]().addOneMany(initialData)

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
  * Immutable, multivalued dimensional data.
  *
  * $classDesc
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
  extends ImmutableBase[Set[V], D, DataMulti[V, D]]
  with DimensionalMultiBase[V, D]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Specific multivalue methods that have immutable signatures ----------

  /**
    * $mergeOneDesc
    *
    * @param that
    *   $mergeOneParamThat
    * @return
    *   $immutableReturn
    */
  def mergeOne(that: DataMulti[V, D]): DataMulti[V, D] =
    merge(that, _ ++ _)

  /**
    * $addOneDesc
    *
    * @param data
    *   $addOneParamData
    * @return
    *   $immutableReturn
    */
  def addOne(data: ValidData[V, D]): DataMulti[V, D] =
    copyAndModify(_.addOneInPlace(data))

  /**
    * $removeOneDesc
    *
    * @param data
    *   $removeOneParamData
    * @return
    *   $immutableReturn
    */
  def removeOne(data: ValidData[V, D]): DataMulti[V, D] =
    copyAndModify(_.removeOneInPlace(data))

  /**
    * $addOneManyDesc [[addOne]].
    *
    * @param allData
    *   $addOneManyParamAllData
    * @return
    *   $immutableReturn
    */
  def addOneMany(allData: Iterable[ValidData[V, D]]): DataMulti[V, D] = copyAndModify: result =>
    allData.foreach(result.addOneInPlace)

  /**
    * $removeOneManyDesc [[removeOne]].
    *
    * @param allData
    *   $removeOneManyParamAllData
    * @return
    *   $immutableReturn
    */
  def removeOneMany(allData: Iterable[ValidData[V, D]]): DataMulti[V, D] = copyAndModify: result =>
    allData.foreach(result.removeOneInPlace)

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  //   (these return Data rather than DataMulti because B isn't necessarily a Set type)

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  ): Data[B, S] = Data(
    getAll.map(f)
  ).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[Set[V], D], ValidData[B, S]]
  ): Data[B, S] = Data(
    getAll.collect(pf)
  ).compressAll()

  override def mapValues[B](
    f: Set[V] => B
  ): Data[B, D] = Data(
    getAll.map(d => d.copy(value = f(d.value)))
  ).compressAll()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): DataMulti[V, S] = DataMulti(
    getAll.map(d => d.copy(interval = f(d.interval)))
  ).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => DimensionalBase[B, S]
  ): Data[B, S] = Data(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: DataMulti[V, D] =
    new DataMulti(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(Set[V], B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: Set[V], thatDefault: B): Data[(Set[V], B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtHead[D, H],
    Domain.HasAtLeastTwoDimensions[D],
    Domain.IsReconstructibleFromHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): DataMulti[V, Domain.NonEmptyTail[D]] =
    DataMulti(getByHeadDimensionData(domain)).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int & Singleton,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsReconstructible[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMulti[V, R] =
    DataMulti(getByDimensionData(dimensionIndex, domain)).compressAll()

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    intervalidus.mutable.DataMulti(getAll)

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] =
    this
