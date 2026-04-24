package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalBase.State

/**
  * Constructs multi-data in multidimensional intervals.
  */
object DataMulti extends DimensionalMultiBaseObject[DataMulti]:
  type In1D[V, R1] = DataMulti[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataMulti[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataMulti[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataMulti[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[Set[V], D]] = Iterable.empty[ValidData[Set[V], D]]
  )(using config: CoreConfig[D]): DataMulti[V, D] =
    new DataMulti(State.from(initialData))

  override def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using config: CoreConfig[D]): DataMulti[V, D] =
    val result = empty[V, D]
    result.addOneMany(initialData)
    result

/**
  * Mutable, multivalued dimensional data.
  *
  * $classDesc
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataMulti[V, D <: NonEmptyTuple: DomainLike] private (
  initialState: State[Set[V], D]
)(using val config: CoreConfig[D])
  extends MutableBase[Set[V], D]
  with DimensionalMultiBase[V, D]:

  @volatile override protected var state: State[Set[V], D] = initialState

  config.experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = transactionalRead:
      require(Interval.isDisjoint(getAllInternal.map(_.interval)), "data must be disjoint")
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
  def addOne(data: ValidData[V, D]): Unit = transactionalUpdate:
    addOneInPlace(data)

  /**
    * $removeOneDesc $mutableAction
    *
    * @param data
    *   $removeOneParamData
    */
  def removeOne(data: ValidData[V, D]): Unit = transactionalUpdate:
    removeOneInPlace(data)

  /**
    * $addOneManyDesc [[addOne]]. $mutableAction
    *
    * @param allData
    *   $addOneManyParamAllData
    */
  def addOneMany(allData: IterableOnce[ValidData[V, D]]): Unit = transactionalUpdate:
    addManyInPlace(allData)

  /**
    * $removeOneManyDesc [[removeOne]]. $mutableAction
    *
    * @param allData
    *   $removeOneManyParamAllData
    */
  def removeOneMany(allData: IterableOnce[ValidData[V, D]]): Unit = transactionalUpdate:
    removeManyInPlace(allData)

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataMulti because the resultant value type isn't necessarily a Set type) ----

  override def copy: DataMulti[V, D] =
    new DataMulti(state.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(Set[V], B), D] = transactionalReadWith(that): thatTx =>
    Data(zipData(that, thatTx, (_, _)))

  override def zipAll[B](
    that: DimensionalBase[B, D],
    thisDefault: Set[V],
    thatDefault: B
  ): Data[(Set[V], B), D] = transactionalReadWith(that): thatTx =>
    Data(zipAllData(that, thatTx, thisDefault, thatDefault, (_, _)))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using altConfig: CoreConfig[Domain.NonEmptyTail[D]]): DataMulti[V, Domain.NonEmptyTail[D]] = transactionalRead:
    val result = DataMulti(getByHeadDimensionData(domain))(using config = altConfig)
    result.compressAll()
    result

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DataMulti[V, R] = transactionalRead:
    val result = DataMulti(getByDimensionData(dimensionIndex, domain))(using config = altConfig)
    result.compressAll()
    result

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    this

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] = transactionalRead:
    intervalidus.immutable.DataMulti(getAllInternal)
