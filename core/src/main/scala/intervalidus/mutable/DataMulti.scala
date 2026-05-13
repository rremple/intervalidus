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
  override val initialState: State[Set[V], D]
)(using val config: CoreConfig[D])
  extends MutableBase[Set[V], D]
  with DimensionalMultiBase[V, D]:

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
    removeOneManyInPlace(allData)

  /**
    * $unionDesc $mutableAction
    *
    * @param that
    *   $unionParmThat
    */
  infix def union(that: DimensionalMultiBase[V, D]): Unit = merge(that, _ ++ _)

  /**
    * Same as [[union]].
    *
    * $unionDesc $mutableAction
    *
    * @param that
    *   $unionParmThat
    */
  infix def ∪(that: DimensionalMultiBase[V, D]): Unit = union(that)

  /**
    * $intersectionDesc $mutableAction
    *
    * @param that
    *   $intersectionParamThat
    */
  infix def intersection(that: DimensionalMultiBase[V, D]): Unit = transactionalUpdateWith(that): thatTx =>
    replaceValidData(zipData(that, thatTx, _ intersect _).filterNot(_.value.isEmpty))

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc $mutableAction
    *
    * @param that
    *   $intersectionParamThat
    */
  infix def ∩(that: DimensionalMultiBase[V, D]): Unit = intersection(that)

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataMulti because the resultant value type isn't necessarily a Set type) ----

  override def copy(using config: CoreConfig[D]): DataMulti[V, D] =
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
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  )(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): DataMulti[V, Domain.NonEmptyTail[D]] = transactionalRead:
    val result = DataMulti(getByHeadDimensionData(domain))(using config = altConfig)
    result.compressedUpdate()
    result

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMulti[V, R] = transactionalRead:
    val result = DataMulti(getByDimensionData(dimensionIndex, domain))(using config = altConfig)
    result.compressedUpdate()
    result

  override def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mergeValues: (Set[V], Set[V]) => Set[V]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMulti[V, R] = transactionalRead:
    val result = DataMulti.empty[V, R]
    result.mergeMany(getAllInternal.map(d => d.interval.dropDimension(dimensionIndex) -> d.value), mergeValues)
    result

  override def flattenDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMulti[V, R] = collapseDimension(dimensionIndex, _ ++ _)

  override def extrudeDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R]
  ): DataMulti[V, R] = transactionalRead:
    val result = DataMulti(extrudeDimensionData(dimensionIndex, extent))(using config = altConfig)
    result.compressedUpdate()
    result

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    this

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] = transactionalRead:
    intervalidus.immutable.DataMulti(getAllInternal)
