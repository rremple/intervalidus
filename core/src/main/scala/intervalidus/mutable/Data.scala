package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalBase.State

/**
  * $objectDesc
  */
object Data extends DimensionalBaseObject[Data]:

  type In1D[V, R1] = Data[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = Data[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = Data[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = Data[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using config: CoreConfig[D]): Data[V, D] =
    new Data(State.from(initialData))

  extension [V, D <: NonEmptyTuple: DomainLike](data: DimensionalBase[V, D])
    /**
      * Creates a general mutable dimensional data structure from a some other dimensional structure.
      *
      * @return
      *   A new mutable structure with the same valid values.
      */
    def asData: Data[V, D] = new Data(data.stateCopy)(using config = data.config)

  /**
    * Automatically converts some other dimensional structure to a general mutable dimensional data structure.
    */
  given [V, D <: NonEmptyTuple: DomainLike]: Conversion[DimensionalBase[V, D], Data[V, D]] = _.asData

/**
  * Mutable dimensional data.
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class Data[V, D <: NonEmptyTuple: DomainLike] private (
  override val initialState: State[V, D]
)(using val config: CoreConfig[D])
  extends MutableBase[V, D]:

  config.experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = transactionalRead:
      require(Interval.isDisjoint(getAllInternal.map(_.interval)), "data must be disjoint")
  )

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy(using config: CoreConfig[D]): Data[V, D] =
    new Data(state.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] = transactionalReadWith(that): thatTx =>
    Data(zipData(that, thatTx, (_, _)))

  override def zipAll[B](
    that: DimensionalBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): Data[(V, B), D] = transactionalReadWith(that): thatTx =>
    Data(zipAllData(that, thatTx, thisDefault, thatDefault, (_, _)))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  )(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): Data[V, Domain.NonEmptyTail[D]] = transactionalRead:
    val result = Data(getByHeadDimensionData(domain))
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
  ): Data[V, R] = transactionalRead:
    val result = Data(getByDimensionData(dimensionIndex, domain))
    result.compressedUpdate()
    result

  override def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mergeValues: (V, V) => V
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): Data[V, R] = transactionalRead:
    val result = Data.empty[V, R]
    result.mergeMany(getAllInternal.map(d => d.interval.dropDimension(dimensionIndex) -> d.value), mergeValues)
    result.compressedUpdate()
    result

  override def extrudeDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R]
  ): Data[V, R] = transactionalRead:
    val result = Data(extrudeDimensionData(dimensionIndex, extent))(using config = altConfig)
    result.compressedUpdate()
    result

  override def toMutable: intervalidus.mutable.Data[V, D] =
    this

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    intervalidus.immutable.Data.asData(this)
