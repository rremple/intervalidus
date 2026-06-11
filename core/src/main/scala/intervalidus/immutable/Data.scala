package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalBase.{Transaction, State}

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
      * Creates a general immutable dimensional data structure from a some other dimensional structure.
      *
      * @return
      *   A new immutable structure with the same valid values.
      */
    def asData: Data[V, D] = new Data(data.stateCopy)(using config = data.config)

  /**
    * Automatically converts some other dimensional structure to a general immutable dimensional data structure.
    */
  given [V, D <: NonEmptyTuple: DomainLike]: Conversion[DimensionalBase[V, D], Data[V, D]] = _.asData

/**
  * Immutable dimensional data.
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
  extends ImmutableBase[V, D, Data[V, D]]:

  config.experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = transactionalRead:
      require(Interval.isDisjoint(getAllInternal.map(_.interval)), "data must be disjoint")
  )

  // ---------- Implement methods from ImmutableBase that create new instances ----------

  override protected def copyInternal(using tx: Transaction[V, D])(using CoreConfig[D]): Data[V, D] =
    new Data(tx.state.copy)

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    mapInternal(f)

  protected def mapInternal[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S])(using Transaction[V, D]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.map(f)
    )(using config = altConfig).compressedUpdate()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.collect(pf)
    )(using config = altConfig).compressedUpdate()

  override def mapValues[B](
    f: V => B
  ): Data[B, D] = transactionalRead:
    mapInternal(d => d.copy(value = f(d.value)))

  override def collectValues[B](
    pf: PartialFunction[V, B]
  ): Data[B, D] = transactionalRead:
    Data(collectValuesData(pf)).compressedUpdate()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): Data[V, S] = transactionalRead:
    mapInternal(d => d.copy(interval = f(d.interval)))(using altConfig = altConfig)

  override def collectIntervals[S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[Interval[D], Interval[S]]
  )(using altConfig: CoreConfig[S]): Data[V, S] = transactionalRead:
    Data(collectIntervalsData(pf))(using config = altConfig).compressedUpdate()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.flatMap(f(_).getAll)
    )(using config = altConfig).compressedUpdate()

  // ---------- Implement methods from DimensionalBase that create new instances ----------

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
    Data(getByHeadDimensionData(domain))(using config = altConfig).compressedUpdate()

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
    Data(getByDimensionData(dimensionIndex, domain))(using config = altConfig).compressedUpdate()

  override def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mergeValues: (V, V) => V
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): Data[V, R] = transactionalRead:
    Data
      .empty[V, R]
      .mergeMany(getAllInternal.map(d => d.interval.dropDimension(dimensionIndex) -> d.value), mergeValues)

  override def extrudeDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R]
  ): Data[V, R] = transactionalRead:
    Data(extrudeDimensionData(dimensionIndex, extent))(using config = altConfig).compressedUpdate()

  override def toMutable: intervalidus.mutable.Data[V, D] =
    intervalidus.mutable.Data.asData(this)

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    this
