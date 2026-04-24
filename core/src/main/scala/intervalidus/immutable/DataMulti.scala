package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalBase.{Transaction, State}

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
  )(using config: CoreConfig[D]): DataMulti[V, D] = empty[V, D].addOneMany(initialData)

/**
  * Immutable, multivalued dimensional data.
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
  extends ImmutableBase[Set[V], D, DataMulti[V, D]]
  with DimensionalMultiBase[V, D]:

  @volatile override protected var state: State[Set[V], D] = initialState

  config.experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = transactionalRead:
      require(Interval.isDisjoint(getAllInternal.map(_.interval)), "data must be disjoint")
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
  def addOneMany(allData: IterableOnce[ValidData[V, D]]): DataMulti[V, D] =
    copyAndModify(_.addManyInPlace(allData))

  /**
    * $removeOneManyDesc [[removeOne]].
    *
    * @param allData
    *   $removeOneManyParamAllData
    * @return
    *   $immutableReturn
    */
  def removeOneMany(allData: IterableOnce[ValidData[V, D]]): DataMulti[V, D] =
    copyAndModify(_.removeManyInPlace(allData))

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  // ----  (some return Data rather than DataMulti because the resultant value type isn't necessarily a Set type) ----

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    mapInternal((f))

  protected def mapInternal[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S])(using Transaction[Set[V], D]): Data[B, S] =
    Data(
      getAllInternal.map(f)
    )(using config = altConfig).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.flatMap(f(_).getAll)
    )(using config = altConfig).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[Set[V], D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.collect(pf)
    )(using config = altConfig).compressAll()

  override def mapValues[B](
    f: Set[V] => B
  ): Data[B, D] = transactionalRead:
    mapInternal(d => d.copy(value = f(d.value)))

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): DataMulti[V, S] = transactionalRead:
    DataMulti(
      getAllInternal.map(d => d.copy(interval = f(d.interval)))
    )(using config = altConfig).compressAll()

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
    DataMulti(getByHeadDimensionData(domain))(using config = altConfig).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DataMulti[V, R] = transactionalRead:
    DataMulti(getByDimensionData(dimensionIndex, domain))(using config = altConfig).compressAll()

  override def toMutable: intervalidus.mutable.DataMulti[V, D] = transactionalRead:
    intervalidus.mutable.DataMulti(getAllInternal)

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] =
    this
