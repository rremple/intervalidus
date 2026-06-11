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

  extension [V, D <: NonEmptyTuple: DomainLike](data: DimensionalBase[Set[V], D])
    /**
      * Creates an immutable muti-value structure from a non-multi structure managing sets of values.
      *
      * @return
      *   A new immutable muti-value structure with the same valid values.
      */
    def asDataMulti: DataMulti[V, D] = new DataMulti(data.stateCopy)(using config = data.config)

  /**
    * Automatically converts a non-multi structure managing sets of values to an immutable multi-value structure.
    */
  given [V, D <: NonEmptyTuple: DomainLike]: Conversion[DimensionalBase[Set[V], D], DataMulti[V, D]] = _.asDataMulti

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
  override val initialState: State[Set[V], D]
)(using val config: CoreConfig[D])
  extends ImmutableBase[Set[V], D, DataMulti[V, D]]
  with DimensionalMultiBase[V, D]:

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
    copyAndModify(_.removeOneManyInPlace(allData))

  /**
    * $unionDesc
    *
    * @param that
    *   $unionParmThat
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def union(that: DimensionalMultiBase[V, D]): DataMulti[V, D] = merge(that, _ ++ _)

  /**
    * Same as [[union]].
    *
    * $unionDesc
    *
    * @param that
    *   $unionParmThat
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def ∪(that: DimensionalMultiBase[V, D]): DataMulti[V, D] = union(that)

  /**
    * $intersectionDesc
    *
    * @param that
    *   $intersectionParamThat
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def intersection(that: DimensionalMultiBase[V, D]): DataMulti[V, D] = transactionalReadWith(that): thatTx =>
    DataMulti(zipData(that, thatTx, _ intersect _).filterNot(_.value.isEmpty))

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc
    *
    * @param that
    *   $intersectionParamThat
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def ∩(that: DimensionalMultiBase[V, D]): DataMulti[V, D] = intersection(that)

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  // ----  (some return Data rather than DataMulti because the resultant value type isn't necessarily a Set type) ----

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    mapInternal(f)

  protected def mapInternal[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S])(using Transaction[Set[V], D]): Data[B, S] =
    Data(
      getAllInternal.map(f)
    )(using config = altConfig).compressedUpdate()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.flatMap(f(_).getAll)
    )(using config = altConfig).compressedUpdate()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[Set[V], D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.collect(pf)
    )(using config = altConfig).compressedUpdate()

  override def mapValues[B](
    f: Set[V] => B
  ): Data[B, D] = transactionalRead:
    mapInternal(d => d.copy(value = f(d.value)))

  override def collectValues[B](
    pf: PartialFunction[Set[V], B]
  ): Data[B, D] = transactionalRead:
    Data(collectValuesData(pf)).compressedUpdate()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): DataMulti[V, S] = transactionalRead:
    DataMulti(
      getAllInternal.map(d => d.copy(interval = f(d.interval)))
    )(using config = altConfig).compressedUpdate()

  override def collectIntervals[S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[Interval[D], Interval[S]]
  )(using altConfig: CoreConfig[S]): DataMulti[V, S] = transactionalRead:
    DataMulti(collectIntervalsData(pf))(using config = altConfig).compressedUpdate()

  // ---------- Implement methods from DimensionalBase and DimensionalMultiBase that create new instances ----------
  // ----  (some return Data rather than DataMulti because the resultant value type isn't necessarily a Set type) ----

  override protected def copyInternal(using tx: Transaction[Set[V], D])(using CoreConfig[D]): DataMulti[V, D] =
    new DataMulti(tx.state.copy)

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
    DataMulti(getByHeadDimensionData(domain))(using config = altConfig).compressedUpdate()

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
    DataMulti(getByDimensionData(dimensionIndex, domain))(using config = altConfig).compressedUpdate()

  override def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mergeValues: (Set[V], Set[V]) => Set[V]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMulti[V, R] = transactionalRead:
    DataMulti
      .empty[V, R]
      .mergeMany(getAllInternal.map(d => d.interval.dropDimension(dimensionIndex) -> d.value), mergeValues)

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
    DataMulti(extrudeDimensionData(dimensionIndex, extent))(using config = altConfig).compressedUpdate()

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    intervalidus.mutable.DataMulti.asDataMulti(this)

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] =
    this
