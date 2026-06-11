package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalBase.{State, Transaction}
import intervalidus.Domain.{HasDisplacementType, HasScalarType}

import scala.language.implicitConversions

/**
  * Constructs dimensional data where intervals exist in an affine domain.
  */
object DataAffine extends DimensionalAffineBaseObject[DataAffine]:
  type In1D[V, R1] = DataAffine[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataAffine[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataAffine[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataAffine[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V, D <: NonEmptyTuple: DomainAffineLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using config: CoreConfig[D]): DataAffine[V, D] =
    new DataAffine(State.from(initialData))

  extension [V, D <: NonEmptyTuple: DomainAffineLike](data: DimensionalBase[V, D])
    /**
      * Creates an immutable affine structure from a non-affine structure in an affine domain.
      *
      * @return
      *   A new immutable affine structure with the same valid values.
      */
    def asDataAffine: DataAffine[V, D] = new DataAffine(data.stateCopy)(using config = data.config)

  /**
    * Automatically converts a non-affine structure in an affine domain to an immutable affine structure.
    */
  given [V, D <: NonEmptyTuple: DomainAffineLike]: Conversion[DimensionalBase[V, D], DataAffine[V, D]] = _.asDataAffine

/**
  * Immutable dimensional data where intervals exist in an affine domain.
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataAffine[V, D <: NonEmptyTuple] private (
  override val initialState: State[V, D]
)(using val config: CoreConfig[D], domainAffineLike: DomainAffineLike[D])
  extends ImmutableBase[V, D, DataAffine[V, D]]
  with DimensionalAffineBase[V, D]:
  /**
    * $scaledAboutInDesc
    * @note
    *   $scaledAboutNote
    * @param dimensionIndex
    *   $scaledAboutInParamDimensionIndex
    * @param center
    *   $scaledAboutInParamCenter
    * @param scaledBy
    *   $scaledAboutInParamScaledBy
    * @tparam H
    *   $scaledAboutInTParamH
    * @return
    *   A new structure representing this scaled about the center.
    */
  def scaledAboutIn[H](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    center: Domain1D[H],
    scaledBy: dimOp.Scalar
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): DataAffine[V, D] =
    if scaledBy == dimOp.identityScalar then this
    else DataAffine.empty[V, D].setMany(scaled1dData(dimensionIndex, center, scaledBy))

  /**
    * $reflectedAboutInDesc
    *
    * @param dimensionIndex
    *   $reflectedAboutInParmDimensionIndex
    * @param pivot
    *   $reflectedAboutInParmPivot
    * @tparam H
    *   $reflectedAboutInTParmH
    * @return
    *   A new structure representing this reflected about the pivot.
    */
  def reflectedAboutIn[H: DomainAffineValueLike](
    dimensionIndex: Domain.DimensionIndex,
    pivot: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): DataAffine[V, D] =
    collectIntervals(maybeReflected1d(dimensionIndex, pivot).unlift)

  /**
    * $displacedByInDesc
    *
    * @param dimensionIndex
    *   $displacedByInParmDimensionIndex
    * @param offset
    *   $displacedByInParmOffset
    * @tparam H
    *   $displacedByInTParmH
    * @return
    *   A new structure representing this displaced by the offset.
    */
  def displacedByIn[H](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    offset: dimOp.Displacement
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): DataAffine[V, D] =
    if offset == dimOp.zeroDisplacement then this
    else collectIntervals(maybeDisplaced1d(dimensionIndex, offset).unlift)

  /**
    * $convolvedByInDesc1
    *
    * $convolvedByInDesc2
    *
    * $convolvedByInDesc3
    *
    * $convolvedByInDesc4
    *
    * @param dimensionIndex
    *   $convolvedByInParmDimensionIndex
    * @param kernel
    *   $convolvedByInParmKernel
    * @param kernelOrigin
    *   $convolvedByInParmKernelOrigin
    * @param epsilon
    *   $convolvedByInParmEpsilon
    * @param combine
    *   $convolvedByInParmCombine
    * @param scaledByEpsilon
    *   $convolvedByInParmScaleByEpsilon
    * @param accumulate
    *   $convolvedByInParmAccumulate
    * @tparam H
    *   $convolvedByInTParamH
    * @tparam K
    *   $convolvedByInTParamK
    * @return
    *   A new structure representing the convolution of this with the supplied kernel.
    */
  def convolvedByIn[H, K](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    kernel: DataAffine[K, Domain.In1D[H]],
    kernelOrigin: Domain1D[H],
    epsilon: dimOp.Displacement,
    combine: (V, K) => V,
    scaledByEpsilon: (V, dimOp.Displacement) => V,
    accumulate: (V, V) => V
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): DataAffine[V, D] =
    convolvedInternal(kernel, kernelOrigin, epsilon, accumulate): (kernelValue, offset, delta) =>
      displacedByIn(dimensionIndex, offset).mapValues: signalValue =>
        scaledByEpsilon(combine(signalValue, kernelValue), delta)
    .toImmutable

  /**
    * $reflectedAboutDesc
    *
    * @param pivot
    *   $reflectedAboutParmPivot
    * @return
    *   A new structure representing this reflected about the pivot.
    */
  def reflectedAbout(pivot: D): DataAffine[V, D] =
    collectIntervals(maybeReflected(pivot).unlift)

  /**
    * $scaledAboutDesc
    * @note
    *   $scaledAboutNote
    * @param center
    *   $scaledAboutParamCenter
    * @param scaledBy
    *   $scaledAboutParamScaledBy
    * @tparam S
    *   $scaledAboutTParamS
    * @return
    *   A new structure representing this scaled about the center.
    */
  def scaledAbout[S <: NonEmptyTuple](center: D, scaledBy: S)(using D HasScalarType S): DataAffine[V, D] =
    DataAffine.empty[V, D].setMany(scaledData(center, scaledBy))

  /**
    * $displacedByDesc
    *
    * @param offset
    *   $displacedByParmOffset
    * @tparam S
    *   $displacedByTParmS
    * @return
    *   A new structure representing this displaced by the offset.
    */
  def displacedBy[S <: NonEmptyTuple](offset: S)(using D HasDisplacementType S): DataAffine[V, D] =
    collectIntervals(maybeDisplaced(offset).unlift)

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  // ---  (some return Data rather than DataAffine because the resultant domain isn't necessarily an affine domain) ---

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    mapInternal(f)

  protected def mapInternal[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S])(using Transaction[V, D]): Data[B, S] =
    Data(
      getAllInternal.map(f)
    )(using config = altConfig).compressedUpdate()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.flatMap(f(_).getAll)
    )(using config = altConfig).compressedUpdate()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.collect(pf)
    )(using config = altConfig).compressedUpdate()

  override def mapValues[B](
    f: V => B
  ): DataAffine[B, D] = transactionalRead:
    DataAffine(
      getAllInternal.map(d => d.copy(value = f(d.value)))
    ).compressedUpdate()

  override def collectValues[B](pf: PartialFunction[V, B]): DataAffine[B, D] = transactionalRead:
    DataAffine(collectValuesData(pf)).compressedUpdate()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): Data[V, S] = transactionalRead:
    Data(
      getAllInternal.map(d => d.copy(interval = f(d.interval)))
    )(using config = altConfig).compressedUpdate()

  override def collectIntervals[S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[Interval[D], Interval[S]]
  )(using altConfig: CoreConfig[S]): Data[V, S] = transactionalRead:
    Data(collectIntervalsData(pf)).compressedUpdate()

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataAffine because the resultant domain isn't necessarily an affine) ----

  override protected def copyInternal(using tx: Transaction[V, D])(using CoreConfig[D]): DataAffine[V, D] =
    new DataAffine(tx.state.copy)

  override def zip[B](that: DimensionalBase[B, D]): DataAffine[(V, B), D] = transactionalReadWith(that): thatTx =>
    DataAffine(zipData(that, thatTx, (_, _)))

  override def zipAll[B](
    that: DimensionalBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): DataAffine[(V, B), D] = transactionalReadWith(that): thatTx =>
    DataAffine(zipAllData(that, thatTx, thisDefault, thatDefault, (_, _)))

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

  override def toMutable: intervalidus.mutable.DataAffine[V, D] =
    intervalidus.mutable.DataAffine.asDataAffine(this)

  override def toImmutable: intervalidus.immutable.DataAffine[V, D] =
    this
