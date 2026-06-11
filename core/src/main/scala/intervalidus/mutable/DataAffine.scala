package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalBase.State
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
      * Creates a mutable affine structure from a non-affine structure in an affine domain.
      *
      * @return
      *   A new mutable affine structure with the same valid values.
      */
    def asDataAffine: DataAffine[V, D] = new DataAffine(data.stateCopy)(using config = data.config)

  /**
    * Automatically converts a non-affine structure in an affine domain to a mutable affine structure.
    */
  given [V, D <: NonEmptyTuple: DomainAffineLike]: Conversion[DimensionalBase[V, D], DataAffine[V, D]] = _.asDataAffine

/**
  * Mutable dimensional data where intervals exist in an affine domain.
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataAffine[V, D <: NonEmptyTuple: DomainAffineLike] private (
  override val initialState: State[V, D]
)(using val config: CoreConfig[D])
  extends MutableBase[V, D]
  with DimensionalAffineBase[V, D]:
  /**
    * $scaledAboutInDesc $mutableAction
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
  ): Unit = transactionalUpdate:
    if scaledBy != dimOp.identityScalar then
      val resultBuffer = DataAffine.empty[V, D]
      resultBuffer.setMany(scaled1dData(dimensionIndex, center, scaledBy))
      replaceValidData(resultBuffer.getAll)

  /**
    * $reflectedAboutInDesc $mutableAction
    *
    * @param dimensionIndex
    *   $reflectedAboutInParmDimensionIndex
    * @param pivot
    *   $reflectedAboutInParmPivot
    * @tparam H
    *   $reflectedAboutInTParmH
    */
  def reflectedAboutIn[H: DomainAffineValueLike](
    dimensionIndex: Domain.DimensionIndex,
    pivot: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): Unit =
    collectIntervals(maybeReflected1d(dimensionIndex, pivot).unlift)

  /**
    * $displacedByInDesc $mutableAction
    *
    * @param dimensionIndex
    *   $displacedByInParmDimensionIndex
    * @param offset
    *   $displacedByInParmOffset
    * @tparam H
    *   $displacedByInTParmH
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
  ): Unit =
    if offset != dimOp.zeroDisplacement then collectIntervals(maybeDisplaced1d(dimensionIndex, offset).unlift)

  /**
    * $convolvedByInDesc1 $mutableAction
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
  ): Unit = transactionalUpdate:
    val result = convolvedInternal(kernel, kernelOrigin, epsilon, accumulate): (kernelValue, offset, delta) =>
      val offsetLayer = copy
      offsetLayer.displacedByIn(dimensionIndex, offset)
      offsetLayer.mapValues(signalValue => scaledByEpsilon(combine(signalValue, kernelValue), delta))
      offsetLayer
    replaceValidData(result.getAll)

  /**
    * $reflectedAboutDesc $mutableAction
    *
    * @param pivot
    *   $reflectedAboutParmPivot
    */
  def reflectedAbout(pivot: D): Unit =
    collectIntervals(maybeReflected(pivot).unlift)

  /**
    * $scaledAboutDesc $mutableAction
    * @note
    *   $scaledAboutNote
    * @param center
    *   $scaledAboutParamCenter
    * @param scaledBy
    *   $scaledAboutParamScaledBy
    * @tparam S
    *   $scaledAboutTParamS
    */
  def scaledAbout[S <: NonEmptyTuple](center: D, scaledBy: S)(using D HasScalarType S): Unit = transactionalUpdate:
    val resultBuffer = DataAffine.empty[V, D]
    resultBuffer.setMany(scaledData(center, scaledBy))
    replaceValidData(resultBuffer.getAll)

  /**
    * $displacedByDesc $mutableAction
    *
    * @param offset
    *   $displacedByParmOffset
    * @tparam S
    *   $displacedByTParmS
    */
  def displacedBy[S <: NonEmptyTuple](offset: S)(using D HasDisplacementType S): Unit =
    collectIntervals(maybeDisplaced(offset).unlift)

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataAffine because the resultant domain isn't necessarily an affine) ----

  override def copy(using config: CoreConfig[D]): DataAffine[V, D] =
    new DataAffine(state.copy)

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
    val result = Data(getByHeadDimensionData(domain))(using config = altConfig)
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
    val result = Data(getByDimensionData(dimensionIndex, domain))(using config = altConfig)
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

  override def toMutable: intervalidus.mutable.DataAffine[V, D] =
    this

  override def toImmutable: intervalidus.immutable.DataAffine[V, D] =
    intervalidus.immutable.DataAffine.asDataAffine(this)
