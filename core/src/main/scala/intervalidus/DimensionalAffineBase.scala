package intervalidus

import scala.language.implicitConversions

/**
  * Constructs dimensional data where intervals are based on an affine domain.
  *
  * @tparam Constructed
  *   Constructed type.
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the affine domain type -- a non-empty tuple that is DomainAffineLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
trait DimensionalAffineBaseObject[Constructed[_, _ <: NonEmptyTuple] <: DimensionalAffineBase[?, ?]]:

  // ---------- Abstract ----------

  /**
    * Constructor for multiple initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid values within intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainAffineLike](
    initialData: Iterable[ValidData[V, D]]
  )(using config: CoreConfig[D]): Constructed[V, D]

  // ---------- Concrete ----------

  /**
    * Constructor where no values are valid. The empty set.
    *
    * @param config
    *   $configParam
    * @tparam D
    *   $intervalDomainType
    */
  def empty[V, D <: NonEmptyTuple: DomainAffineLike](using config: CoreConfig[D]): Constructed[V, D] =
    apply(Iterable.empty)

  /**
    * Same as [[empty]]
    *
    * Constructor where no values are valid. The empty set.
    *
    * @param config
    *   $configParam
    */
  def ∅[V, D <: NonEmptyTuple: DomainAffineLike](using config: CoreConfig[D]): Constructed[V, D] = empty

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @param value
    *   value that is valid in the full affine domain (`Interval.unbounded[D]`).
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainAffineLike](value: V)(using config: CoreConfig[D]): Constructed[V, D] =
    of(Interval.unbounded[D] -> value)

  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @param data
    *   value valid within an interval.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainAffineLike](
    data: ValidData[V, D]
  )(using config: CoreConfig[D]): Constructed[V, D] = apply(Iterable.single(data))

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V, D <: NonEmptyTuple: DomainAffineLike](using
    config: CoreConfig[D]
  ): scala.collection.mutable.Builder[ValidData[V, D], Constructed[V, D]] =
    ValidData.Builds[V, D, Constructed[V, D]](apply(_))

/**
  * Base for all dimensional data, both mutable and immutable, where intervals exist in an affine domain.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam D
  *   the affine domain type -- a non-empty tuple that is DomainAffineLike.
  *
  * @define intervalDomainType
  *   the affine domain type -- a non-empty tuple that is DomainAffineLike.
  * @define reflectedAboutInDesc
  *   Reflect this structure about a pivot in a specific dimension. Data in intervals that are no longer valid after
  *   reflection are dropped.
  * @define reflectedAboutInParmDimensionIndex
  *   The dimension being reflected.
  * @define reflectedAboutInParmPivot
  *   The anchor of the reflection.
  * @define reflectedAboutInTParmH
  *   The domain value type of the dimension being reflected.
  * @define reflectedAboutDesc
  *   Reflect this structure about a pivot in all dimensions. Data in intervals that are no longer valid after
  *   reflection are dropped.
  * @define reflectedAboutParmPivot
  *   The anchor of the reflection.
  * @define displacedByInDesc
  *   Displace this structure by some offset in a specific dimension. Data in intervals that are no longer valid after
  *   displacement are dropped.
  * @define displacedByInParmDimensionIndex
  *   The dimension being displaced.
  * @define displacedByInParmOffset
  *   The offset by which this will be displaced.
  * @define displacedByInTParmH
  *   The domain value type of the dimension being displaced.
  * @define displacedByDesc
  *   Displace this structure by a tuple of offsets. Data in intervals that are no longer valid after displacement are
  *   dropped.
  * @define displacedByParmOffset
  *   The offset tuple by which this will be displaced in all dimensions.
  * @define displacedByTParmS
  *   The tuple type for the offsets -- must match the Displacement types in all dimensions of the affine domain.
  * @define convolvedByInDesc1
  *   Convolve this structure using a one-dimensional kernel in a specific dimension.
  * @define convolvedByInDesc2
  *   Mathematically, a convolution is the infinite integral of a signal function blended with the reflection of a
  *   kernel function as it "slides" continuously over it. Because the underlying dimensional data can be thought of as
  *   piecewise-constant functions mapping intervals to values, this method approximates the continuous "sliding" using
  *   Riemann sums over each independent piece shifted in small steps (epsilon).
  * @define convolvedByInDesc3
  *   The implementation is fully compression-invariant and dynamically scales the final step of each kernel interval to
  *   adaptively match fractional interval boundaries without numerical drift.
  * @define convolvedByInDesc4
  *   See [[https://en.wikipedia.org/wiki/Convolution]] and [[https://en.wikipedia.org/wiki/Riemann_sum]]
  * @define convolvedByInParmDimensionIndex
  *   The dimension being convolved.
  * @define convolvedByInParmKernel
  *   The convolution kernel (a one-dimensional affine structure).
  * @define convolvedByInParmKernelOrigin
  *   The reflection pivot of the kernel (usually its center). Because the kernel is reflected, kernelOrigin positions
  *   are also mirrored (e.g., treating the end of the kernel as the origin moves the output forward).
  * @define convolvedByInParmEpsilon
  *   The integration step size, defining the maximum measure of the Riemann sum elements.
  * @define convolvedByInParmCombine
  *   The multiplication function combining the signal value with the kernel value, determining the unscaled height of
  *   the Riemann sum elements.
  * @define convolvedByInParmScaleByEpsilon
  *   Scales combined values by their step measure, computing the physical area of Riemann sum elements.
  * @define convolvedByInParmAccumulate
  *   The addition function used to merge intermediate spatial layers into the final buffer.
  * @define convolvedByInTParamH
  *   The domain value type of the dimension being convolved.
  * @define convolvedByInTParamK
  *   The value type of the kernel.
  * @define scaledAboutDesc
  *   Scale this structure about a center point by some scalar factor all dimensions. Data in intervals that are no
  *   longer valid after scaling are dropped.
  * @define scaledAboutNote
  *   Because of rounding, shrinking a shape using a small scaling factor may cause resultant intervals to overlap with
  *   one another. Therefore, this doesn't just map intervals, it consolidates them, resolving any conflicts with a
  *   "last write wins" strategy for any overlaps. That is, during consolidation, because data are processed in
  *   `interval.start` order, elements starting further to the "right" in each affected dimension will generally
  *   overwrite elements starting further to the "left" in areas where there are spatial collisions.
  * @define scaledAboutParamCenter
  *   The homothetic center of scaling (see [[https://en.wikipedia.org/wiki/Homothetic_center]]).
  * @define scaledAboutParamScaledBy
  *   A tuple of scaling factors for each dimension. Each can be positive or negative.
  * @define scaledAboutTParamS
  *   The tuple type for the scaling factors -- must match the Scalar types in all dimensions of the affine domain.
  * @define scaledAboutInDesc
  *   Scale this structure about a center point by some scalar factor in a specific dimension. Data in intervals that
  *   are no longer valid after scaling are dropped.
  * @define scaledAboutInParamDimensionIndex
  *   The dimension being scaled.
  * @define scaledAboutInParamCenter
  *   The homothetic center of scaling (see [[https://en.wikipedia.org/wiki/Homothetic_center]]).
  * @define scaledAboutInParamScaledBy
  *   The scaling factor. Can be positive or negative.
  * @define scaledAboutInTParamH
  *   The domain value type of the dimension being scaled.
  */
trait DimensionalAffineBase[V, D <: NonEmptyTuple: DomainAffineLike] extends DimensionalBase[V, D]:
  import DomainAffineLike.* // extension methods
  import Domain.{HasDisplacementType, HasScalarType}

  /**
    * Returns data with intervals scaled in one dimension. Only data in intervals that remain valid after scaling are
    * returned.
    * @param dimensionIndex
    *   $scaledAboutInParamDimensionIndex
    * @param center
    *   $scaledAboutInParamCenter
    * @param scaledBy
    *   $scaledAboutInParamScaledBy
    * @tparam H
    *   $scaledAboutInTParamH
    */
  def scaled1dData[H](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    center: Domain1D[H],
    scaledBy: dimOp.Scalar
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): Iterable[ValidData[V, D]] =
    getAll.flatMap: d =>
      val interval1D: Interval1D[H] = d.interval.apply[H](dimensionIndex)
      val scaled1D = interval1D.scaledAbout(center, scaledBy)
      scaled1D.map(s => d.copy(interval = d.interval.withDimensionUpdate[H](dimensionIndex, _ => s)))

  /**
    * Returns data with scaled intervals. Only data in intervals that remain valid after scaling are returned.
    * @param center
    *   $scaledAboutParamCenter
    * @param scaledBy
    *   $scaledAboutParamScaledBy
    * @tparam S
    *   $scaledAboutTParamS
    */
  protected def scaledData[S <: NonEmptyTuple](
    center: D,
    scaledBy: S
  )(using D HasScalarType S): Iterable[ValidData[V, D]] =
    getAll.flatMap: d =>
      d.interval.scaledAbout(center, scaledBy).map(scaledInterval => d.copy(interval = scaledInterval))

  /**
    * Builds a function returning option that can be unlifted into a partial function for collecting intervals reflected
    * in one dimension. The partial function will only be defined on values where reflectedAbout succeeds, i.e.,
    * interval does not fall over the edge of the domain.
    * @param dimensionIndex
    *   $reflectedAboutInParmDimensionIndex
    * @param pivot
    *   $reflectedAboutInParmPivot
    * @tparam H
    *   $reflectedAboutInTParmH
    */
  protected def maybeReflected1d[H](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    pivot: Domain1D[H]
  )(
    interval: Interval[D]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): Option[Interval[D]] = interval(dimensionIndex)
    .reflectedAbout(pivot) // displacedBy fails (returns None) if the interval shifts outside the domain entirely
    .map(reflected1D => interval.withDimensionUpdate[H](dimensionIndex, _ => reflected1D))

  /**
    * Builds a function returning option that can be unlifted into a partial function for collecting reflected
    * intervals. The partial function will only be defined on values where reflectedAbout succeeds, i.e., interval does
    * not fall over the edge of the domain.
    *
    * @param pivot
    *   $reflectedAboutParmPivot
    */
  protected def maybeReflected(pivot: D)(
    interval: Interval[D]
  ): Option[Interval[D]] = interval.reflectedAbout(pivot)

  /**
    * Builds a function returning option that can be unlifted into a partial function for collecting intervals displaced
    * in one dimension. The partial function will only be defined on values where displacedBy succeeds, i.e., interval
    * does not fall over the edge of the domain.
    *
    * @param dimensionIndex
    *   $displacedByInParmDimensionIndex
    * @param offset
    *   $displacedByInParmOffset
    * @tparam H
    *   $displacedByInTParmH
    */
  protected def maybeDisplaced1d[H](using
    dimOp: DomainAffineValueLike[H]
  )(
    dimensionIndex: Domain.DimensionIndex,
    offset: dimOp.Displacement
  )(
    interval: Interval[D]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
  ): Option[Interval[D]] = interval(dimensionIndex)
    .displacedBy(offset) // displacedBy fails (returns None) if the interval shifts outside the domain entirely
    .map(displaced1D => interval.withDimensionUpdate[H](dimensionIndex, _ => displaced1D))

  /**
    * Builds a function returning option that can be unlifted into a partial function for collecting displaced
    * intervals. The partial function will only be defined on values where displacedBy succeeds, i.e., interval does not
    * fall over the edge of the domain.
    * @param offset
    *   $displacedByParmOffset
    * @tparam S
    *   $displacedByTParmS
    */
  protected def maybeDisplaced[S <: NonEmptyTuple](offset: S)(
    interval: Interval[D]
  )(using D HasDisplacementType S): Option[Interval[D]] = interval.displacedBy(offset)

  /**
    * Common logic for both mutable and immutable convolutions. Combines this affine space with a one-dimensional kernel
    * along a specific dimension index using a function that creates layers for each kernel element displacement. Rather
    * than sliding the kernel's reflection over the signal, we actually slide the signal over the kernel, and accumulate
    * those results.
    *
    * @param kernel
    *   $convolvedByInParmKernel
    * @param kernelOrigin
    *   $convolvedByInParmKernelOrigin
    * @param epsilon
    *   $convolvedByInParmEpsilon
    * @param accumulate
    *   $convolvedByInParmAccumulate
    * @tparam H
    *   $convolvedByInTParamH
    * @tparam K
    *   $convolvedByInTParamK
    * @return
    *   A mutable structure for additional processing.
    */
  protected def convolvedInternal[H, K](using
    dimOp: DomainAffineValueLike[H]
  )(
    kernel: DimensionalAffineBase[K, Domain.In1D[H]],
    kernelOrigin: Domain1D[H],
    epsilon: dimOp.Displacement,
    accumulate: (V, V) => V
  )(
    offsetLayer: (K, dimOp.Displacement, dimOp.Displacement) => DimensionalAffineBase[V, D]
  ): mutable.DataAffine[V, D] =
    val delayCompression: CoreConfig[D] = config.withCompressOnUpdate(false)
    val resultBuffer: mutable.DataAffine[V, D] = mutable.DataAffine.empty(using config = delayCompression)
    kernel.getAll.foreach: kernelElement =>
      val kernelInterval1D = kernelElement.interval.headInterval1D

      def toOriginFrom(f: Interval1D[H] => Domain1D[H]) = (f(kernelInterval1D) displacementTo kernelOrigin).getOrElse:
        throw IllegalArgumentException(s"Invalid kernel element / origin: $kernelElement / $kernelOrigin")

      /**
        * Why the heck does this use `end.rightAdjacent` instead of just `end`? This is explained in a comment in
        * [[DomainAffineValueLike.measure]].
        */
      val offsetsAndDeltas = dimOp.range(toOriginFrom(_.start), toOriginFrom(_.end.rightAdjacent), epsilon.negated)
      offsetsAndDeltas.iterator.foreach: (offset, delta) =>
        resultBuffer.merge(offsetLayer(kernelElement.value, offset, delta.magnitude), accumulate)

    if config.compressOnUpdate then resultBuffer.compressAll()
    resultBuffer

  /**
    * The shape forming a shell around the boundary of all valid data. The shell's thickness and direction are
    * determined per dimension by the given displacement vector. The resulting shell represents the symmetric difference
    * of the original shape and the same shape padded with the provided thickness.
    *
    * When all thickness components are positive (the common case), the resulting shape will be adjacent to valid data
    * everywhere, but not intersecting it anywhere.
    *
    * @note
    *   If data are valid on the boundaries of the domain in any dimension, the resulting shell may not be contiguous.
    * @param thickness
    *   A tuple of displacements representing the shell's thickness in each dimension. Positive values expand the
    *   boundaries outward, while negative values contract them inward. Dimensions can mix positive and negative
    *   displacements.
    * @return
    *   A shape that forms a shell around the boundary of all valid data.
    */
  def boundingShape[S <: NonEmptyTuple](thickness: S)(using D HasDisplacementType S): IntervalShape[D] =
    val paddedIntervals = allIntervals.flatMap(_.paddedBy(thickness))
    (IntervalShape.∅ ++ paddedIntervals) △ domain
