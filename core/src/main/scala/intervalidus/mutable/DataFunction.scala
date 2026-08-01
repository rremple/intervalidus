package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalFunctionBase.*

/**
  * Constructs data consisting of piecewise functions in multidimensional intervals.
  */
object DataFunction extends DimensionalFunctionBaseObject[DataFunction]:
  type In1D[V, R1] = DataFunction[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataFunction[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataFunction[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataFunction[V, Domain.In4D[R1, R2, R3, R4]]

  override protected def fromData[V, D <: NonEmptyTuple: DomainLike](
    data: MutableFunctionData[V, D]
  ): DataFunction[V, D] = new DataFunction(data)

/**
  * Represents a mutable piecewise function defined over a multidimensional domain, mapping intervals to domain
  * functions.
  *
  * Because domain functions (`D => V`) are arbitrary executable closures, equality and lookup operations within
  * `DataFunction` that rely on the identity of these values (e.g., [[compress]], [[intervals]], and [[removeValue]])
  * ultimately depend on reference identity of functions (i.e., `eq`), not logical equivilance. Also functions created
  * through functional composition (e.g., `f.andThen(g)`) instantiate distinct object references on the heap even when
  * mathematically identical.
  *
  * {{{
  * type D = Domain.In1D[Double]
  * val f: FunctionValue[Double, D] = _ => 0.0
  * val f2: FunctionValue[Double, D] = _ => 0.0
  * f == f2 // returns false (although functionally equivilant, f and f2 are different objects)
  *
  * val g: Double => Double = _ + 1.0
  * val fg = f.andThen(g)
  * val fg2 = f.andThen(g)
  * fg == fg2 // returns false (although functionally equivilant, fg and fg2 are different objects)
  * }}}
  *
  * Consequently:
  *   - Transformation methods (e.g., [[mapValues]] and [[collectValues]]) that compose or instantiate new functions
  *     will produce distinct function references across intervals even if they were the same before transformation.
  *   - Adjacent intervals with transformed functions will not coalesce under [[compress]] unless the exact same
  *     transformed function instance is shared.
  *
  * Best Practices:
  *   1. Bind domain functions to stable `val` identifiers wherever identity is important.
  *   2. When querying or modifying existing pieces dynamically, reuse references retrieved directly from the structure
  *      (e.g., `df.getDataAt(pt).map(_.value)`).
  *
  * @tparam V
  *   the result type of the domain function managed as data.
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  */
class DataFunction[V, D <: NonEmptyTuple: DomainLike] private (
  initialData: MutableFunctionData[V, D]
) extends DimensionalFunctionBase[V, D](initialData):

  private def ofUnderlying[B, S <: NonEmptyTuple: DomainLike](
    data: MutableFunctionData[B, S]
  ): DataFunction[B, S] = DataFunction.fromData(data)

  // ---------- Implement methods from DimensionalFunctionBase that create new instances ----------

  override def copy(using CoreConfig[D]): DataFunction[V, D] = DataFunction.fromData(underlying.copy)

  override def zip[B](
    that: DimensionalFunctionBase[B, D]
  ): DataFunction[(V, B), D] = ofUnderlying(zipData(that))

  override def zipAll[B](
    that: DimensionalFunctionBase[B, D],
    thisDefault: DomainFunction[V, D],
    thatDefault: DomainFunction[B, D]
  ): DataFunction[(V, B), D] = ofUnderlying(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  )(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): DataFunction[V, Domain.NonEmptyTail[D]] = ofUnderlying(getByHeadDimensionData(domain))

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    Domain.IsInsertedInResult[R, dimensionIndex.type, H, D]
  ): DataFunction[V, R] = ofUnderlying(getByDimensionData(dimensionIndex, domain))

  override def collapseDimension[R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    mapFunctions: DomainFunction[V, D] => DomainFunction[V, R],
    mergeValues: (V, V) => V
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataFunction[V, R] = ofUnderlying(collapseDimensionData(dimensionIndex, mapFunctions, mergeValues))

  override def extrudeDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    extent: Interval1D[H]
  )(using
    altConfig: CoreConfig[R]
  )(using
    Domain.HasIndex[R, dimensionIndex.type],
    Domain.IsInsertedInResult[D, dimensionIndex.type, H, R],
    Domain.IsDroppedInResult[R, dimensionIndex.type, D]
  ): DataFunction[V, R] = ofUnderlying(extrudeDimensionData(dimensionIndex, extent))

  override def toImmutable: immutable.DataFunction[V, D] = immutable.DataFunction.asDataFunction(underlying)

  override def toMutable: DataFunction[V, D] = this

  // ---------- Implement methods like those in MutableBase that require something special ----------
  // ---------- (the API is slightly different than the underlying data API) ----------

  /**
    * $mapValuesDesc $mutableAction
    *
    * @param f
    *   $mapValuesParamF
    */
  def mapValues(f: V => V): Unit =
    underlying.mapValues(_.andThen(f))

  /**
    * Applies a function to all the elements of this structure and updates valid functions from the elements of the
    * resulting structures. $mutableAction
    *
    * @param f
    *   $flatMapParamF
    */
  def flatMap(f: ValidFunction[V, D] => DimensionalFunctionBase[V, D]): Unit =
    underlying.flatMap(f(_).underlying)

  /**
    * $syncWithDesc $mutableAction
    *
    * @param that
    *   $syncWithParamThat
    */
  def syncWith(that: DimensionalFunctionBase[V, D]): Unit =
    underlying.syncWith(that.underlying)

  /**
    * $mergeDesc $mutableAction
    *
    * @param that
    *   $mergeParamThat
    * @param mergeValues
    *   $mergeParamMergeValues
    */
  def merge(that: DimensionalFunctionBase[V, D], mergeValues: (V, V) => V): Unit =
    underlying.merge(that.underlying, _.merge(_, mergeValues))

  /**
    * $mergeManyDesc $mutableAction
    *
    * @param thatData
    *   $mergeManyParamThatData
    * @param mergeValues
    *   $mergeManyParamMergeValues
    */
  def mergeMany(
    thatData: IterableOnce[ValidFunction[V, D]],
    mergeValues: (V, V) => V
  ): Unit = underlying.mergeMany(thatData, _.merge(_, mergeValues))

  /**
    * $symmetricDifferenceDesc $mutableAction
    *
    * @param that
    *   $symmetricDifferenceParamThat
    */
  infix def symmetricDifference(that: DimensionalFunctionBase[V, D]): Unit =
    underlying.symmetricDifference(that.underlying)

  /**
    * $differenceDesc $mutableAction
    *
    * @param that
    *   $differenceParamThat
    */
  infix def difference(that: DimensionalFunctionBase[V, D]): Unit =
    underlying.difference(that.underlying)

  // equivalent symbolic method names

  /**
    * Same as [[symmetricDifference]].
    *
    * $symmetricDifferenceDesc
    *
    * @param that
    *   $symmetricDifferenceParamThat
    */
  infix def △(that: DataFunction[V, D]): Unit = symmetricDifference(that)

  /**
    * Same as [[difference]].
    *
    * $differenceDesc $mutableAction
    *
    * @param that
    *   $differenceParamThat
    */
  infix def \(that: DataFunction[V, D]): Unit = difference(that)

  // ---------- Export other methods from MutableBase that can be handled directly by underlying... ----------
  // ---------- (these just operate on the underlying data without an API difference) ----------

  // ...with the exception of intersection and ∩: these overloaded functions causes coverage confusion, so we just
  // forward them manually.

  /**
    * $intersectionDesc $mutableAction
    * @param interval
    *   $intersectionParamInterval
    */
  infix def intersection(interval: Interval[D]): Unit = underlying.intersection(interval)

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc $mutableAction
    *
    * @param interval
    *   $intersectionParamInterval
    */
  def ∩(interval: Interval[D]): Unit = intersection(interval)

  // Everything else exports cleanly (TODO: this should fully inherit scaladoc, but doesn't because of a ScalaDoc bug).
  export underlying.{
    map,
    mapIntervals,
    collect,
    collectValues,
    collectIntervals,
    filter,
    set,
    setMany,
    setIfNoConflict,
    update,
    replace,
    replaceByKey,
    remove,
    removeByKey,
    removeMany,
    removeValue,
    compress,
    compressAll,
    recompressAll,
    applyDiffActions,
    fill,
    +,
    ++,
    -,
    --
  }
