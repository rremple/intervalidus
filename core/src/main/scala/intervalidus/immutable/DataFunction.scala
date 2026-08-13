package intervalidus.immutable

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
  * Represents an immutable piecewise function defined over a multidimensional domain, mapping intervals to domain
  * functions.
  *
  * Because domain functions (`D => V`) are arbitrary executable closures, equality and lookup operations within
  * `DataFunction` that rely on the identity of these values (e.g., [[compress]], [[mutable.Data.intervals intervals]],
  * and [[removeValue]]) ultimately depend on reference identity of functions (i.e., `eq`), not logical equivilance.
  * Also functions created through functional composition (e.g., `f.andThen(g)`) instantiate distinct object references
  * even when mathematically identical.
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
  * @note
  *   This class inherits [[DimensionalFunctionBase]] methods exported from the underlying [[mutable.Data]] structure.
  *   Because of this [[https://github.com/scala/scala3/issues/14342 Scala issue]], only exported methods without
  *   parameters are rendered correctly in the API docs. Although not in the API doc, these methods are also available:
  *   - [[mutable.Data.isDefinedAt isDefinedAt]]
  *   - [[mutable.Data.getDataAt getDataAt]]
  *   - [[mutable.Data.getIntersecting getIntersecting]]
  *   - [[mutable.Data.intersects intersects]]
  *   - [[mutable.Data.isSubsetOf isSubsetOf]]
  *   - [[mutable.Data.intervals intervals]]
  *   - [[mutable.Data.foldLeft foldLeft]]
  *   - [[mutable.Data.diffActionsFrom diffActionsFrom]]
  *   - [[mutable.Data.⊆ ⊆]]
  *
  * @tparam V
  *   the result type of the domain function managed as data.
  * @tparam D
  *   $intervalDomainType
  */
class DataFunction[V, D <: NonEmptyTuple: DomainLike] private (
  initialData: MutableFunctionData[V, D]
) extends DimensionalFunctionBase[V, D](initialData):

  private def ofUnderlying[B, S <: NonEmptyTuple: DomainLike](
    data: MutableFunctionData[B, S]
  ): DataFunction[B, S] = DataFunction.fromData(data)

  private def withUnderlying[B, S <: NonEmptyTuple: DomainLike](
    f: ImmutableFunctionData[V, D] => ImmutableFunctionData[B, S]
  ): DataFunction[B, S] = DataFunction.fromData(transformUnderlying(f))

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

  override def toImmutable: DataFunction[V, D] = this

  override def toMutable: mutable.DataFunction[V, D] = mutable.DataFunction.asDataFunction(underlying)

  // ---------- Implement methods like those in ImmutableBase/DimensionalBase ----------

  /**
    * $mapValuesDesc Only the valid data domain function result type can be changed in the mapping.
    *
    * @param f
    *   $mapValuesParamF
    * @tparam B
    *   the valid data domain function result type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def mapValues[B](f: V => B): DataFunction[B, D] =
    withUnderlying(_.mapValues(_.andThen(f)))

  /**
    * Builds a new structure by applying a function to all the elements of this structure and concatenating the elements
    * of the resulting structures.
    *
    * @param f
    *   $flatMapParamF
    * @param altConfig
    *   $configParam
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure and
    *   concatenating the results.
    */
  def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidFunction[V, D] => DataFunction[B, S]
  )(using altConfig: CoreConfig[S]): DataFunction[B, S] =
    withUnderlying(_.flatMap(f(_).underlying))

  /**
    * $syncWithDesc
    *
    * @param that
    *   $syncWithParamThat
    * @return
    *   $immutableReturn
    */
  def syncWith(that: DimensionalFunctionBase[V, D]): DataFunction[V, D] =
    withUnderlying(_.syncWith(that.underlying))

  /**
    * $mergeDesc
    *
    * @param that
    *   $mergeParamThat
    * @param mergeValues
    *   $mergeParamMergeValues
    * @return
    *   $immutableReturn
    */
  def merge(that: DimensionalFunctionBase[V, D], mergeValues: (V, V) => V): DataFunction[V, D] =
    withUnderlying(_.merge(that.underlying, _.merge(_, mergeValues)))

  /**
    * $mergeManyDesc
    *
    * @param thatData
    *   $mergeManyParamThatData
    * @param mergeValues
    *   $mergeManyParamMergeValues
    * @return
    *   $immutableReturn
    */
  def mergeMany(
    thatData: IterableOnce[ValidFunction[V, D]],
    mergeValues: (V, V) => V
  ): DataFunction[V, D] = withUnderlying(_.mergeMany(thatData, _.merge(_, mergeValues)))

  /**
    * $symmetricDifferenceDesc
    *
    * @param that
    *   $symmetricDifferenceParamThat
    * @return
    *   a new shape with the elements in this and that, but not in both.
    */
  infix def symmetricDifference(that: DimensionalFunctionBase[V, D]): DataFunction[V, D] =
    withUnderlying(_.symmetricDifference(that.underlying))

  /**
    * $differenceDesc
    *
    * @param that
    *   $differenceParamThat
    * @return
    *   a new shape that is the difference of this and that.
    */
  infix def difference(that: DimensionalFunctionBase[V, D]): DataFunction[V, D] =
    withUnderlying(_.difference(that.underlying))

  // equivalent symbolic method names

  /**
    * Same as [[symmetricDifference]].
    *
    * $symmetricDifferenceDesc
    *
    * @param that
    *   $symmetricDifferenceParamThat
    * @return
    *   a new shape with the elements in this and that, but not in both.
    */
  infix def △(that: DataFunction[V, D]): DataFunction[V, D] = symmetricDifference(that)

  /**
    * Same as [[difference]].
    *
    * $differenceDesc
    *
    * @param that
    *   $differenceParamThat
    * @return
    *   a new shape that is the difference of this and that.
    */
  infix def \(that: DataFunction[V, D]): DataFunction[V, D] = difference(that)

  // ---------- Implement more methods not in DimensionalBase (that mutable just exports) ----------
  // ---------- (these just operate on the underlying data without an API difference) ----------

  /**
    * $intersectionDesc
    *
    * @param interval
    *   $intersectionParamInterval
    * @return
    *   a new shape that is the intersection of this and the interval (i.e., this is "clipped" within the interval).
    */
  infix def intersection(interval: Interval[D]): DataFunction[V, D] =
    withUnderlying(_.intersection(interval))

  /**
    * Applies a function to all valid function data. Both the valid data domain function result and domain types can be
    * changed in the mapping.
    *
    * @param f
    *   the function to apply to each valid function data element.
    * @param altConfig
    *   $configParam
    * @tparam B
    *   the valid data domain function result type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidFunction[V, D] => ValidFunction[B, S]
  )(using altConfig: CoreConfig[S]): DataFunction[B, S] =
    withUnderlying(_.map(f))

  /**
    * Applies a function to all valid function data intervals.
    *
    * @param f
    *   the function to apply to the interval part of each valid function data element.
    * @param altConfig
    *   $configParam
    * @return
    *   a new structure resulting from applying the provided function f to each interval.
    */
  def mapIntervals(f: Interval[D] => Interval[D]): DataFunction[V, D] =
    withUnderlying(_.mapIntervals(f))

  /**
    * Applies a partial function to all valid function data on which it is defined. Both the valid data domain function
    * result and domain types can be changed in the mapping.
    *
    * @param pf
    *   the partial function to apply to each data element.
    * @param altConfig
    *   $configParam
    * @tparam B
    *   the valid data domain function result type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure on which it is
    *   defined.
    */
  def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidFunction[V, D], ValidFunction[B, S]]
  )(using altConfig: CoreConfig[S]): DataFunction[B, S] =
    withUnderlying(_.collect(pf))

  /**
    * Applies a partial function to all valid function data values on which it is defined. Whole functions are
    * considered rather than just function results. Only the valid data domain function result type can be changed in
    * the mapping.
    *
    * @param pf
    *   the partial function to apply to the value part of each valid function data element.
    * @tparam B
    *   the valid data value domain function result of the returned structure.
    * @return
    *   a new structure resulting from applying the provided partial function pf to each element of this structure where
    *   it is defined.
    */
  def collectValues[B](pf: PartialFunction[DomainFunction[V, D], DomainFunction[B, D]]): DataFunction[B, D] =
    withUnderlying(_.collectValues(pf))

  /**
    * Applies a partial function to all valid function data intervals on which it is defined.
    *
    * @param pf
    *   the partial function to apply to the interval part of each valid function data element.
    * @param altConfig
    *   $configParam
    * @return
    *   a new structure resulting from applying the provided partial function pf to each interval where it is defined.
    */
  def collectIntervals(pf: PartialFunction[Interval[D], Interval[D]]): DataFunction[V, D] =
    withUnderlying(_.collectIntervals(pf))

  /**
    * Selects all elements that satisfy a predicate.
    *
    * @param p
    *   the predicate used to test elements.
    * @return
    *   a new structure consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidFunction[V, D] => Boolean): DataFunction[V, D] =
    withUnderlying(_.filter(p))

  /**
    * $setDesc
    *
    * @param data
    *   $setParamData
    * @return
    *   $immutableReturn
    */
  def set(data: ValidFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.set(data))

  /**
    * $setManyDesc @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    * @return
    *   $immutableReturn
    */
  def setMany(data: IterableOnce[ValidFunction[V, D]]): DataFunction[V, D] =
    withUnderlying(_.setMany(data))

  /**
    * Set new valid function data, but only if there are no data previously valid in this interval.
    *
    * @param data
    *   the valid function data to set.
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(data: ValidFunction[V, D]): Option[DataFunction[V, D]] =
    underlying.toImmutable.setIfNoConflict(data).map(immutable => DataFunction.fromData(immutable.toMutable))

  /**
    * Update everything valid in the data's interval to have the data's value. No new intervals of validity are added as
    * part of this operation. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    * @return
    *   $immutableReturn
    */
  def update(data: ValidFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.update(data))

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param oldData
    *   the old data to be replaced.
    * @param newData
    *   the new data replacing the old data
    * @return
    *   $immutableReturn
    */
  def replace(oldData: ValidFunction[V, D], newData: ValidFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.replace(oldData, newData))

  /**
    * Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
    * overlaps with the new data interval are adjusted accordingly.
    *
    * @param key
    *   key of the old data to be replaced (the interval start).
    * @param newData
    *   the new data replacing the old data
    * @return
    *   $immutableReturn
    */
  def replaceByKey(key: D, newData: ValidFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.replaceByKey(key, newData))

  /**
    * $removeDesc
    *
    * @param interval
    *   $removeParamInterval
    * @return
    *   $immutableReturn
    */
  def remove(interval: Interval[D]): DataFunction[V, D] =
    withUnderlying(_.remove(interval))

  /**
    * Remove the valid function with an interval starting at the key.
    *
    * @param key
    *   key of the data to be removed (the interval start).
    * @return
    *   $immutableReturn
    */
  def removeByKey(key: D): DataFunction[V, D] =
    withUnderlying(_.removeByKey(key))

  /**
    * $removeManyDesc
    *
    * @param intervals
    *   $removeManyParamIntervals
    * @return
    *   $immutableReturn
    */
  def removeMany(intervals: IterableOnce[Interval[D]]): DataFunction[V, D] =
    withUnderlying(_.removeMany(intervals))

  /**
    * Remove the value in all the intervals where it is valid.
    *
    * @param value
    *   the value that is removed.
    * @return
    *   $immutableReturn
    */
  def removeValue(value: DomainFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.removeValue(value))

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid function data are compressed.
    * @return
    *   $immutableReturn
    */
  def compress(value: DomainFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.compress(value))

  /**
    * Compress out adjacent intervals with the same value for all values.
    *
    * @return
    *   $immutableReturn
    */
  def compressAll(): DataFunction[V, D] =
    withUnderlying(_.compressAll())

  /**
    * Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    *
    * @param otherIntervals
    *   other intervals to be considered when decompressing the space. This is useful in testing equivalence of two
    *   structures where their starting intervals differ enough that they result in a different enough decompression
    *   that it results in different recompressions.
    * @return
    *   $immutableReturn
    */
  def recompressAll(otherIntervals: IterableOnce[Interval[D]] = Iterable.empty): DataFunction[V, D] =
    withUnderlying(_.recompressAll(otherIntervals))

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * @param diffActions
    *   actions to be applied.
    * @return
    *   $immutableReturn
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[DomainFunction[V, D], D]]): DataFunction[V, D] =
    withUnderlying(_.applyDiffActions(diffActions))

  /**
    * Adds a value as valid in portions of the interval where there aren't already valid functions.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    * @return
    *   $immutableReturn
    */
  def fill(data: ValidFunction[V, D]): DataFunction[V, D] =
    withUnderlying(_.fill(data))

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * $setDesc
    *
    * @param data
    *   $setParamData
    * @return
    *   $immutableReturn
    */
  infix def +(data: ValidFunction[V, D]): DataFunction[V, D] = set(data)

  /**
    * Same as [[setMany]]
    *
    * $setManyDesc @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    * @return
    *   $immutableReturn
    */
  infix def ++(data: IterableOnce[ValidFunction[V, D]]): DataFunction[V, D] = setMany(data)

  /**
    * Same as [[remove]]
    *
    * $removeDesc
    *
    * @param interval
    *   $removeParamInterval
    * @return
    *   $immutableReturn
    */
  infix def -(interval: Interval[D]): DataFunction[V, D] = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * $removeManyDesc
    *
    * @param intervals
    *   $removeManyParamIntervals
    * @return
    *   $immutableReturn
    */
  infix def --(intervals: IterableOnce[Interval[D]]): DataFunction[V, D] = removeMany(intervals)

  /**
    * Same as [[intersection]]
    *
    * $intersectionDesc
    *
    * @param interval
    *   $intersectionParamInterval
    * @return
    *   a new shape that is the intersection of this and the interval (i.e., this is "clipped" within the interval).
    */
  infix def ∩(interval: Interval[D]): DataFunction[V, D] = intersection(interval)
