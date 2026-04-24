package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalBase.{ReadThatTransaction, Transaction, UpdateTransaction}

/**
  * Immutable dimensional data.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  * @tparam Self
  *   F-bounded self-type.
  */
trait ImmutableBase[V, D <: NonEmptyTuple: DomainLike, Self <: ImmutableBase[V, D, Self]] extends DimensionalBase[V, D]:

  protected def copyAndModify(f: Self => UpdateTransaction[V, D] ?=> Unit): Self =
    val result = copy
    // dirty because result.state is already copied
    given UpdateTransaction[V, D] = UpdateTransaction.startDirty(result.state)
    f(result)
    result.commit()
    result

  // ---------- To be implemented by inheritor ----------

  override def copy: Self // refine the result type for `copyAndModify`

  /**
    * $mapDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * @param f
    *   $mapParamF
    * @param altConfig
    *   $configParam
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): DimensionalBase[B, S]

  /**
    * $collectDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * @param pf
    *   $collectParamPf
    * @param altConfig
    *   $configParam
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure on which it is
    *   defined.
    */
  def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): DimensionalBase[B, S]

  /**
    * $mapValuesDesc Only the valid data value type can be changed in the mapping.
    *
    * @param f
    *   $mapValuesParamF
    * @tparam B
    *   the valid data value type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each element of this structure.
    */
  def mapValues[B](f: V => B): DimensionalBase[B, D]

  /**
    * $mapIntervalsDesc The interval type can be changed in the mapping.
    *
    * @param f
    *   $mapIntervalsParamF
    * @param altConfig
    *   $configParam
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each interval.
    */
  def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): DimensionalBase[V, S]

  /**
    * Builds a new structure by applying a function to all the elements of this collection and concatenating the
    * elements of the resulting structures.
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
    f: ValidData[V, D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): DimensionalBase[B, S]

  // ---------- Implement methods not in DimensionalBase that have immutable signatures ----------

  /**
    * Selects all elements that satisfy a predicate.
    *
    * @param p
    *   $filterParamP
    * @return
    *   a new structure consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData[V, D] => Boolean): Self = copyAndModify: result =>
    result.getAllInternal.filterNot(p).foreach(result.removeValidData)

  /**
    * $intersectionDesc
    *
    * @param interval
    *   $intersectionParamInterval
    * @return
    *   a new shape that is the intersection of this and the interval (i.e., this is "clipped" within the interval).
    */
  infix def intersection(interval: Interval[D]): Self = copyAndModify: result =>
    result.replaceValidData(result.intersectionData(interval))

  /**
    * $symmetricDifferenceDesc
    *
    * @param that
    *   $symmetricDifferenceParamThat
    * @return
    *   a new shape with the elements in this and that, but not in both.
    */
  infix def symmetricDifference(that: DimensionalBase[V, D]): Self = transactionalReadOnly(that): thatTx =>
    copyAndModify: result =>
      result.replaceValidData(result.symmetricDifferenceData(that, thatTx))

  /**
    * $setDesc
    *
    * @param data
    *   $setParamData
    * @return
    *   $immutableReturn
    */
  def set(data: ValidData[V, D]): Self =
    copyAndModify(_.setInPlace(data))

  /**
    * $setManyDesc @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    * @return
    *   $immutableReturn
    */
  def setMany(data: IterableOnce[ValidData[V, D]]): Self = copyAndModify: result =>
    val affected = Set.newBuilder[V]
    data.iterator.foreach: d =>
      val updatedValues = result.updateOrRemoveNoCompress(d.interval, _ => None)
      result.addValidData(d)
      affected.addAll(updatedValues)
      affected.addOne(d.value)
    affected.result().foreach(result.compressInPlace)

  /**
    * $setIfNoConflictDesc
    *
    * @param data
    *   $setIfNoConflictParamData
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(data: ValidData[V, D]): Option[Self] =
    if intersects(data.interval) then None // this test is in a separate transactionalRead
    else
      val updated = copyAndModify: result =>
        result.addValidData(data)
        result.compressInPlace(data.value)
      Some(updated)

  /**
    * $updateDesc
    *
    * @param data
    *   $updateParamData
    * @return
    *   $immutableReturn
    */
  def update(data: ValidData[V, D]): Self =
    copyAndModify(_.updateOrRemove(data.interval, _ => Some(data.value)))

  /**
    * $replaceDesc
    *
    * @param oldData
    *   $replaceParamOldData
    * @param newData
    *   $replaceParamNewData
    * @return
    *   $immutableReturn
    */
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Self = copyAndModify: result =>
    result.removeValidData(oldData)
    result.setInPlace(newData)

  /**
    * $replaceByKeyDesc
    *
    * @param key
    *   $replaceByKeyParamKey
    * @param newData
    *   $replaceByKeyParamNewData
    * @return
    *   $immutableReturn
    */
  def replaceByKey(key: D, newData: ValidData[V, D]): Self = copyAndModify: result =>
    result.removeValidDataByKey(key)
    result.setInPlace(newData)

  /**
    * $removeDesc
    *
    * @param interval
    *   $removeParamInterval
    * @return
    *   $immutableReturn
    */
  def remove(interval: Interval[D]): Self =
    copyAndModify(_.updateOrRemove(interval, _ => None))

  /**
    * $removeManyDesc
    *
    * @param intervals
    *   $removeManyParamIntervals
    * @return
    *   $immutableReturn
    */
  def removeMany(intervals: IterableOnce[Interval[D]]): Self = copyAndModify: result =>
    val updatedValues = Set.newBuilder[V]
    intervals.iterator.foreach: interval =>
      updatedValues.addAll(result.updateOrRemoveNoCompress(interval, _ => None))
    updatedValues.result().foreach(compressInPlace)

  /**
    * $differenceDesc
    *
    * @param that
    *   $differenceParamThat
    * @return
    *   a new shape that is the difference of this and that.
    */
  infix def difference(that: DimensionalBase[V, D]): Self =
    removeMany(that.allIntervals) // uses external API of that

  /**
    * $removeValueDesc
    *
    * @param value
    *   $removeValueParamValue
    * @return
    *   $immutableReturn
    */
  def removeValue(value: V): Self = copyAndModify: result =>
    intervalsInternal(value).foreach(result.updateOrRemoveNoCompress(_, _ => None)) // no compression needed

  /**
    * $compressDesc
    *
    * @param value
    *   $compressParamValue
    * @return
    *   $immutableReturn
    */
  def compress(value: V): Self =
    copyAndModify(_.compressInPlace(value))

  /**
    * $compressAllDesc
    *
    * @return
    *   $immutableReturn
    */
  def compressAll(): Self = copyAndModify: result =>
    valuesInternal.foreach(result.compressInPlace)

  /**
    * $recompressAllDesc1
    *
    * $recompressAllDesc2
    *
    * @param otherIntervals
    *   $recompressAllParamOtherIntervals
    * @return
    *   $immutableReturn
    */
  def recompressAll(otherIntervals: IterableOnce[Interval[D]] = Iterable.empty): Self =
    copyAndModify(_.recompressInPlace(otherIntervals))

  /**
    * $applyDiffActionsDesc
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    * @return
    *   $immutableReturn
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[V, D]]): Self = copyAndModify: result =>
    diffActions.iterator.foreach(result.applyDiffActionInPlace)

  /**
    * $syncWithDesc
    *
    * @param that
    *   $syncWithParamThat
    * @return
    *   $immutableReturn
    */
  def syncWith(that: DimensionalBase[V, D]): Self =
    applyDiffActions(that.diffActionsFrom(this)) // uses external API of that

  /**
    * $fillDesc
    *
    * @param data
    *   $fillParamData
    * @return
    *   $immutableReturn
    */
  def fill(data: ValidData[V, D]): Self = copyAndModify(_.fillInPlace(data))

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
  def merge(
    that: DimensionalBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Self = copyAndModify(_.mergeInPlace(that.getAll, mergeValues)) // uses external API of that

  // equivalent symbolic method names

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
  def ∩(interval: Interval[D]): Self = intersection(interval)

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
  infix def △(that: DimensionalBase[V, D]): Self = symmetricDifference(that)

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
  infix def +(data: ValidData[V, D]): Self = set(data)

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
  infix def ++(data: IterableOnce[ValidData[V, D]]): Self = setMany(data)

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
  infix def -(interval: Interval[D]): Self = remove(interval)

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
  infix def --(intervals: IterableOnce[Interval[D]]): Self = removeMany(intervals)

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
  infix def \(that: DimensionalBase[V, D]): Self = difference(that)
