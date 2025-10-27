package intervalidus.immutable

import intervalidus.*

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
trait ImmutableBase[V, D <: NonEmptyTuple: DomainLike, Self <: ImmutableBase[V, D, Self]](using
  Experimental
) extends DimensionalBase[V, D]:

  protected def copyAndModify(f: Self => Unit): Self =
    val result = copy
    f(result)
    result

  // ---------- To be implemented by inheritor ----------

  override def copy: Self // refine the result type for `copyAndModify`

  /**
    * $mapDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * @param f
    *   $mapParamF
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each element of this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  ): DimensionalBase[B, S]

  /**
    * $collectDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * @param pf
    *   $collectParamPf
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
  ): DimensionalBase[B, S]

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
  def mapValues[B](
    f: V => B
  ): DimensionalBase[B, D]

  /**
    * $mapIntervalsDesc The interval type can be changed in the mapping.
    *
    * @param f
    *   $mapIntervalsParamF
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function f to each interval.
    */
  def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): DimensionalBase[V, S]

  /**
    * Builds a new structure by applying a function to all the elements of this collection and concatenating the
    * elements of the resulting structures.
    *
    * @param f
    *   $flatMapParamF
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
  ): DimensionalBase[B, S]

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
    getAll.filterNot(p).foreach(result.removeValidData)

  /**
    * $setDesc
    *
    * @param newData
    *   $setParamNewData
    * @return
    *   $immutableReturn
    */
  def set(newData: ValidData[V, D]): Self = copyAndModify: result =>
    result.updateOrRemove(newData.interval, _ => None)
    result.addValidData(newData)
    result.compressInPlace(newData.value)

  /**
    * $setManyDesc @note $setManyNote
    *
    * @param newData
    *   $setManyParamNewData
    * @return
    *   $immutableReturn
    */
  def setMany(newData: Iterable[ValidData[V, D]]): Self = copyAndModify: result =>
    val values = newData.map(_.value).toSet
    newData.foreach: data =>
      result.updateOrRemove(data.interval, _ => None)
      result.addValidData(data)
    values.foreach: value =>
      result.compressInPlace(value)

  /**
    * $setIfNoConflictDesc
    *
    * @param newData
    *   $setIfNoConflictParamNewData
    * @return
    *   some new, updated structure if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D]): Option[Self] =
    if intersects(newData.interval) then None
    else
      Some(copyAndModify: result =>
        result.addValidData(newData)
        compressInPlace(newData.value)
      )

  /**
    * $updateDesc
    *
    * @param data
    *   $updateParamNewData
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
    result.updateOrRemove(newData.interval, _ => None)
    result.addValidData(newData)
    result.compressInPlace(newData.value)

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
  def replaceByKey(key: D, newData: ValidData[V, D]): Self =
    replace(dataByStartAsc(key), newData)

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
  def removeMany(intervals: Iterable[Interval[D]]): Self = copyAndModify: result =>
    intervals.foreach: interval =>
      result.updateOrRemove(interval, _ => None)

  /**
    * $removeValueDesc
    *
    * @param value
    *   $removeValueParamValue
    * @return
    *   $immutableReturn
    */
  def removeValue(value: V): Self = copyAndModify: result =>
    intervals(value).foreach: interval =>
      result.updateOrRemove(interval, _ => None)

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
    dataByValue.keySet.foreach(result.compressInPlace)

  /**
    * $recompressAllDesc1
    *
    * $recompressAllDesc2
    *
    * @return
    *   $immutableReturn
    */
  def recompressAll(): Self = copyAndModify(_.recompressInPlace())

  /**
    * $applyDiffActionsDesc
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    * @return
    *   $immutableReturn
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, D]]): Self =
    copyAndModify: result =>
      diffActions.foreach(result.applyDiffActionInPlace)

  /**
    * $syncWithDesc
    *
    * @param that
    *   $syncWithParamThat
    * @return
    *   $immutableReturn
    */
  def syncWith(that: Self): Self =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * $fillDesc
    *
    * @param data
    *   $fillParamData
    * @return
    *   $immutableReturn
    */
  def fill(data: ValidData[V, D]): Self = copyAndModify: result =>
    result.fillInPlace(data)

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
    that: Self,
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Self = copyAndModify: result =>
    result.mergeInPlace(that.getAll, mergeValues)

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * $setDesc
    *
    * @param newData
    *   $setParamNewData
    * @return
    *   $immutableReturn
    */
  infix def +(newData: ValidData[V, D]): Self = set(newData)

  /**
    * Same as [[setMany]]
    *
    * $setManyDesc @note $setManyNote
    *
    * @param newData
    *   $setManyParamNewData
    * @return
    *   $immutableReturn
    */
  infix def ++(newData: Iterable[ValidData[V, D]]): Self = setMany(newData)

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
  infix def --(intervals: Iterable[Interval[D]]): Self = removeMany(intervals)
