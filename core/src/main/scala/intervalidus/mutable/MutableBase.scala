package intervalidus.mutable

import intervalidus.*

/**
  * Mutable dimensional data.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
trait MutableBase[V, D <: NonEmptyTuple: DomainLike] extends DimensionalBase[V, D]:

  // ---------- Implement methods not in DimensionalBase that have mutable signatures ----------

  /**
    * $intersectionDesc $mutableAction
    * @param interval
    *   $intersectionParamInterval
    */
  infix def intersection(interval: Interval[D]): Unit = synchronized:
    replaceValidData(intersectionData(interval))

  /**
    * $symmetricDifferenceDesc $mutableAction
    *
    * @param that
    *   $symmetricDifferenceParamThat
    */
  infix def symmetricDifference(that: DimensionalBase[V, D]): Unit = synchronized:
    replaceValidData(symmetricDifferenceData(that))

  /**
    * $mapDesc $mutableAction
    *
    * @param f
    *   $mapParamF
    */
  def map(f: ValidData[V, D] => ValidData[V, D]): Unit = synchronized:
    replaceValidData(getAll.map(f))

  /**
    * $collectDesc $mutableAction
    *
    * @param pf
    *   $collectParamPf
    */
  def collect(pf: PartialFunction[ValidData[V, D], ValidData[V, D]]): Unit = synchronized:
    replaceValidData(getAll.collect(pf))

  /**
    * $mapValuesDesc $mutableAction
    *
    * @param f
    *   $mapValuesParamF
    */
  def mapValues(f: V => V): Unit =
    map(d => d.copy(value = f(d.value)))

  /**
    * $mapIntervalsDesc $mutableAction
    *
    * @param f
    *   $mapIntervalsParamF
    */
  def mapIntervals(f: Interval[D] => Interval[D]): Unit = synchronized:
    map(d => d.copy(interval = f(d.interval)))

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures. $mutableAction
    *
    * @param f
    *   $flatMapParamF
    */
  def flatMap(f: ValidData[V, D] => DimensionalBase[V, D]): Unit = synchronized:
    replaceValidData(getAll.flatMap(f(_).getAll))

  /**
    * Updates structure to only include elements which satisfy a predicate. $mutableAction
    *
    * @param p
    *   $filterParamP
    */
  def filter(p: ValidData[V, D] => Boolean): Unit = synchronized:
    replaceValidData(getAll.filter(p))

  /**
    * $setDesc $mutableAction
    *
    * @param data
    *   $setParamData
    */
  def set(data: ValidData[V, D]): Unit = synchronized:
    setInPlace(data)

  /**
    * $setManyDesc $mutableAction @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    */
  def setMany(data: IterableOnce[ValidData[V, D]]): Unit = synchronized:
    val affected = Set.newBuilder[V]
    data.iterator.foreach: d =>
      val updatedValues = updateOrRemoveNoCompress(d.interval, _ => None)
      addValidData(d)
      affected.addAll(updatedValues)
      affected.addOne(d.value)
    affected.result().foreach(compressInPlace)

  /**
    * $setIfNoConflictDesc $mutableAction
    *
    * @param data
    *   $setIfNoConflictParamData
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(data: ValidData[V, D]): Boolean = synchronized:
    if getIntersecting(data.interval).isEmpty then
      addValidData(data)
      compress(data.value)
      true
    else false

  /**
    * $updateDesc $mutableAction
    *
    * @param data
    *   $updateParamData
    */
  def update(data: ValidData[V, D]): Unit = synchronized:
    updateOrRemove(data.interval, _ => Some(data.value))

  /**
    * $replaceDesc $mutableAction
    *
    * @param oldData
    *   $replaceParamOldData
    * @param newData
    *   $replaceParamNewData
    */
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit = synchronized:
    removeValidData(oldData)
    setInPlace(newData)

  /**
    * $replaceByKeyDesc $mutableAction
    *
    * @param key
    *   $replaceByKeyParamKey
    * @param newData
    *   $replaceByKeyParamNewData
    */
  def replaceByKey(key: D, newData: ValidData[V, D]): Unit =
    replace(dataByStart(key), newData)

  /**
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  def remove(interval: Interval[D]): Unit = synchronized:
    updateOrRemove(interval, _ => None)

  /**
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  def removeMany(intervals: IterableOnce[Interval[D]]): Unit = synchronized:
    val updatedValues = Set.newBuilder[V]
    intervals.iterator.foreach: interval =>
      updatedValues.addAll(updateOrRemoveNoCompress(interval, _ => None))
    updatedValues.result().foreach(compressInPlace)

  /**
    * $differenceDesc $mutableAction
    *
    * @param that
    *   $differenceParamThat
    */
  infix def difference(that: DimensionalBase[V, D]): Unit = removeMany(that.allIntervals)

  /**
    * $removeValueDesc $mutableAction
    *
    * @param value
    *   $removeValueParamValue
    */
  def removeValue(value: V): Unit = synchronized:
    intervals(value).foreach(updateOrRemoveNoCompress(_, _ => None)) // no compression needed

  /**
    * $compressDesc $mutableAction
    *
    * @param value
    *   $compressParamValue
    */
  def compress(value: V): Unit = synchronized:
    compressInPlace(value)

  /**
    * $compressAllDesc $mutableAction
    */
  def compressAll(): Unit = synchronized:
    dataByValue.keySet.foreach(compress)

  /**
    * $recompressAllDesc1
    *
    * $recompressAllDesc2
    *
    * $mutableAction
    *
    * @param otherIntervals
    *   $recompressAllParamOtherIntervals
    */
  def recompressAll(otherIntervals: IterableOnce[Interval[D]] = Iterable.empty): Unit = synchronized:
    recompressInPlace(otherIntervals)

  /**
    * $applyDiffActionsDesc $mutableAction
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[V, D]]): Unit = synchronized:
    diffActions.iterator.foreach(applyDiffActionInPlace)

  /**
    * $syncWithDesc $mutableAction
    *
    * @param that
    *   $syncWithParamThat
    */
  def syncWith(that: DimensionalBase[V, D]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * $fillDesc $mutableAction
    *
    * @param data
    *   $fillParamData
    */
  def fill(data: ValidData[V, D]): Unit = synchronized:
    fillInPlace(data)

  /**
    * $mergeDesc $mutableAction
    *
    * @param that
    *   $mergeParamThat
    * @param mergeValues
    *   $mergeParamMergeValues
    */
  def merge(
    that: DimensionalBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = synchronized:
    mergeInPlace(that.getAll, mergeValues)

  // equivalent symbolic method names

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc $mutableAction
    *
    * @param interval
    *   $intersectionParamInterval
    */
  infix def ∩(interval: Interval[D]): Unit = intersection(interval)

  /**
    * Same as [[symmetricDifference]].
    *
    * $symmetricDifferenceDesc
    *
    * @param that
    *   $symmetricDifferenceParamThat
    */
  infix def △(that: DimensionalBase[V, D]): Unit = symmetricDifference(that)

  /**
    * Same as [[set]]
    *
    * $setDesc $mutableAction
    *
    * @param data
    *   $setParamData
    */
  infix def +(data: ValidData[V, D]): Unit = set(data)

  /**
    * Same as [[setMany]]
    *
    * $setManyDesc $mutableAction @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    */
  infix def ++(data: IterableOnce[ValidData[V, D]]): Unit = setMany(data)

  /**
    * Same as [[remove]]
    *
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  infix def -(interval: Interval[D]): Unit = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  infix def --(intervals: IterableOnce[Interval[D]]): Unit = removeMany(intervals)

  /**
    * Same as [[difference]].
    *
    * $differenceDesc $mutableAction
    *
    * @param that
    *   $differenceParamThat
    */
  infix def \(that: DimensionalBase[V, D]): Unit = difference(that)
