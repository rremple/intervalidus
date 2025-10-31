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
trait MutableBase[V, D <: NonEmptyTuple: DomainLike](using Experimental) extends DimensionalBase[V, D]:

  // ---------- Implement methods not in DimensionalBase that have mutable signatures ----------

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
    getAll
      .map(d => d.copy(value = f(d.value)))
      .foreach(updateValidData)

  /**
    * $mapIntervalsDesc $mutableAction
    *
    * @param f
    *   $mapIntervalsParamF
    */
  def mapIntervals(f: Interval[D] => Interval[D]): Unit = synchronized:
    replaceValidData(getAll.map(d => d.copy(interval = f(d.interval))))

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
    remove(data.interval)
    addValidData(data)
    compress(data.value)

  /**
    * $setManyDesc $mutableAction @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    */
  def setMany(data: Iterable[ValidData[V, D]]): Unit = synchronized:
    data.foreach: d =>
      updateOrRemove(d.interval, _ => None)
      addValidData(d)
    values.foreach(compressInPlace)

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
  def update(data: ValidData[V, D]): Unit =
    updateOrRemove(data.interval, _ => Some(data.value))

  /**
    * $replaceDesc $mutableAction
    *
    * @param oldData
    *   $replaceParamOldData
    * @param newData
    *   $replaceParamNewData
    */
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit =
    removeValidData(oldData)
    set(newData)

  /**
    * $replaceByKeyDesc $mutableAction
    *
    * @param key
    *   $replaceByKeyParamKey
    * @param newData
    *   $replaceByKeyParamNewData
    */
  def replaceByKey(key: D, newData: ValidData[V, D]): Unit =
    replace(dataByStartAsc(key), newData)

  /**
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  def remove(interval: Interval[D]): Unit = updateOrRemove(interval, _ => None)

  /**
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  def removeMany(intervals: Iterable[Interval[D]]): Unit =
    intervals.foreach(updateOrRemove(_, _ => None))

  /**
    * $removeValueDesc $mutableAction
    *
    * @param value
    *   $removeValueParamValue
    */
  def removeValue(value: V): Unit =
    intervals(value).foreach(updateOrRemove(_, _ => None))

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
    * @return
    *   a new, updated structure.
    */
  def recompressAll(): Unit =
    recompressInPlace()

  /**
    * $applyDiffActionsDesc $mutableAction
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, D]]): Unit = synchronized:
    diffActions.foreach(applyDiffActionInPlace)

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
  ): Unit = mergeInPlace(that.getAll, mergeValues)

  // equivalent symbolic method names

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
  infix def ++(data: Iterable[ValidData[V, D]]): Unit = setMany(data)

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
  infix def --(intervals: Iterable[Interval[D]]): Unit = removeMany(intervals)
