package intervalidus.mutable

import intervalidus.*
import intervalidus.CoreConfig.IsolationLevel.*
import intervalidus.DimensionalBase.{ReadThatTransaction, UpdateTransaction}

/**
  * Mutable dimensional data.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
trait MutableBase[V, D <: NonEmptyTuple: DomainLike] extends DimensionalBase[V, D]:

  /**
    * Wraps a function body in a new update transaction, committing the resulting changes and returning the result.
    * @note
    *   Transactional updates are synchronized to "serialize" update transactions, even if the isolation level is
    *   ReadUncommitted. Otherwise, two threads could concurrently start transactions, and the commit of one would
    *   overwrite the commit of the other. The isolation level controls how isolated readers are from writers, not how
    *   isolated writers are from each other.
    */
  protected def transactionalUpdate[T](body: UpdateTransaction[V, D] ?=> T): T = synchronized:
    given UpdateTransaction[V, D] = config.isolationLevel match
      case Serializable    => UpdateTransaction.start(state)
      case ReadUncommitted => UpdateTransaction.startDirty(state)
    val result = body
    commit()
    result

  /**
    * Given some other structure, wraps a function body in new transactions for updating this structure while reading
    * from that structure, committing the resulting changes and returning the result. Transactions are started
    * atomically for reflexive integrity.
    */
  protected def transactionalUpdateWith[T, B, S <: NonEmptyTuple](that: DimensionalBase[B, S])(
    body: UpdateTransaction[V, D] ?=> ReadThatTransaction[B, S] => T
  ): T = synchronized:
    val (updateTransaction, readThatTransaction) = atomicStartUpdateTransactionWith(that)
    val result = body(using updateTransaction)(readThatTransaction)
    commit()(using updateTransaction)
    result

  /**
    * Used internally to compress the result of an update if configured to do so.
    */
  def compressedUpdate(): Unit = transactionalUpdate(compressedUpdateInternal())

  private def compressedUpdateInternal()(using UpdateTransaction[V, D]): Unit =
    if config.compressOnUpdate then compressAllInternal()

  // ---------- Implement methods not in DimensionalBase that have mutable signatures ----------

  /**
    * $intersectionDesc $mutableAction
    * @param interval
    *   $intersectionParamInterval
    */
  infix def intersection(interval: Interval[D]): Unit = transactionalUpdate:
    replaceValidData(intersectionData(interval))

  /**
    * $symmetricDifferenceDesc $mutableAction
    *
    * @param that
    *   $symmetricDifferenceParamThat
    */
  infix def symmetricDifference(that: DimensionalBase[V, D]): Unit = transactionalUpdateWith(that): thatTx =>
    replaceValidData(symmetricDifferenceData(that, thatTx))

  /**
    * $mapDesc $mutableAction
    *
    * @param f
    *   $mapParamF
    */
  def map(f: ValidData[V, D] => ValidData[V, D]): Unit = transactionalUpdate:
    mapInternal(f)

  private def mapInternal(f: ValidData[V, D] => ValidData[V, D])(using UpdateTransaction[V, D]): Unit =
    replaceValidData(getAllInternal.map(f))
    compressedUpdateInternal()

  /**
    * $collectDesc $mutableAction
    *
    * @param pf
    *   $collectParamPf
    */
  def collect(pf: PartialFunction[ValidData[V, D], ValidData[V, D]]): Unit = transactionalUpdate:
    replaceValidData(getAllInternal.collect(pf))
    compressedUpdateInternal()

  /**
    * $mapValuesDesc $mutableAction
    *
    * @param f
    *   $mapValuesParamF
    */
  def mapValues(f: V => V): Unit = transactionalUpdate:
    mapInternal(d => d.copy(value = f(d.value)))

  /**
    * $collectValuesDesc $mutableAction
    *
    * @param pf
    *   $collectValuesParamPf
    */
  def collectValues(pf: PartialFunction[V, V]): Unit = transactionalUpdate:
    replaceValidData(collectValuesData(pf))
    compressedUpdateInternal()

  /**
    * $mapIntervalsDesc $mutableAction
    *
    * @param f
    *   $mapIntervalsParamF
    */
  def mapIntervals(f: Interval[D] => Interval[D]): Unit = transactionalUpdate:
    mapInternal(d => d.copy(interval = f(d.interval)))

  /**
    * $collectIntervalsDesc $mutableAction
    *
    * @param pf
    *   $collectIntervalsParamPf
    */
  def collectIntervals(pf: PartialFunction[Interval[D], Interval[D]]): Unit = transactionalUpdate:
    val collected = getAllInternal.collect: d =>
      pf.lift(d.interval) match
        case Some(newInterval) => d.copy(interval = newInterval)
    replaceValidData(collected)
    compressedUpdateInternal()

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures. $mutableAction
    *
    * @param f
    *   $flatMapParamF
    */
  def flatMap(f: ValidData[V, D] => DimensionalBase[V, D]): Unit = transactionalUpdate:
    replaceValidData(getAllInternal.flatMap(f(_).getAll))
    compressedUpdateInternal()

  /**
    * Updates structure to only include elements which satisfy a predicate. $mutableAction
    *
    * @param p
    *   $filterParamP
    */
  def filter(p: ValidData[V, D] => Boolean): Unit = transactionalUpdate:
    replaceValidData(getAllInternal.filter(p))

  /**
    * $setDesc $mutableAction
    *
    * @param data
    *   $setParamData
    */
  def set(data: ValidData[V, D]): Unit = transactionalUpdate:
    setInPlace(data)

  /**
    * $setManyDesc $mutableAction @note $setManyNote
    *
    * @param data
    *   $setManyParamData
    */
  def setMany(data: IterableOnce[ValidData[V, D]]): Unit = transactionalUpdate:
    val affected = Set.newBuilder[V]
    data.iterator.foreach: d =>
      val updatedValues = updateOrRemoveNoCompress(d.interval, _ => None)
      addValidData(d)
      if config.compressOnUpdate then
        affected.addAll(updatedValues)
        affected.addOne(d.value)
    if config.compressOnUpdate then affected.result().foreach(compressInPlace)

  /**
    * $setIfNoConflictDesc $mutableAction
    *
    * @param data
    *   $setIfNoConflictParamData
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(data: ValidData[V, D]): Boolean = transactionalUpdate:
    if getIntersectingInternal(data.interval).isEmpty then
      addValidData(data)
      if config.compressOnUpdate then compress(data.value)
      true
    else false

  /**
    * $updateDesc $mutableAction
    *
    * @param data
    *   $updateParamData
    */
  def update(data: ValidData[V, D]): Unit = transactionalUpdate:
    updateOrRemove(data.interval, _ => Some(data.value))

  /**
    * $replaceDesc $mutableAction
    *
    * @param oldData
    *   $replaceParamOldData
    * @param newData
    *   $replaceParamNewData
    */
  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit = transactionalUpdate:
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
  def replaceByKey(key: D, newData: ValidData[V, D]): Unit = transactionalUpdate:
    removeValidDataByKey(key)
    setInPlace(newData)

  /**
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  def remove(interval: Interval[D]): Unit = transactionalUpdate:
    updateOrRemove(interval, _ => None)

  /**
    * $removeByKeyDesc $mutableAction
    *
    * @param key
    *   $removeByKeyParamKey
    */
  def removeByKey(key: D): Unit = transactionalUpdate:
    removeValidDataByKey(key)

  /**
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  def removeMany(intervals: IterableOnce[Interval[D]]): Unit = transactionalUpdate:
    removeManyInPlace(intervals)

  /**
    * $differenceDesc $mutableAction
    *
    * @param that
    *   $differenceParamThat
    */
  infix def difference(that: DimensionalBase[V, D]): Unit = transactionalUpdateWith(that): thatTx =>
    differenceInPlace(that, thatTx)

  /**
    * $removeValueDesc $mutableAction
    *
    * @param value
    *   $removeValueParamValue
    */
  def removeValue(value: V): Unit = transactionalUpdate:
    intervalsInternal(value).foreach(updateOrRemoveNoCompress(_, _ => None)) // no compression needed

  /**
    * $compressDesc $mutableAction
    *
    * @param value
    *   $compressParamValue
    */
  def compress(value: V): Unit = transactionalUpdate:
    compressInPlace(value)

  /**
    * $compressAllDesc $mutableAction
    */
  def compressAll(): Unit = transactionalUpdate:
    compressAllInternal()

  protected def compressAllInternal()(using UpdateTransaction[V, D]): Unit =
    valuesInternal.foreach(compressInPlace)

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
  def recompressAll(otherIntervals: IterableOnce[Interval[D]] = Iterable.empty): Unit = transactionalUpdate:
    recompressInPlace(otherIntervals)

  /**
    * $applyDiffActionsDesc $mutableAction
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[V, D]]): Unit = transactionalUpdate:
    applyDiffActionsInPlace(diffActions)

  /**
    * $syncWithDesc $mutableAction
    *
    * @param that
    *   $syncWithParamThat
    */
  def syncWith(that: DimensionalBase[V, D]): Unit = transactionalUpdateWith(that): thatTx =>
    syncWithInPlace(that, thatTx)

  /**
    * $fillDesc $mutableAction
    *
    * @param data
    *   $fillParamData
    */
  def fill(data: ValidData[V, D]): Unit = transactionalUpdate:
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
  ): Unit = transactionalUpdateWith(that): thatTx =>
    mergeInPlace(that, thatTx, mergeValues)

  /**
    * $mergeManyDesc $mutableAction
    *
    * @param thatData
    *   $mergeManyParamThatData
    * @param mergeValues
    *   $mergeManyParamMergeValues
    */
  def mergeMany(
    thatData: IterableOnce[ValidData[V, D]],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = transactionalUpdate:
    mergeManyInPlace(thatData, mergeValues)

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
