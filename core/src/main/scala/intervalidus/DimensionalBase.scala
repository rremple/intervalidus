package intervalidus

import intervalidus.DomainLike.given
import intervalidus.collection.{Boundary, Box, BoxedPayload, Capacity}
import intervalidus.collection.immutable.MultiMapSorted
import intervalidus.collection.mutable.BoxTree

import java.util.NoSuchElementException
import scala.collection.immutable.TreeMap
import scala.collection.mutable

/**
  * Constructs data in multidimensional intervals.
  *
  * @tparam Constructed
  *   Constructed type.
  *
  * @define objectDesc
  *   Constructs data in multidimensional intervals.
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
trait DimensionalBaseObject[Constructed[_, _ <: NonEmptyTuple] <: DimensionalBase[?, ?]]:

  // ---------- Abstract ----------

  /**
    * Constructor for multiple initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of values valid within intervals -- intervals must be disjoint.
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using config: CoreConfig[D]): Constructed[V, D]

  // ---------- Concrete ----------

  /**
    * Constructor where no values are valid.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   a new structure with no valid values.
    */
  def empty[V, D <: NonEmptyTuple: DomainLike](using config: CoreConfig[D]): Constructed[V, D] = apply(Iterable.empty)

  /**
    * Shorthand constructor for a single initial value that is valid in a specific interval.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param data
    *   value valid within an interval.
    * @return
    *   a new structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using config: CoreConfig[D]): Constructed[V, D] = apply(Iterable.single(data))

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @param config
    *   $configParam
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param value
    *   value that is valid in the full domain (`Interval.unbounded[D]`).
    * @return
    *   a new structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using config: CoreConfig[D]): Constructed[V, D] = of(Interval.unbounded[D] -> value)

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
  def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    config: CoreConfig[D]
  ): mutable.Builder[ValidData[V, D], Constructed[V, D]] = ValidData.Builds[V, D, Constructed[V, D]](apply(_))

/**
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  */
object DimensionalBase:

  /**
    * A transaction is used to at least access (read) and at most alter (update) the state in an isolated way that
    * performs well.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  trait Transaction[V, D <: NonEmptyTuple]:
    /**
      * Access the current state of this transaction.
      */
    def state: State[V, D]

    /**
      * Access the map of data by start.
      */
    def dataByStart: TreeMap[D, ValidData[V, D]] = state.dataByStart

    /**
      * Access the multimap of data by value.
      */
    def dataByValue: MultiMapSorted[V, ValidData[V, D]] = state.dataByValue

    /**
      * Access data organized in a box tree.
      */
    def dataInBoxTree: BoxTree[ValidData[V, D]] = state.dataInBoxTree

  /**
    * A read transaction on this structure where the state never changes from the initial state.
    *
    * @param state
    *   the state with which we both start and end
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  class ReadTransaction[V, D <: NonEmptyTuple](override val state: State[V, D]) extends Transaction[V, D]

  object ReadTransaction:
    /**
      * Starts a read-only transaction. The state is not copied since it will not be updated.
      * @param priorState
      *   the state at the beginning of the transaction.
      * @tparam V
      *   $dataValueType
      * @tparam D
      *   $intervalDomainType
      * @return
      *   a new transaction.
      */
    def start[V, D <: NonEmptyTuple](priorState: State[V, D]): ReadTransaction[V, D] =
      ReadTransaction(priorState)

  /**
    * A read transaction on a separate structure where the state never changes from the initial state. Not expected to
    * be committed. (Same as [[ReadTransaction]], but with a different name to avoid confusion.)
    *
    * @param state
    *   the state with which we both start and end
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  class ReadThatTransaction[V, D <: NonEmptyTuple](override val state: State[V, D]) extends Transaction[V, D]

  object ReadThatTransaction:
    /**
      * Starts a read-only transaction on another structure. The state is not copied since it will not be updated.
      * @param priorState
      *   the state of the other structure at the beginning of the transaction.
      * @tparam V
      *   $dataValueType
      * @tparam D
      *   $intervalDomainType
      * @return
      *   a new transaction.
      */
    def start[V, D <: NonEmptyTuple](priorState: State[V, D]): ReadThatTransaction[V, D] =
      ReadThatTransaction(priorState)

  /**
    * An update transaction where the state is managed internally and evolves with each update. This transaction must be
    * committed to actually update the state externally. Because [[UpdateTransaction]] extends [[Transaction]], it can
    * be used in contexts requiring a read transactions, making reads and writes consistent.
    *
    * @param initialState
    *   the state we start with
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  class UpdateTransaction[V, D <: NonEmptyTuple](initialState: State[V, D]) extends Transaction[V, D]:
    private var workingDataByStart: TreeMap[D, ValidData[V, D]] = initialState.dataByStart
    private var workingDataByValue: MultiMapSorted[V, ValidData[V, D]] = initialState.dataByValue
    private val workingDataInBoxTree: BoxTree[ValidData[V, D]] = initialState.dataInBoxTree

    override def state: State[V, D] = State(workingDataByStart, workingDataByValue, workingDataInBoxTree)
    override def dataByStart: TreeMap[D, ValidData[V, D]] = workingDataByStart
    override def dataByValue: MultiMapSorted[V, ValidData[V, D]] = workingDataByValue
    override def dataInBoxTree: BoxTree[ValidData[V, D]] = workingDataInBoxTree

    /**
      * Update the internal state of this transaction.
      *
      * @param byStartMod
      *   function yielding a new instance of the dataByStart immutable map.
      * @param byValueMod
      *   function yielding a new instance of the dataByValue immutable multimap.
      * @param boxTreeMod
      *   function updating the dataInBoxTree mutable box tree.
      */
    def update(
      byStartMod: TreeMap[D, ValidData[V, D]] => TreeMap[D, ValidData[V, D]],
      byValueMod: MultiMapSorted[V, ValidData[V, D]] => MultiMapSorted[V, ValidData[V, D]],
      boxTreeMod: BoxTree[ValidData[V, D]] => Unit
    ): Unit =
      workingDataByStart = byStartMod(workingDataByStart)
      workingDataByValue = byValueMod(workingDataByValue)
      boxTreeMod(workingDataInBoxTree)

  object UpdateTransaction:
    /**
      * Starts a "clean" update transaction. The prior state is copied, isolating updates while the transaction is in
      * progress.
      * @note
      *   It is important for performance that this copy is done once rather than repeatedly, which is why internal
      *   methods prefer calling other internal methods which use an already-established transaction context.
      *
      * @param priorState
      *   the state at the beginning of the transaction.
      * @tparam V
      *   $dataValueType
      * @tparam D
      *   $intervalDomainType
      * @return
      *   a new transaction.
      */
    def start[V, D <: NonEmptyTuple](priorState: State[V, D]): UpdateTransaction[V, D] =
      UpdateTransaction(priorState.copy)

    /**
      * Starts a "dirty" update transaction. The prior state is not copied, so updates are not isolated. This is
      * appropriate when applying updates to an immutable structure where the result structure has already been copied.
      *
      * @param priorState
      *   the state at the beginning of the transaction.
      * @tparam V
      *   $dataValueType
      * @tparam D
      *   $intervalDomainType
      * @return
      *   a new transaction.
      */
    def startDirty[V, D <: NonEmptyTuple](priorState: State[V, D]): UpdateTransaction[V, D] =
      UpdateTransaction(priorState)

  /**
    * Collection of data structures kept in sync to represent the state of this structure.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  class State[V, D <: NonEmptyTuple](
    /**
      * Internal data structure where all the interval-bounded data are stored, always expected to be disjoint. TreeMap
      * maintains interval key order.
      */
    val dataByStart: TreeMap[D, ValidData[V, D]],

    /**
      * An internal shadow data structure where all the interval-bounded data are also stored, but using the value
      * itself as the key (for faster compression, which is done by value). The ValidData[V, D] are stored in a sorted
      * set, so they are retrieved in key order, making compression operations repeatable.
      */
    val dataByValue: MultiMapSorted[V, ValidData[V, D]],

    /**
      * An internal shadow data structure where all the interval-bounded data are also stored, but in a "box search
      * tree" -- a hyperoctree (i.e., a B-tree, quadtree, octree, etc., depending on the dimension) that supports quick
      * retrieval by interval.
      */
    val dataInBoxTree: BoxTree[ValidData[V, D]]
  ):
    /**
      * Copies state. Note that only the mutable box tree is copied, because the other two data structures are
      * immutable.
      *
      * @return
      *   a new state with copied data
      */
    def copy = State(dataByStart, dataByValue, dataInBoxTree.copy)

  object State:
    /**
      * Given a collection of valid data, returns initial state data for `dataByStart`, `dataByValue`, and
      * `dataInBoxTree`. Applies the spacial capacity hint from the config if there is one.
      * @note
      *   If there is no spatial capacity hint, a tight boundary around the origin with a capacity large enough to
      *   contain the initial data is used. Benchmarks have shown that a tightly-defined capacity has better insert
      *   performance even if it has to be resized at some point. But having a reasonable capacity hint can improve on
      *   this.
      *
      * @param initialData
      *   a collection of values valid within intervals -- intervals must be disjoint.
      * @param config
      *   $configParam
      * @tparam V
      *   $dataValueType
      * @tparam D
      *   $intervalDomainType
      * @return
      *   tuple of `TreeMap` data, `MultiMapSorted` data, and `BoxTree` data used when constructing something that is a
      *   `DimensionalBase` and has overridden `dataByStart`, `dataByValue`, and `dataInBoxTree` in the constructor.
      */
    def from[V, D <: NonEmptyTuple](
      initialData: Iterable[ValidData[V, D]]
    )(using
      domainValue: DomainLike[D],
      config: CoreConfig[D]
    ): State[V, D] =
      val initialPayloads = initialData.map(_.asBoxedPayload)
      val initialCapacity = config.capacityHint match
        case Some(hint) =>
          hint.asBox.fixUnbounded(Capacity.aroundOrigin(domainValue.arity))
        case None =>
          initialPayloads.foldLeft(Capacity.aroundOrigin(domainValue.arity)): (capacity, payload) =>
            capacity.growAround(payload.box)
      State(
        TreeMap.from(initialData.map(_.withStartKey)),
        MultiMapSorted.from(initialData.map(_.withValueKey)),
        BoxTree.from(Boundary(initialCapacity), initialPayloads)
      )

import intervalidus.DimensionalBase.*

/**
  * Base for all dimensional data, both mutable and immutable, of multiple dimensions.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam D
  *   the domain type -- a non-empty tuple that is DomainLike.
  *
  * @define configParam
  *   context parameter for configuration -- uses defaults if not given explicitly
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  * @define immutableReturn
  *   a new, updated structure.
  * @define mutableAction
  *   Data are mutated in place.
  * @define intersectionDesc
  *   The intersection of this and a single interval. See [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
  * @define intersectionParamInterval
  *   a single interval with which to intersect.
  * @define symmetricDifferenceDesc
  *   The "exclusive or" of this and that. That is, the portions of the that which are not in the domain of this and the
  *   portions of this which are not in the domain of that. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
  * @define symmetricDifferenceParamThat
  *   shape to combine.
  * @define mapDesc
  *   Applies a function to all valid data.
  * @define mapParamF
  *   the function to apply to each valid data element.
  * @define collectDesc
  *   Applies a partial function to all valid data on which it is defined.
  * @define collectParamPf
  *   the partial function to apply to each data element.
  * @define mapValuesDesc
  *   Applies a function to all valid data values.
  * @define mapValuesParamF
  *   the function to apply to the value part of each valid data element.
  * @define mapIntervalsDesc
  *   Applies a function to all valid data intervals.
  * @define mapIntervalsParamF
  *   the function to apply to the interval part of each valid data element.
  * @define flatMapParamF
  *   the function to apply to each valid data element which results in a new structure.
  * @define filterParamP
  *   the predicate used to test elements.
  * @define setDesc
  *   Set new valid data. Replaces any data previously valid in this interval.
  * @define setParamData
  *   the valid data to set.
  * @define setManyDesc
  *   Set a collection of new valid data. Replaces any data previously valid in this interval.
  * @define setManyNote
  *   if intervals overlap, later items will update earlier ones, so order can matter.
  * @define setManyParamData
  *   collection of valid data to set.
  * @define setIfNoConflictDesc
  *   Set new valid data, but only if there are no data previously valid in this interval.
  * @define setIfNoConflictParamData
  *   the valid data to set.
  * @define updateDesc
  *   Update everything valid in the data's interval to have the data's value. No new intervals of validity are added as
  *   part of this operation. Data with overlaps are adjusted accordingly.
  * @define updateParamData
  *   the new value and interval existing data should take on.
  * @define removeDesc
  *   Remove valid values on the interval. If there are values valid on portions of the interval, those values have
  *   their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeParamInterval
  *   the interval where any valid values are removed.
  * @define removeManyDesc
  *   Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
  *   intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeManyParamIntervals
  *   the intervals where any valid values are removed.
  * @define differenceDesc
  *   The elements in this which are not in the domain of that. The values of that are ignored. See
  *   [[https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement]].
  * @define differenceParamThat
  *   shape to remove.
  * @define removeValueDesc
  *   Remove the value in all the intervals where it is valid.
  * @define removeValueParamValue
  *   the value that is removed.
  * @define compressDesc
  *   Compress out adjacent intervals with the same value.
  * @define compressParamValue
  *   value for which valid data are compressed.
  * @define compressAllDesc
  *   Compress out adjacent intervals with the same value for all values.
  * @define recompressAllDesc1
  *   Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
  *   [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
  * @define recompressAllDesc2
  *   First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
  *   would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
  *   recompresses the data, which results in a unique physical representation. It may be useful when comparing two
  *   structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
  * @define recompressAllParamOtherIntervals
  *   other intervals to be considered when decompressing the space. This is useful in testing equivalence of two
  *   structures where their starting intervals differ enough that they result in a different enough decompression that
  *   it results in different recompressions.
  * @define applyDiffActionsDesc
  *   Applies a sequence of diff actions to this structure.
  * @define applyDiffActionsParamDiffActions
  *   actions to be applied.
  * @define syncWithDesc
  *   Synchronizes this with another structure by getting and applying the applicable diff actions.
  * @define syncWithParamThat
  *   the structure with which this is synchronized.
  * @define fillDesc
  *   Adds a value as valid in portions of the interval where there aren't already valid values.
  * @define fillParamData
  *   value to make valid in any validity gaps found in the interval
  * @define mergeDesc
  *   Merges this structure with data from that structure. In intervals where both structures have valid values, the two
  *   values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
  *   are added (a fill operation).
  * @define mergeParamThat
  *   structure to merge with this one
  * @define mergeParamMergeValues
  *   function that merges values where both this and that have valid values, where the default merge operation is to
  *   give this data values priority and drop that data values
  * @define replaceDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceParamOldData
  *   the old data to be replaced.
  * @define replaceParamNewData
  *   the new data replacing the old data
  * @define replaceByKeyDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceByKeyParamKey
  *   key of the old data to be replaced (the interval start).
  * @define replaceByKeyParamNewData
  *   the new data replacing the old data
  */
trait DimensionalBase[V, D <: NonEmptyTuple](using
  domainLike: DomainLike[D]
) extends PartialFunction[D, V]:

  /**
    * $configParam
    */
  given config: CoreConfig[D]

  /**
    * Wraps a function body in a new read-only transaction, returning the result.
    */
  protected def transactionalRead[T](body: Transaction[V, D] ?=> T): T =
    body(using ReadTransaction.start(state))

  /**
    * Given some other structure, wraps a function body in a new read-only transaction on that structure, returning the
    * result.
    */
  protected def transactionalReadOnly[T, B](that: DimensionalBase[B, D])(body: ReadThatTransaction[B, D] => T): T =
    body(ReadThatTransaction.start(that.state))

  /**
    * Given some other structure, wraps a function body in new read-only transactions on both this structure and that
    * structure, returning the result.
    */
  protected def transactionalReadWith[T, B](body: DimensionalBase[B, D])(
    result: ReadTransaction[V, D] ?=> ReadThatTransaction[B, D] => T
  ): T = result(using ReadTransaction.start(state))(ReadThatTransaction.start(body.state))

  override def equals(obj: Any): Boolean = obj match
    case that: DimensionalBase[?, ?] =>
      transactionalRead:
        val thatTx = ReadThatTransaction.start(that.state)
        sizeInternal == that.sizeInternal(using thatTx) &&
        getAllInternal.iterator.zip(that.getAllInternal(using thatTx).iterator).forall(_ == _)
    case _ => false

  override def hashCode(): Int = size.hashCode() * 31 + state.dataByStart.headOption.hashCode()

  def decompressedData(otherIntervals: IterableOnce[Interval[D]]): Iterable[ValidData[V, D]] = transactionalRead:
    decompressedDataInternal(otherIntervals)

  protected def decompressedDataInternal(
    otherIntervals: IterableOnce[Interval[D]]
  )(using Transaction[V, D]): Iterable[ValidData[V, D]] =
    for
      atomicInterval <- Interval.uniqueIntervals(allIntervalsInternal ++ otherIntervals)
      intersecting <- getIntersectingInternal(atomicInterval) // always returns either one or zero results
    yield intersecting.copy(interval = atomicInterval)

  private infix def equivalentWithMutualDecompression(
    that: DimensionalBase[V, D],
    thatTx: ReadThatTransaction[V, D]
  )(using thisTx: Transaction[V, D]): Boolean =
    val thisData = decompressedDataInternal(that.allIntervalsInternal(using thatTx))(using thisTx)
    val thatData = that.decompressedDataInternal(allIntervalsInternal(using thisTx))(using thatTx)
    thisData.size == thatData.size && thisData.iterator.zip(thatData.iterator).forall(_ == _)

  /**
    * Indicates whether some other dimensional structure is "logically equivalent to" this one. That is, either this and
    * that are equal, or they are equal after being decompressed using the same base intervals (the same decompression
    * used in recompressAll).
    * @param that
    *   dimensional structure to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def isEquivalentTo(that: DimensionalBase[V, D]): Boolean = transactionalReadWith(that): thatTx =>
    equals(that) || equivalentWithMutualDecompression(that, thatTx)

  /**
    * Same as [[isEquivalentTo]]
    *
    * Indicates whether some other object is "logically equivalent to" this one. That is, either this and that are
    * equal, or they are equal after being decompressed using the same base intervals (the same decompression used in
    * recompressAll).
    *
    * @param that
    *   dimensional structure to compare
    * @return
    *   true if this and that are logically equivalent
    */
  infix def ≡(that: DimensionalBase[V, D]): Boolean = isEquivalentTo(that)

  // Utility methods for managing state, not part of API

  /**
    * Internal mutator to add valid data, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData[V, D])(using tx: UpdateTransaction[V, D]): Unit =
    // assert(!tx.dataByStart.isDefinedAt(data.interval.start))
    tx.update(
      _.updated(data.interval.start, data),
      _.addOne(data.withValueKey),
      _.addOne(data.asBoxedPayload)
    )

  /**
    * Internal mutator to update valid data, where a value with v.interval.start already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData[V, D])(using tx: UpdateTransaction[V, D]): Unit =
    // assert(tx.dataByStart.isDefinedAt(data.interval.start))
    val oldData = tx.dataByStart(data.interval.start)
    tx.update(
      _.updated(data.interval.start, data),
      _.subtractOne(oldData.withValueKey).addOne(data.withValueKey),
      treeCopy =>
        treeCopy.remove(oldData.asBoxedPayload)
        treeCopy.addOne(data.asBoxedPayload)
    )

  /**
    * Internal mutator to remove valid data, where a known value already exists.
    *
    * @param oldData
    *   valid data to remove.
    */
  protected def removeValidData(oldData: ValidData[V, D])(using tx: UpdateTransaction[V, D]): Unit =
    tx.update(
      _.removed(oldData.interval.start),
      _.subtractOne(oldData.withValueKey),
      _.remove(oldData.asBoxedPayload)
    )

  /**
    * Internal mutator to remove valid data, where a value with a known key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D)(using tx: UpdateTransaction[V, D]): Unit =
    removeValidData(tx.dataByStart(key))

  /**
    * Internal mutator to replace all valid data.
    * @param data
    *   new valid data replacing the old valid data
    */
  protected def replaceValidData(data: Iterable[ValidData[V, D]])(using tx: UpdateTransaction[V, D]): Unit =
    tx.update(
      _ => TreeMap.from(data.map(_.withStartKey)),
      _ => MultiMapSorted.from(data.map(_.withValueKey)),
      treeCopy =>
        treeCopy.clear()
        treeCopy.addAll(data.map(_.asBoxedPayload))
    )

  /**
    * Updates the state based on the result of the update transaction.
    *
    * @param tx
    *   transaction with accumulated updates.
    */
  protected def commit()(using tx: UpdateTransaction[V, D]): Unit =
    state = tx.state

  /**
    * Internal method, to update or remove in place.
    *
    * Remove or update valid values on the target interval. If there are values valid on portions of the interval, those
    * values have their intervals adjusted (e.g., shortened, shifted, split) accordingly. The logic of remove and update
    * are similar, and this method supports both.
    *
    * @note
    *   this gets even more complicated in higher dimensions. Exclusions in one dimension can have three remainders:
    *   none (simple), single (partial), and split. But multidimensional exclusions have these same three remainders in
    *   each dimension, so there are a total of 3<sup>n</sup> remainder cases. But there is symmetry because order
    *   doesn't matter, so actually there are only `(3+n-1)! / (3! x (n-1)!)` (we want the unique combinations of
    *   choosing from 3 remainder scenarios n times, where order doesn't matter).
    *
    * For example, with `n = 4`, this reduces to `6!/(3! x 3!) = 20` unique cases. But just giving these cases intuitive
    * names would be hard enough. (Nothing in four dimensions is ever intuitive!)
    *
    * Falling back to more familiar space, using `n = 3` this reduces to `5!/(2! x 3!) = 10` unique cases, which can be
    * enumerated as follows:
    *   1. simple = none + none + none (1 case)
    *   1. corner = single + single + single (1 case)
    *   1. core = split + split + split (1 case)
    *   1. face = single + none + none (3 symmetric cases)
    *   1. edge = single + single + none (3 symmetric cases)
    *   1. slice = split + none + none (3 symmetric cases)
    *   1. hole = split + split + none (3 symmetric cases)
    *   1. notch = split + single + single (3 symmetric cases)
    *   1. divot = split + split + single (3 symmetric cases)
    *   1. bite = split + single + none (6 symmetric cases)
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  protected def updateOrRemove(
    targetInterval: Interval[D],
    updateValue: V => Option[V]
  )(using UpdateTransaction[V, D]): Unit =
    updateOrRemoveNoCompress(targetInterval, updateValue).iterator.distinct.foreach(compressInPlace)

  /**
    * Same as [[updateOrRemove]], but returning the affected values rather than compressing. For operations that make
    * multiple updateOrRemove calls, it is more efficient compressing at the end, especially when dealing with
    * structures that have many intervals for the same value. (The maximum impact of this fact, and the main motivation
    * for making compression deferred, is IntervalShape, where all values in the underlying Data are the same, i.e.,
    * unit.)
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    * @return
    *   all potentially affected values (before and after update), may include duplicates
    */
  protected def updateOrRemoveNoCompress(
    targetInterval: Interval[D],
    updateValue: V => Option[V]
  )(using UpdateTransaction[V, D]): Iterable[V] =
    getIntersectingInternal(targetInterval).flatMap: overlap =>
      val updatedValueOption = updateValue(overlap.value)
      (overlap.interval ∩ targetInterval).foreach: intersection => // there will always be one
        // Creates an atomic deconstruction of the intervals covering the overlap without the intersection
        val atomicNonIntersections = overlap.interval.separateUsing(intersection).filter(_ != intersection)
        /*
         * Compression is important to minimize add/update calls (which are expensive). Initially, to avoid the
         * TreeMap build in `Interval.compress`, a linear folding algorithm was used here that only looked back two
         * elements. Although it worked, compression had more of an "above" than "right" bias leading to results
         * that, although properly compressed, differed from the results of `recompressAll`. By benchmarking
         * `remove` using these alternative approaches (folding vs. `Interval.compress`) it was shown that the
         * throughput was kind of a wash in two dimensions, but the throughput in three dimensions was more than 40%
         * better using `Interval.compress`. This is probably because, to effectively compress in n dimensions, the
         * folding method would need to look back at least n elements, and 3 > 2. The ineffective compression led
         * to more unnecessary update/add calls, and therefore lower throughput. This effect is likely even more
         * pronounced in higher dimensions. That's why this just uses standard `Interval.compress` now, and it is a
         * nice side effect that all the compressions are consistent without taking the performance hit of
         * `recompressAll`.
         */
        val nonIntersections = Interval.compress(atomicNonIntersections)

        // remove the intersecting region if it happens to have the same key as the overlap
        if intersection hasSameStartAs overlap.interval then removeValidData(overlap)

        // add/update non-intersecting regions
        nonIntersections.foreach: subinterval =>
          if subinterval hasSameStartAs overlap.interval
          then updateValidData(subinterval -> overlap.value)
          else addValidData(subinterval -> overlap.value)

        // if there is an updated value, add it back in
        updatedValueOption.foreach: newValue =>
          addValidData(intersection -> newValue)

      // intersecting and updated value results for compression later
      Iterable.single(overlap.value) ++ updatedValueOption

  /**
    * Internal method, to fill in place.
    *
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param data
    *   specifies the interval in which the value should be filled
    */
  protected def fillInPlaceNoCompress(data: ValidData[V, D])(using UpdateTransaction[V, D]): Unit =
    Interval
      .uniqueIntervals(getIntersectingInternal(data.interval).map(_.interval) ++ Seq(data.interval))
      .foreach: i =>
        if i ⊆ data.interval && !intersectsInternal(i) then addValidData(i -> data.value)

  protected def fillInPlace(data: ValidData[V, D])(using UpdateTransaction[V, D]): Unit =
    fillInPlaceNoCompress(data)
    compressInPlace(data.value)

  /**
    * Internal method, to merge in place.
    *
    * Merges this structure with data from that structure. In intervals where both structures have valid values, the two
    * values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
    * are added (a fill operation).
    *
    * @param that
    *   data to merge into this one
    * @param mergeValues
    *   function that merges values where both this and that have valid values
    */
  protected def mergeInPlace(
    that: Iterable[ValidData[V, D]],
    mergeValues: (V, V) => V
  )(using UpdateTransaction[V, D]): Unit =
    val affected = that.flatMap: thatData =>
      val updatedValues =
        updateOrRemoveNoCompress(thatData.interval, thisDataValue => Some(mergeValues(thisDataValue, thatData.value)))
      fillInPlaceNoCompress(thatData)
      updatedValues ++ Iterable.single(thatData.value)
    affected.iterator.distinct.foreach(compressInPlace)

  /**
    * Internal method, to compress in place.
    *
    * Assumes caller does synchronization (if needed). Assumes underlying data are disjoint, so no need to address
    * intersections.
    *
    * @param value
    *   value to be evaluated
    * @return
    *   this structure once compressed (not a copy)
    */
  protected def compressInPlace(value: V)(using tx: UpdateTransaction[V, D]): Unit = Interval.compressGeneric(
    initialState = (), // no state -- updates applied in place
    result = identity, // no result -- updates applied in place
    dataIterable = _ => tx.dataByValue.get(value),
    interval = _.interval,
    valueMatch = _.value == _.value,
    lookup = (_, start) => tx.dataByStart.get(start),
    compressAdjacent = (r, s, _) =>
      removeValidData(s)
      val newData = r.interval ∪ s.interval -> value
      (newData, updateValidData(newData))
  )

  /**
    * Internal method, to recompress in place.
    *
    * Unlike in 1D, there are no unique compressions in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data to a unique arrangement of "atomic" intervals. In the above example, that would be
    * the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it recompresses the
    * data, which results in a unique physical representation. This may be useful when comparing two structures to see
    * if they are logically equivalent even if, physically, they differ in how they are compressed.
    *
    * @param otherIntervals
    *   other intervals to be considered when decompressing the space. This is useful in testing equivalence of two
    *   structures where their starting intervals differ enough that they result in a different enough decompression
    *   that it results in different recompressions. E.g., one might compare immutable.Data 'a' and 'b' using
    *   'a.recompressAll(b.allIntervals) shouldBe b.recompressAll(a.allIntervals)'.
    */
  protected def recompressInPlace(otherIntervals: IterableOnce[Interval[D]])(using tx: UpdateTransaction[V, D]): Unit =
    replaceValidData(decompressedDataInternal(otherIntervals))

    // recompress
    valuesInternal.foreach(compressInPlace)

  /**
    * Internal method to set in place.
    */
  protected def setInPlace(data: ValidData[V, D])(using UpdateTransaction[V, D]): Unit =
    val updatedValues = updateOrRemoveNoCompress(data.interval, _ => None)
    addValidData(data)
    (updatedValues.iterator ++ Iterator.single(data.value)).distinct.foreach(compressInPlace)

  /**
    * Applies a diff action to this structure.
    *
    * @param diffAction
    *   action to be applied.
    */
  protected def applyDiffActionInPlace(diffAction: DiffAction[V, D])(using UpdateTransaction[V, D]): Unit =
    diffAction match
      case DiffAction.Create(data: ValidData[V, D]) => addValidData(data)
      case DiffAction.Update(data: ValidData[V, D]) => updateValidData(data)
      case DiffAction.Delete(key)                   => removeValidDataByKey(key)
    // Not sure why, but returning explicit Unit here resolves runtime type check warning above
    ()

  /**
    * Data for the intersection of this and a single interval. See
    * [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param interval
    *   a single interval to intersect.
    * @return
    *   data representing the intersection of this and the interval.
    */
  protected def intersectionData(interval: Interval[D])(using Transaction[V, D]): Iterable[ValidData[V, D]] =
    for
      d <- getIntersectingInternal(interval)
      i <- d.interval ∩ interval
    yield d.copy(interval = i)

  /**
    * Intervals in the intersection of this and another structure. See
    * [[https://en.wikipedia.org/wiki/Intersection_(set_theory)]].
    *
    * @param that
    *   another structure.
    * @return
    *   intervals representing the intersection of this and that. (The values in this and that are ignored.)
    */
  def intersectingIntervals(
    that: DimensionalBase[?, D]
  ): Iterable[Interval[D]] = transactionalReadWith(that): thatTx =>
    for
      a <- getAllInternal
      b <- that.getIntersectingInternal(a.interval)(using thatTx)
      intervalIntersection <- a.interval ∩ b.interval
    yield intervalIntersection

  /**
    * Internal method, to zip with the data of another dimensional structure with the same domain type. The result
    * domain will be the intersection of this domain and that domain.
    *
    * @param that
    *   dimensional structure to zip with -- must have the same domain type
    * @param f
    *   combines the values of this and that into the value result type
    * @tparam B
    *   the value type of that
    * @tparam R
    *   the value result type
    * @return
    *   a collection of valid data representing this structure's data zipped with that structure's data.
    */
  protected def zipData[B, R](
    that: DimensionalBase[B, D],
    thatTx: ReadThatTransaction[B, D],
    f: (V, B) => R
  )(using Transaction[V, D]): Iterable[ValidData[R, D]] =
    for
      a <- getAllInternal
      b <- that.getIntersectingInternal(a.interval)(using thatTx)
      intersection <- a.interval ∩ b.interval
    yield intersection -> f(a.value, b.value)

  /**
    * Internal method, to zip with the data of another dimensional structure with the same domain type and apply
    * defaults where data are valid in one structure and not the other. The result domain will be the union of this
    * domain and that domain.
    *
    * @param that
    *   dimensional structure to zip with -- must have the same domain type
    * @param thisDefault
    *   default value used on the left-hand side when no data are valid in this structure when data are valid in that
    *   structure.
    * @param thatDefault
    *   default value used on the right-hand side when no data are valid in that structure when data are valid in this
    *   structure.
    * @tparam B
    *   the value type of that
    * @return
    *   a collection of valid data representing this structure's data zipped with that structure's data and defaults.
    */
  protected def zipAllData[B, R](
    that: DimensionalBase[B, D],
    thatTx: ReadThatTransaction[B, D],
    thisDefault: V,
    thatDefault: B,
    f: (V, B) => R
  )(using Transaction[V, D]): Iterable[ValidData[R, D]] =
    zipAllDataGenericInternal(
      that,
      thatTx,
      whenBothMissing = None,
      whenOnlyThis = v => Some(f(v, thatDefault)),
      whenOnlyThat = b => Some(f(thisDefault, b)),
      whenBothPresent = (v, b) => Some(f(v, b))
    )

  /**
    * Data for the "exclusive or" of this and that. That is, the portions of the shapes that are in this or that, but
    * not in both. See [[https://en.wikipedia.org/wiki/Symmetric_difference]].
    *
    * @param that
    *   shape to combine.
    * @return
    *   data representing the symmetric difference of this and that.
    */
  protected def symmetricDifferenceData(
    that: DimensionalBase[V, D],
    thatTx: ReadThatTransaction[V, D]
  )(using Transaction[V, D]): Iterable[ValidData[V, D]] =
    zipAllDataGenericInternal(
      that,
      thatTx,
      whenBothMissing = None,
      whenOnlyThis = Some(_),
      whenOnlyThat = Some(_),
      whenBothPresent = (_, _) => None
    )

  /**
    * Zip with the data of another dimensional structure with the same domain type and select data based on the function
    * parameters. The full domain is considered when whenBothMissing is some value, otherwise only the domain covered by
    * this and that is considered. (This is primarily an internal method supporting zipAll and symmetricDifference, but
    * made public so it can be tested directly.)
    *
    * @param that
    *   dimensional structure to zip with -- must have the same domain type
    * @param whenBothMissing
    *   Optionally, what to return when neither this nor that is defined in the interval.
    * @param whenOnlyThis
    *   Optionally, what to return when this is defined in the interval, but that is not (a function on the value in
    *   this).
    * @param whenOnlyThat
    *   Optionally, what to return when that is defined in the interval, but this is not (a function on the value in
    *   that).
    * @param whenBothPresent
    *   Optionally, what to return when both this and that are defined (a function on the values in this and that).
    * @tparam B
    *   the value type of that
    * @tparam R
    *   the value type the returned iterable
    * @return
    *   a collection of valid data representing this structure's data zipped generically with that structure's data, and
    *   all elements of the domain outside both.
    */
  def zipAllDataGeneric[B, R](
    that: DimensionalBase[B, D],
    whenBothMissing: Option[R],
    whenOnlyThis: V => Option[R],
    whenOnlyThat: B => Option[R],
    whenBothPresent: (V, B) => Option[R]
  ): Iterable[ValidData[R, D]] = transactionalReadWith(that): thatTx =>
    zipAllDataGenericInternal(that, thatTx, whenBothMissing, whenOnlyThis, whenOnlyThat, whenBothPresent)

  private def zipAllDataGenericInternal[B, R](
    that: DimensionalBase[B, D],
    thatTx: ReadThatTransaction[B, D],
    whenBothMissing: Option[R],
    whenOnlyThis: V => Option[R],
    whenOnlyThat: B => Option[R],
    whenBothPresent: (V, B) => Option[R]
  )(using Transaction[V, D]): Iterable[ValidData[R, D]] =
    val intervalsConsidered =
      allIntervalsInternal ++ that.allIntervalsInternal(using thatTx) ++ whenBothMissing.map(_ => Interval.unbounded[D])
    for
      subInterval <- Interval.uniqueIntervals(intervalsConsidered)
      thisValueOption = getIntersectingInternal(subInterval).headOption.map(_.value)
      thatValueOption = that.getIntersectingInternal(subInterval)(using thatTx).headOption.map(_.value)
      valuePair <- (thisValueOption, thatValueOption) match
        case (None, None)                       => whenBothMissing
        case (Some(thisValue), None)            => whenOnlyThis(thisValue)
        case (None, Some(thatValue))            => whenOnlyThat(thatValue)
        case (Some(thisValue), Some(thatValue)) => whenBothPresent(thisValue, thatValue)
    yield subInterval -> valuePair

  /**
    * Internal method to get all data as data in n-1 dimensions based on a lookup in the head dimension.
    *
    * (Equivalent to `getByDimensionData[H, Domain.NonEmptyTail[D]](0, domain)`, though the type checking is simpler.)
    *
    * @tparam H
    *   the domain value type of the 1D domain used for filtering. There are type safety checks that ensure
    *   - the head 1D domain has the specified domain value type
    *   - the current domain tail is a non-empty domain (i.e., the current domain type `D` has at least two dimensions)
    *   - the current domain type can be constructed by concatenating the 1D domain type specified and the current
    *     domain tail.
    * @param domain
    *   the head dimension domain element used for filtering
    * @return
    *   a collection of valid data representing the lower-dimensional (n-1) projection
    */
  protected def getByHeadDimensionData[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]],
    Transaction[V, D]
  ): Iterable[ValidData[V, Domain.NonEmptyTail[D]]] =
    val filteredData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        getAllInternal.filter(_.interval.headInterval1D contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withHeadUpdate[H](_ => Interval1D.intervalAt(domain))
        getIntersectingInternal(lookup)
    filteredData.map: data =>
      data.interval.tailInterval -> data.value

  /**
    * Internal method to get all data as data in n-1 dimensions based on a lookup in the specified dimension.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    * @param domain
    *   the domain element used for filtering
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    * @tparam R
    *   domain of intervals in the returned valid data. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a collection of valid data representing the lower-dimensional (n-1) projection
    */
  protected def getByDimensionData[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    Transaction[V, D]
  ): Iterable[ValidData[V, R]] =
    val filteredData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        getAllInternal.filter(_.interval(dimensionIndex) contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withDimensionUpdate[H](dimensionIndex, _ => Interval1D.intervalAt(domain))
        getIntersectingInternal(lookup)
    filteredData.map: data =>
      data.interval.dropDimension(dimensionIndex) -> data.value

  // ---------- API methods implemented here ----------

  // from Object
  // print a uniform grid representing the data.
  override def toString: String =
    if getAll.isEmpty then "<nothing is valid>"
    else
      // tuples of first dimension start string, first dimension end string, first dimension string (header)
      val horizontalIntervalStrings = domainLike.intervalPreprocessForGrid(allIntervals)
      // tuples of first dimension start string, first dimension end string, value + remaining dimension string
      val validDataStrings = getAll.map(_.preprocessForGrid)
      val maxDataSize = validDataStrings
        .map: (_, _, valueString) =>
          valueString.length + 3
        .maxOption
        .getOrElse(3)
      val maxHorizontalIntervalsSize = horizontalIntervalStrings
        .map: (_, _, intervalString) =>
          intervalString.length
        .maxOption
        .getOrElse(7)

      val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

      def pad(chars: Int, p: String = " "): String = p * chars

      val horizontalBuilders = (StringBuilder(), Map.newBuilder[String, Int], Map.newBuilder[String, Int])
      val (horizontalStringBuilder, horizontalStartPositionBuilder, horizontalEndPositionBuilder) =
        horizontalIntervalStrings.zipWithIndex.foldLeft(horizontalBuilders):
          case (
                (stringBuilder, startPositionBuilder, endPositionBuilder),
                ((startString, endString, formatted), pos)
              ) =>
            startPositionBuilder.addOne(startString, stringBuilder.size)
            stringBuilder.append(formatted)
            val padTo = cellSize * (pos + 1)
            if stringBuilder.size < padTo then stringBuilder.append(pad(padTo - stringBuilder.size))
            endPositionBuilder.addOne(endString, stringBuilder.size)
            (stringBuilder, startPositionBuilder, endPositionBuilder)

      val horizontalStartPosition = horizontalStartPositionBuilder.result()
      val horizontalEndPosition = horizontalEndPositionBuilder.result()
      horizontalStringBuilder.append("|\n")

      validDataStrings.foreach: (startString, endString, valueString) =>
        val leftPosition = horizontalStartPosition(startString)
        val rightPosition = horizontalEndPosition(endString)
        val valuePadding = rightPosition - leftPosition - valueString.length - 2
        horizontalStringBuilder.append(
          s"${pad(leftPosition)}| $valueString${pad(valuePadding)}|\n"
        )

      horizontalStringBuilder.result()

  // from PartialFunction
  override def isDefinedAt(key: D): Boolean = getAt(key).isDefined

  // from PartialFunction
  override def apply(domainIndex: D): V = getAt(domainIndex).getOrElse(
    throw Exception(s"Not defined at $domainIndex")
  )

  /**
    * Tests if there are no valid data in this structure.
    *
    * @return
    *   true if there are no valid data, false otherwise.
    */
  def isEmpty: Boolean = transactionalRead:
    summon[Transaction[V, D]].dataByStart.isEmpty

  /**
    * The number of valid data entries.
    */
  def size: Int = transactionalRead(sizeInternal)

  protected def sizeInternal(using tx: Transaction[V, D]): Int = tx.dataByStart.size

  /**
    * Returns the value if a single, unbounded valid value exists, otherwise throws an exception.
    *
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals).
    */
  def get: V = transactionalRead:
    getAllInternal.headOption match
      case Some(d: ValidData[V, D]) if d.interval.isUnbounded => d.value
      case Some(_)                                            => throw NoSuchElementException("bounded get")
      case None                                               => throw NoSuchElementException("empty get")

  /**
    * Returns Some value if a single, unbounded valid value exists, otherwise returns None.
    */
  def getOption: Option[V] = transactionalRead:
    getAllInternal.headOption.filter(_.interval.isUnbounded).map(_.value)

  /**
    * Returns all valid data in interval start order
    */
  def getAll: Iterable[ValidData[V, D]] = transactionalRead:
    getAllInternal

  protected def getAllInternal(using tx: Transaction[V, D]): Iterable[ValidData[V, D]] =
    tx.dataByStart.values

  /**
    * Returns valid data at the specified domain element. That is, where the specified domain element is a member of
    * some valid data interval. If no such valid data exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value and corresponding interval if valid at the specified domain element, otherwise None.
    */
  def getDataAt(domainIndex: D): Option[ValidData[V, D]] = transactionalRead:
    getDataAtInternal(domainIndex)

  protected def getDataAtInternal(domainIndex: D)(using tx: Transaction[V, D]): Option[ValidData[V, D]] =
    tx.dataInBoxTree
      .get(Box.at(domainIndex.asCoordinateUnfixed))
      .collectFirst:
        case d if domainIndex ∈ d.payload.interval => d.payload

  /**
    * Returns a value that is valid at the specified domain element. That is, where the specified domain element is a
    * member of some valid data interval. If no such valid value exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D): Option[V] = transactionalRead:
    getDataAtInternal(domainIndex).map(_.value)

  /**
    * Returns all data that are valid on some or all of the provided interval.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: Interval[D]): Iterable[ValidData[V, D]] = transactionalRead:
    getIntersectingInternal(interval)

  protected def getIntersectingInternal(
    interval: Interval[D]
  )(using tx: Transaction[V, D]): Iterable[ValidData[V, D]] =
    tx.dataInBoxTree
      .getAndDeduplicate(interval.asBox)
      .collect:
        case BoxedPayload(_, payload, _) if payload.interval intersects interval => payload

  /**
    * Are there values that are valid on some or all of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: Interval[D]): Boolean = transactionalRead:
    intersectsInternal(interval)

  protected infix def intersectsInternal(interval: Interval[D])(using tx: Transaction[V, D]): Boolean =
    tx.dataInBoxTree.get(interval.asBox).exists(_.payload.interval intersects interval)

  /**
    * Is this a subset (proper or improper) of that? See [[https://en.wikipedia.org/wiki/Subset]]. This is the
    * Topological Subset without considering values.
    *
    * @param that
    *   shape to test
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: DimensionalBase[V, D]): Boolean = transactionalReadWith(that): thatTx =>
    Interval
      .uniqueIntervals(allIntervalsInternal ++ that.allIntervalsInternal(using thatTx))
      .forall: subInterval =>
        !intersectsInternal(subInterval) || that.intersectsInternal(subInterval)(using thatTx)

  /**
    * Same as [[isSubsetOf]]
    *
    * Is this a subset (proper or improper) of that? See [[https://en.wikipedia.org/wiki/Subset]]. This is the
    * Topological Subset without considering values.
    *
    * @param that
    *   shape to test
    * @return
    *   true if this is a subset of that.
    */
  infix def ⊆(that: DimensionalBase[V, D]): Boolean = isSubsetOf(that)

  /**
    * Returns all the intervals (compressed) in which there are valid values. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]].
    */
  def domain: Iterable[Interval[D]] = transactionalRead:
    Interval.compress(
      Interval.uniqueIntervals(allIntervalsInternal).filter(intersectsInternal(_))
    )

  /**
    * Returns all the intervals (compressed) in which there are no valid values. That is, all intervals that are not in
    * the [[domain]]. See [[https://en.wikipedia.org/wiki/Domain_of_a_function]] and
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    */
  def domainComplement: Iterable[Interval[D]] = transactionalRead:
    domainComplementInternal

  protected def domainComplementInternal(using Transaction[V, D]): Iterable[Interval[D]] =
    Interval.compress(
      Interval
        .uniqueIntervals(
          allIntervalsInternal ++
            Iterable.single(Interval.unbounded[D])
        )
        .filter(!intersectsInternal(_))
    )

  /**
    * Returns the distinct values that are valid in some interval.
    */
  def values: Iterable[V] = transactionalRead:
    valuesInternal

  protected def valuesInternal(using tx: Transaction[V, D]): Iterable[V] =
    tx.dataByValue.keySet

  /**
    * Returns the intervals in which this value is valid.
    *
    * @param value
    *   the value to look up
    */
  def intervals(value: V): Iterable[Interval[D]] = transactionalRead:
    intervalsInternal(value)

  protected def intervalsInternal(value: V)(using tx: Transaction[V, D]): Iterable[Interval[D]] =
    tx.dataByValue.get(value).map(_.interval)

  /**
    * Returns the intervals in which any values are valid.
    */
  def allIntervals: Iterable[Interval[D]] = transactionalRead:
    allIntervalsInternal

  protected def allIntervalsInternal(using Transaction[V, D]): Iterable[Interval[D]] =
    getAllInternal.map(_.interval)

  /**
    * Applies a binary operator to a start value and all valid data, going left to right.
    *
    * @param z
    *   the start value.
    * @param op
    *   the binary operator.
    * @tparam B
    *   the result type of the binary operator.
    * @return
    *   the result of inserting op between consecutive valid data elements, going left to right with the start value z
    *   on the left. Returns z if there are no valid data elements.
    */
  def foldLeft[B](z: B)(op: (B, ValidData[V, D]) => B): B = transactionalRead:
    getAllInternal.foldLeft(z)(op)

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DimensionalBase[V, D]): Iterable[DiffAction[V, D]] = transactionalReadWith(old): oldTx =>
    diffActionsFromInternal(old, oldTx)

  protected def diffActionsFromInternal(
    old: DimensionalBase[V, D],
    oldTx: ReadThatTransaction[V, D]
  )(using newTx: Transaction[V, D]): Iterable[DiffAction[V, D]] =
    (newTx.dataByStart.keySet ++ oldTx.dataByStart.keys).toSeq.flatMap: key =>
      (oldTx.dataByStart.get(key), newTx.dataByStart.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction.Delete(oldData.interval.start))
        case _                                                    => None

  // ---------- To be implemented by inheritor ----------

  protected def state: State[V, D]
  protected def state_=(v: State[V, D]): Unit

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: DimensionalBase[V, D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intersections) in a pair. The other structure can have a different value type but must have the same interval
    * type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zip[B](that: DimensionalBase[B, D]): DimensionalBase[(V, B), D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intervals in both this and that) in a pair. If one of the two collections has a valid value in an interval
    * where the other one doesn't, default elements are used in the result. The other structure can have a different
    * value type but must have the same interval type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisDefault
    *   default element used in intervals where data are valid in that but not this.
    * @param thatDefault
    *   default element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): DimensionalBase[(V, B), D]

  /**
    * Project as data in n-1 dimensions based on a lookup in the head dimension.
    *
    * (Equivalent to `getByDimension[H, Domain.NonEmptyTail[D]](0, domain)`, though the type checking is simpler)
    *
    * @tparam H
    *   the domain value type of the 1D domain used for filtering. There are type safety checks that ensure
    *   - the head 1D domain has the specified domain value type
    *   - the current domain tail is a non-empty domain (i.e., the current domain type `D` has at least two dimensions)
    *   - the current domain type can be constructed by concatenating the 1D domain type specified and the current
    *     domain tail.
    * @param domain
    *   the head dimension domain element
    * @param altConfig
    *   $configParam
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using altConfig: CoreConfig[Domain.NonEmptyTail[D]]): DimensionalBase[V, Domain.NonEmptyTail[D]]

  /**
    * Project as data in n-1 dimensions based on a lookup in the specified dimension.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    * @param domain
    *   the domain element used for filtering
    * @param altConfig
    *   $configParam
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    * @tparam R
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DimensionalBase[V, R]

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: intervalidus.mutable.MutableBase[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: intervalidus.immutable.ImmutableBase[V, D, ?]
