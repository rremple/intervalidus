package intervalidus

import intervalidus.collection.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Base for all dimensional data, both mutable and immutable, both 1D and 2D.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be [[DomainLike]] and have an [[Ordering]].
  * @tparam I
  *   the interval type, based on the domain type. Must be [[IntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam DiffAction
  *   the diff action type. Must be [[DiffActionLike]] based on [[V]], [[D]], and [[I]].
  * @tparam Self
  *   F-bounded self type.
  */
trait DimensionalBase[
  V,
  D: DomainLike: Ordering,
  I <: IntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  DiffAction: DiffActionLike,
  Self <: DimensionalBase[V, D, I, ValidData, DiffAction, Self]
](using
  experimental: Experimental
) extends PartialFunction[D, V]:

  protected def newValidData(value: V, interval: I): ValidData

  // For defining 1D and 2D toString methods - print a uniform grid representing the data.
  protected def toStringGrid[R1, R2: DomainValueLike](
    dataToString: ValidData => String,
    dataToInterval: ValidData => Interval1D[R1],
    dataToSortBy: ValidData => Domain1D[R2]
  )(using horizontalDomainValue: DomainValueLike[R1]): String =

    val validData = getAll.toList
    val maxDataSize = validData.map(dataToString).map(_.length + 3).maxOption.getOrElse(3)
    val horizontalIntervals = Interval1D.uniqueIntervals(validData.map(dataToInterval))
    val punctuation = horizontalDomainValue match
      case _: ContinuousValue[R1] => horizontalDomainValue.bracePunctuation
      case _: DiscreteValue[R1]   => s" ${horizontalDomainValue.bracePunctuation} "

    def formatInterval(interval: Interval1D[R1]): String = horizontalDomainValue match
      case _: ContinuousValue[R1] =>
        s"${interval.start.leftBrace} ${interval.start}$punctuation${interval.end} ${interval.end.rightBrace} "
      case _: DiscreteValue[R1] =>
        s"| ${interval.start}$punctuation${interval.end} "
    val maxHorizontalIntervalsSize = horizontalIntervals
      .map(formatInterval(_).length)
      .maxOption
      .getOrElse(7)
    val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

    def pad(chars: Int, p: String = " "): String = p * chars

    val (horizontalStringBuilder, horizontalStartPositionBuilder, horizontalEndPositionBuilder) =
      horizontalIntervals.zipWithIndex.foldLeft(
        (StringBuilder(), Map.newBuilder[Domain1D[R1], Int], Map.newBuilder[Domain1D[R1], Int])
      ):
        case ((stringBuilder, startPositionBuilder, endPositionBuilder), (interval, index)) =>
          startPositionBuilder.addOne(interval.start, stringBuilder.size)
          stringBuilder.append(formatInterval(interval))
          val padTo = cellSize * (index + 1)
          if stringBuilder.size < padTo then stringBuilder.append(pad(padTo - stringBuilder.size))
          endPositionBuilder.addOne(interval.end, stringBuilder.size)
          (stringBuilder, startPositionBuilder, endPositionBuilder)

    val horizontalStartPosition = horizontalStartPositionBuilder.result()
    val horizontalEndPosition = horizontalEndPositionBuilder.result()
    horizontalStringBuilder.append("|\n")

    validData
      .sortBy(dataToSortBy)
      .foreach: v =>
        val valueString = dataToString(v)
        val leftPosition = horizontalStartPosition(dataToInterval(v).start)
        val rightPosition = horizontalEndPosition(dataToInterval(v).end)
        val valuePadding = rightPosition - leftPosition - valueString.length - 2
        horizontalStringBuilder.append(
          s"${pad(leftPosition)}| $valueString${pad(valuePadding)}|\n"
        )

    horizontalStringBuilder.result()

  // ---------- To be implemented by inheritor ----------

  /**
    * Internal data structure where all the interval-bounded data are stored, always expected to be disjoint. TreeMap
    * maintains interval key order.
    */
  protected def dataByStartAsc: scala.collection.mutable.TreeMap[D, ValidData]

  /**
    * Internal shadow data structure where all the interval-bounded data are also stored, but using the interval key in
    * reverse order (for much quicker range lookups -- validated in 1D microbenchmarks).
    *
    * @deprecated
    *   Only populated when legacy behavior is enabled through experimental feature 'noSearchTree'
    */
  protected def dataByStartDesc: scala.collection.mutable.TreeMap[D, ValidData]

  /**
    * Another internal shadow data structure where all the interval-bounded data are also stored, but using the value
    * itself as the key (for faster compression, which is done by value). The ValidData are stored in a sorted set, so
    * they are retrieved in key order, making compression operations repeatable.
    */
  protected def dataByValue: MultiMapSorted[V, ValidData]

  protected def dataInSearchTree: BoxTree[ValidData]

  protected def dataInSearchTreeAdd(data: ValidData): Unit =
    dataInSearchTree.addOne(data.asBoxedPayload)

  protected def dataInSearchTreeRemove(data: ValidData): Unit =
    dataInSearchTree.remove(data.asBoxedPayload)

  protected def dataInSearchTreeClear(): Unit =
    dataInSearchTree.clear()

  protected def dataInSearchTreeAddAll(data: Iterable[ValidData]): Unit =
    dataInSearchTree.addAll(data.map(_.asBoxedPayload))

  protected def dataInSearchTreeGet(interval: I): Iterable[ValidData] =
    BoxedPayload
      .deduplicate(dataInSearchTree.get(interval.asBox))
      .map(_.payload)
      .filter(_.interval intersects interval)

  protected def dataInSearchTreeGetByDomain(domainIndex: D): Option[ValidData] =
    dataInSearchTree
      .get(Box.at(domainIndex.asCoordinate))
      .collectFirst:
        case d if d.payload.interval.contains(domainIndex) => d.payload

  protected def dataInSearchTreeIntersects(interval: I): Boolean =
    dataInSearchTree.get(interval.asBox).exists(_.payload.interval intersects interval)

  /**
    * Remove, and possibly update, valid values on the target interval. If there are values valid on portions of the
    * interval, those values have their interval adjusted (e.g., shortened, shifted, split) accordingly. The logic of
    * remove and update are similar, and this method supports both.
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  protected def updateOrRemove(targetInterval: I, updateValue: V => Option[V]): Unit

  /**
    * Generic implementation to remove, and possibly update, valid values on the target interval. Each
    * dimension-specific implementation provides its own way to construct atomic non-intersecting intervals.
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    * @param atomic
    *   maps the overlap and its intersection with target to its atomic deconstruction -- a collection of intervals
    *   covering the overlap that is "separated" by the intersection. (See dimension-specific implementations of
    *   [[updateOrRemove]] and [[Interval1D.separateUsing]] for details.)
    */
  protected def updateOrRemoveGeneric(targetInterval: I, updateValue: V => Option[V])(
    atomic: (I, I) => Iterable[I]
  ): Unit = synchronized:
    val intersectingValues = getIntersecting(targetInterval).map: overlap =>
      overlap.interval
        .intersectionWith(targetInterval)
        .foreach: intersection => // always one
          val atomicNonIntersections = atomic(overlap.interval, intersection)

          // fast compression to minimize adds/updates (which are expensive) without having to build a TreeMap
          // (note that the result is in reverse order, but that shouldn't matter in later steps)
          val nonIntersections = atomicNonIntersections.foldLeft(List.empty[I]):
            case (lastInterval1 :: tail1, nextInterval) if lastInterval1 isAdjacentTo nextInterval =>
              val merged = nextInterval ∪ lastInterval1
              tail1 match // go back one more
                case lastInterval2 :: tail2 if lastInterval2 isAdjacentTo merged => (lastInterval2 ∪ merged) :: tail2
                case _                                                           => merged :: tail1
            case (priorIntervals, nextInterval) => nextInterval :: priorIntervals

          // remove the intersecting region if it happens to have the same key as the overlap
          if intersection hasSameStartAs overlap.interval then removeValidData(overlap)

          // add/update non-intersecting regions
          nonIntersections.foreach: subinterval =>
            if subinterval hasSameStartAs overlap.interval
            then updateValidData(newValidData(overlap.value, subinterval))
            else addValidData(newValidData(overlap.value, subinterval))

          // if there is an updated value, add it back in
          updateValue(overlap.value).foreach: newValue =>
            addValidData(newValidData(newValue, intersection))

      // intersecting value result for compression later
      overlap.value

    // compress all potentially affected values
    val intersectingValueSet = intersectingValues.toSet
    val potentiallyAffectedValues = intersectingValueSet ++ intersectingValueSet.flatMap(updateValue)
    potentiallyAffectedValues.foreach(compressInPlace)

  /**
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param interval
    *   interval where some value should be valid (existing or new)
    * @param value
    *   new value to be valid in the interval where no existing value is already valid
    */
  protected def fillInPlace[B <: V](interval: I, value: B): Unit

  /**
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
    that: Iterable[ValidData],
    mergeValues: (V, V) => V
  ): Unit = that.foreach: thatData =>
    updateOrRemove(thatData.interval, thisDataValue => Some(mergeValues(thisDataValue, thatData.value)))
    fillInPlace(thatData.interval, thatData.value)

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
  protected def compressInPlace(value: V): Unit

  /**
    * Internal method, to recompress in place.
    *
    * Unlike in 1D, there are no unique compressions in 2D and 3D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could
    * also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  protected def recompressInPlace(): Unit

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: Self

  /**
    * Returns all the intervals (compressed) in which there are valid values. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]].
    */
  def domain: Iterable[I]

  /**
    * Returns all the intervals (compressed) in which there are no valid values. That is, all intervals that are not in
    * the [[domain]]. See [[https://en.wikipedia.org/wiki/Domain_of_a_function]] and
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    */
  def domainComplement: Iterable[I]

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: Self & intervalidus.mutable.MutableBase[V, D, I, ValidData, DiffAction, ?]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: Self & intervalidus.immutable.ImmutableBase[V, D, I, ValidData, DiffAction, ?]

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: Self): Iterable[DiffAction]

  // ---------- Implemented here based on the above ----------

  /**
    * Returns all data that are valid on some or all of the provided interval.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: I): Iterable[ValidData] =
    experimental.control("noSearchTree")(
      experimentalResult = getAll.filter(_.interval intersects interval),
      nonExperimentalResult = dataInSearchTreeGet(interval)
    )

  /**
    * Are there values that are valid on some or all of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: I): Boolean =
    experimental.control("noSearchTree")(
      experimentalResult = getAll.exists(_.interval intersects interval),
      nonExperimentalResult = dataInSearchTreeIntersects(interval)
    )

  /**
    * Internal mutator to add, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData): Unit =
    // assert(!dataByStartAsc.isDefinedAt(data.interval.start))
    dataByStartAsc.addOne(data.withKey)
    dataByValue.addOne(data.value -> data)
    experimental.control("noSearchTree")(
      experimentalResult =
        // assert(!dataByStartDesc.isDefinedAt(data.interval.start))
        dataByStartDesc.addOne(data.withKey),
      nonExperimentalResult = dataInSearchTreeAdd(data)
    )

  /**
    * Internal mutator to update, where a value with v.interval.start already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData): Unit =
    // assert(dataByStartAsc.isDefinedAt(data.interval.start))
    val oldData = dataByStartAsc(data.interval.start)
    dataByValue.subtractOne(oldData.value -> oldData)
    dataByValue.addOne(data.value -> data)
    dataByStartAsc.update(data.interval.start, data)
    experimental.control("noSearchTree")(
      experimentalResult =
        // assert(dataByStartDesc.isDefinedAt(data.interval.start))
        dataByStartDesc.update(data.interval.start, data),
      nonExperimentalResult =
        dataInSearchTreeRemove(oldData)
        dataInSearchTreeAdd(data)
    )

  /**
    * Internal mutator to remove, where a known value already exists.
    *
    * @param oldData
    *   valid data to remove.
    */
  protected def removeValidData(oldData: ValidData): Unit =
    val key = oldData.interval.start
    dataByValue.subtractOne(oldData.value -> oldData)
    dataByStartAsc.remove(key)
    experimental.control("noSearchTree")(
      experimentalResult = dataByStartDesc.remove(key),
      nonExperimentalResult = dataInSearchTreeRemove(oldData)
    )

  /**
    * Internal mutator to remove, where a value with a known key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D): Unit =
    removeValidData(dataByStartAsc(key))

  protected def replaceValidData(data: Iterable[ValidData]): Unit =
    dataByStartAsc.clear()
    dataByStartAsc.addAll(data.map(_.withKey))
    dataByValue.clear()
    dataByValue.addAll(data.map(d => d.value -> d))
    experimental.control("noSearchTree")(
      experimentalResult =
        dataByStartDesc.clear()
        dataByStartDesc.addAll(dataByStartAsc)
      ,
      nonExperimentalResult =
        dataInSearchTreeClear()
        dataInSearchTreeAddAll(data)
    )

  // from PartialFunction
  override def isDefinedAt(key: D): Boolean = getAt(key).isDefined

  // from PartialFunction
  override def apply(domainIndex: D): V = getAt(domainIndex).getOrElse(
    throw Exception(s"Not defined at $domainIndex")
  )

  /**
    * Returns the value if a single, unbounded valid value exists, otherwise throws an exception.
    *
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals).
    */
  def get: V = getAll.headOption match
    case Some(d: ValidData) if d.interval.isUnbounded => d.value
    case Some(_)                                      => throw new NoSuchElementException("bounded get")
    case None                                         => throw new NoSuchElementException("empty get")

  /**
    * Returns Some value if a single, unbounded valid value, otherwise returns None.
    */
  def getOption: Option[V] = getAll.headOption.filter(_.interval.isUnbounded).map(_.value)

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
  def getDataAt(domainIndex: D): Option[ValidData] =
    experimental.control("noSearchTree")(
      experimentalResult = dataByStartDesc // Using reverse-key order allows us nearly O(1) for hits
        .valuesIteratorFrom(domainIndex) // (starting at or before the index)
        .filter(_.interval.end >= domainIndex) // but misses are still slow - this slightly improves miss performance
        .collectFirst:
          case d if d.interval.contains(domainIndex) => d,
      nonExperimentalResult = dataInSearchTreeGetByDomain(domainIndex)
    )

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
  def getAt(domainIndex: D): Option[V] = getDataAt(domainIndex).map(_.value)

  /**
    * Tests if there are no valid data in this structure.
    * @return
    *   true if there are no valid data, false otherwise.
    */
  def isEmpty: Boolean = dataByStartAsc.isEmpty

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
  def foldLeft[B](z: B)(op: (B, ValidData) => B): B = getAll.foldLeft(z)(op)

  /**
    * Get all valid data in interval start order
    */
  def getAll: Iterable[ValidData] = dataByStartAsc.values
