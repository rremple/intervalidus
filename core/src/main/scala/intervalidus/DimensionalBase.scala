package intervalidus

import intervalidus.Domain.NonEmptyTail
import intervalidus.DomainLike.given
import intervalidus.collection.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * Constructs data in multidimensional intervals.
  */
trait DimensionalBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DimensionalBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): DimensionalBase[V, D]

  /**
    * Shorthand constructor for a single initial value that is valid in all full interval domains.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DimensionalBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DimensionalBase[V, D]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam D
    *   the type of domain assigned to each value.
    * @return
    *   [[DimensionalBase]] structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using
    Experimental
  ): DimensionalBase[V, D]

trait DimensionalBaseConstructorParams:
  protected def constructorParams[V, D <: NonEmptyTuple](
    initialData: Iterable[ValidData[V, D]]
  )(using domainValue: DomainLike[D])(using Experimental): (
    mutable.TreeMap[D, ValidData[V, D]],
    MultiMapSorted[V, ValidData[V, D]],
    BoxTree[ValidData[V, D]]
  ) =
    val dataByStartAsc: mutable.TreeMap[D, ValidData[V, D]] =
      mutable.TreeMap.from(initialData.map(_.withKey))

    val dataByValue: MultiMapSorted[V, ValidData[V, D]] =
      collection.mutable.MultiMapSorted.from(initialData.map(v => v.value -> v))

    val dataInSearchTree: BoxTree[ValidData[V, D]] =
      BoxTree.from[ValidData[V, D]](
        Interval(domainValue.bottom, domainValue.top).asBox,
        initialData.map(_.asBoxedPayload)
      )

    (dataByStartAsc, dataByValue, dataInSearchTree)

/**
  * Base for all dimensional data, both mutable and immutable, of arbitrary dimensions.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals, must be [[DomainLike]].
  */
trait DimensionalBase[V, D <: NonEmptyTuple](using
  domainLike: DomainLike[D]
)(using Experimental)
  extends PartialFunction[D, V]:

  // from Object
  // print a uniform grid representing the data.
  override def toString: String =
    // tuples of first dimension start string, first dimension end string, first dimension string (header)
    val horizontalIntervalStrings = domainLike.intervalPreprocessForGrid(getAll.map(_.interval))
    // tuples of first dimension start string, first dimension end string, value + remaining dimension string
    val validDataStrings = getAll.map(_.preprocessForGrid)
    val maxDataSize = validDataStrings
      .map(_._3.length + 3)
      .maxOption
      .getOrElse(3)
    val maxHorizontalIntervalsSize = horizontalIntervalStrings
      .map(_._3.length)
      .maxOption
      .getOrElse(7)

    val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

    def pad(chars: Int, p: String = " "): String = p * chars

    val (horizontalStringBuilder, horizontalStartPositionBuilder, horizontalEndPositionBuilder) =
      horizontalIntervalStrings.zipWithIndex.foldLeft(
        (StringBuilder(), Map.newBuilder[String, Int], Map.newBuilder[String, Int])
      ):
        case ((stringBuilder, startPositionBuilder, endPositionBuilder), ((startString, endString, formatted), pos)) =>
          startPositionBuilder.addOne(startString, stringBuilder.size)
          stringBuilder.append(formatted)
          val padTo = cellSize * (pos + 1)
          if stringBuilder.size < padTo then stringBuilder.append(pad(padTo - stringBuilder.size))
          endPositionBuilder.addOne(endString, stringBuilder.size)
          (stringBuilder, startPositionBuilder, endPositionBuilder)

    val horizontalStartPosition = horizontalStartPositionBuilder.result()
    val horizontalEndPosition = horizontalEndPositionBuilder.result()
    horizontalStringBuilder.append("|\n")

    validDataStrings
      // .sortBy(dataToSortBy) // if needed, could be hard to implement...
      .foreach: (startString, endString, valueString) =>
        val leftPosition = horizontalStartPosition(startString)
        val rightPosition = horizontalEndPosition(endString)
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
  protected def dataByStartAsc: scala.collection.mutable.TreeMap[D, ValidData[V, D]]

  /**
    * Another internal shadow data structure where all the interval-bounded data are also stored, but using the value
    * itself as the key (for faster compression, which is done by value). The ValidData[V, D] are stored in a sorted
    * set, so they are retrieved in key order, making compression operations repeatable.
    */
  protected def dataByValue: MultiMapSorted[V, ValidData[V, D]]

  protected def dataInSearchTree: BoxTree[ValidData[V, D]]

  protected def dataInSearchTreeAdd(data: ValidData[V, D]): Unit =
    dataInSearchTree.addOne(data.asBoxedPayload)

  protected def dataInSearchTreeRemove(data: ValidData[V, D]): Unit =
    dataInSearchTree.remove(data.asBoxedPayload)

  protected def dataInSearchTreeClear(): Unit =
    dataInSearchTree.clear()

  protected def dataInSearchTreeAddAll(data: Iterable[ValidData[V, D]]): Unit =
    dataInSearchTree.addAll(data.map(_.asBoxedPayload))

  protected def dataInSearchTreeGet(interval: Interval[D]): Iterable[ValidData[V, D]] =
    BoxedPayload
      .deduplicate(dataInSearchTree.get(interval.asBox))
      .map(_.payload)
      .filter(_.interval intersects interval)

  protected def dataInSearchTreeGetByDomain(domainIndex: D): Option[ValidData[V, D]] =
    dataInSearchTree
      .get(Box.at(domainIndex.asCoordinate))
      .collectFirst:
        case d if d.payload.interval.contains(domainIndex) => d.payload

  protected def dataInSearchTreeIntersects(interval: Interval[D]): Boolean =
    dataInSearchTree.get(interval.asBox).exists(_.payload.interval intersects interval)

  /**
    * Remove, and possibly update, valid values on the target interval. If there are values valid on portions of the
    * interval, those values have their interval adjusted (e.g., shortened, shifted, split) accordingly. The logic of
    * remove and update are similar, and this method supports both.
    *
    * More specifically, this gets even more complicated because there are multipledimensions. Exclusions in one
    * dimension can have three remainders: none (simple), single (partial), and split. But multidimensional exclusions
    * have these same three remainders in each dimension, so there are a total of 3<sup>n</sup> remainder cases. But
    * there is symmetry because order doesn't matter, so actually there are only (3+n-1)! / (3! x (n-1)!) (we want the
    * unique combinations of choosing from 3 remainder scenarios n times, where order doesn't matter).
    *
    * For example, with n = 4, this reduces to 6!/(3! x 3!) = 20 unique cases. But just giving these cases intuitive
    * names would be hard enough. (Nothing in four dimensions is ever intuitive!)
    *
    * Falling back to more familiar space, using n = 3 this reduces to 5!/(2! x 3!) = 10 unique cases, enumerated below:
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
  protected def updateOrRemove(targetInterval: Interval[D], updateValue: V => Option[V]): Unit = synchronized:
    val intersectingValues = getIntersecting(targetInterval).map: overlap =>
      overlap.interval
        .intersectionWith(targetInterval)
        .foreach: intersection => // always one
          // maps the overlap and its intersection with target to its atomic deconstruction -- a collection of intervals
          // covering the overlap that is "separated" by the intersection.
          val atomicNonIntersections = overlap.interval.separateUsing(intersection).filter(_ != intersection)
          // fast compression to minimize adds/updates (which are expensive) without having to build a TreeMap
          // (note that the result is in reverse order, but that shouldn't matter in later steps)
          val nonIntersections = atomicNonIntersections.foldLeft(List.empty[Interval[D]]):
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
            then updateValidData(subinterval -> overlap.value)
            else addValidData(subinterval -> overlap.value)

          // if there is an updated value, add it back in
          updateValue(overlap.value).foreach: newValue =>
            addValidData(intersection -> newValue)

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
  protected def fillInPlace[B <: V](interval: Interval[D], value: B): Unit = synchronized:
    val intersectingIntervals = getIntersecting(interval).map(_.interval)
    Interval
      .uniqueIntervals(intersectingIntervals.toSeq :+ interval)
      .foreach: i =>
        if interval.intersects(i) && !this.intersects(i) then addValidData(i -> value)
    compressInPlace(value)

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
    that: Iterable[ValidData[V, D]],
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
  protected def compressInPlace(value: V): Unit = Interval.compressGeneric(
    initialState = (), // no state -- updates applied in place
    result = identity, // no result -- updates applied in place
    dataIterable = _ => dataByValue.get(value),
    interval = _.interval,
    valueMatch = _.value == _.value,
    lookup = (_, start) => dataByStartAsc.get(start),
    compressAdjacent = (r, s, intervalByStart) =>
      removeValidData(s)
      updateValidData(r.interval ∪ s.interval -> value)
  )

  /**
    * Internal method, to recompress in place.
    *
    * Unlike in 1D, there are no unique compressions in higher dimensions. For example {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- Interval.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // always returns either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    dataByValue.keySet.foreach(compressInPlace)

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: DimensionalBase[V, D]

  /**
    * Returns all the intervals (compressed) in which there are valid values. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]].
    */
  def domain: Iterable[Interval[D]] = Interval.compress(
    Interval.uniqueIntervals(getAll.map(_.interval)).filter(intersects)
  )

  /**
    * Returns all the intervals (compressed) in which there are no valid values. That is, all intervals that are not in
    * the [[domain]]. See [[https://en.wikipedia.org/wiki/Domain_of_a_function]] and
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    */
  def domainComplement: Iterable[Interval[D]] = Interval.compress(
    Interval.uniqueIntervals(getAll.map(_.interval) ++ Iterable(Interval.unbounded[D])).filter(!intersects(_))
  )

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: intervalidus.mutable.MutableBase[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: intervalidus.immutable.ImmutableBase[V, D, ?]

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DimensionalBase[V, D]): Iterable[DiffAction[V, D]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction.Delete(oldData.interval.start))
        case _                                                    => None

  // ---------- Implemented here based on the above ----------

  /**
    * Returns all data that are valid on some or all of the provided interval.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: Interval[D]): Iterable[ValidData[V, D]] = dataInSearchTreeGet(interval)

  /**
    * Are there values that are valid on some or all of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: Interval[D]): Boolean = dataInSearchTreeIntersects(interval)

  /**
    * Internal mutator to add, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData[V, D]): Unit =
    // assert(!dataByStartAsc.isDefinedAt(data.interval.start))
    dataByStartAsc.addOne(data.withKey)
    dataByValue.addOne(data.value -> data)
    dataInSearchTreeAdd(data)

  /**
    * Internal mutator to update, where a value with v.interval.start already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData[V, D]): Unit =
    // assert(dataByStartAsc.isDefinedAt(data.interval.start))
    val oldData = dataByStartAsc(data.interval.start)
    dataByValue.subtractOne(oldData.value -> oldData)
    dataByValue.addOne(data.value -> data)
    dataByStartAsc.update(data.interval.start, data)
    dataInSearchTreeRemove(oldData)
    dataInSearchTreeAdd(data)

  /**
    * Internal mutator to remove, where a known value already exists.
    *
    * @param oldData
    *   valid data to remove.
    */
  protected def removeValidData(oldData: ValidData[V, D]): Unit =
    val key = oldData.interval.start
    dataByValue.subtractOne(oldData.value -> oldData)
    dataByStartAsc.remove(key)
    dataInSearchTreeRemove(oldData)

  /**
    * Internal mutator to remove, where a value with a known key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D): Unit =
    removeValidData(dataByStartAsc(key))

  protected def replaceValidData(data: Iterable[ValidData[V, D]]): Unit =
    dataByStartAsc.clear()
    dataByStartAsc.addAll(data.map(_.withKey))
    dataByValue.clear()
    dataByValue.addAll(data.map(d => d.value -> d))
    dataInSearchTreeClear()
    dataInSearchTreeAddAll(data)

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
    case Some(d: ValidData[V, D]) if d.interval.isUnbounded => d.value
    case Some(_)                                            => throw new NoSuchElementException("bounded get")
    case None                                               => throw new NoSuchElementException("empty get")

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
  def getDataAt(domainIndex: D): Option[ValidData[V, D]] =
    dataInSearchTreeGetByDomain(domainIndex)

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
  def foldLeft[B](z: B)(op: (B, ValidData[V, D]) => B): B = getAll.foldLeft(z)(op)

  /**
    * Get all valid data in interval start order
    */
  def getAll: Iterable[ValidData[V, D]] = dataByStartAsc.values

  protected def zipData[B](that: DimensionalBase[B, D]): Iterable[ValidData[(V, B), D]] =
    for
      subInterval <- Interval.uniqueIntervals(getAll.map(_.interval) ++ that.getAll.map(_.interval))
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

  protected def zipAllData[B](
    that: DimensionalBase[B, D],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData[(V, B), D]] =
    for
      subInterval <- Interval.uniqueIntervals(getAll.map(_.interval) ++ that.getAll.map(_.interval))
      vOption = getIntersecting(subInterval).headOption.map(_.value)
      bOption = that.getIntersecting(subInterval).headOption.map(_.value)
      valuePair <- (vOption, bOption) match
        case (None, None)       => None
        case (Some(v), Some(b)) => Some((v, b))
        case (Some(v), None)    => Some((v, thatElem))
        case (None, Some(b))    => Some((thisElem, b))
    yield subInterval -> valuePair

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
    * (all intersections) in a pair. If one of the two collections has a valid value in an interval where the other one
    * doesn't, placeholder elements are used in the result. The other structure can have a different value type but must
    * have the same interval type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisElem
    *   placeholder element used in intervals where data are valid in that but not this.
    * @param thatElem
    *   placeholder element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](that: DimensionalBase[B, D], thisElem: V, thatElem: B): DimensionalBase[(V, B), D]

  protected def getByHeadIndexData[H: DomainValueLike](headIndex: Domain1D[H])(using
    D =:= Domain1D[H] *: Tuple.Tail[D],
    DomainLike[NonEmptyTail[D]]
  ): Iterable[ValidData[V, NonEmptyTail[D]]] =
    val lookup = Interval.unbounded[D].withHeadUpdate[H](_ => Interval1D.intervalAt(headIndex))
    dataInSearchTreeGet(lookup).map:
      case ValidData(value, interval) => interval.tailInterval[NonEmptyTail[D]] -> value

  /**
    * Project as lower dimensional data based on a head domain element
    *
    * @param headIndex
    *   the first dimension domain element
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByHeadIndex[H: DomainValueLike](headIndex: Domain1D[H])(using
    D =:= Domain1D[H] *: Tuple.Tail[D],
    DomainLike[NonEmptyTail[D]]
  ): DimensionalBase[V, NonEmptyTail[D]]

object DimensionalBase:
  type In1D[V, R1] = DimensionalBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalBase[V, Domain.In4D[R1, R2, R3, R4]]
