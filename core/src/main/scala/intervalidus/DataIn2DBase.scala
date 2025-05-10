package intervalidus

import intervalidus.Interval1D.{between, unbounded}
import intervalidus.collection.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * Constructs data in two-dimensional intervals.
  */
trait DataIn2DBaseObject extends DataIn2DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DataIn2DBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike](
    data: ValidData2D[V, R1, R2]
  )(using Experimental): DataIn2DBase[V, R1, R2]

  /**
    * Shorthand constructor for a single initial value that is valid in both full interval domains.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn2DBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike](value: V)(using Experimental): DataIn2DBase[V, R1, R2]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @return
    *   [[DataIn2DBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DomainValueLike, R2: DomainValueLike](initialData: Iterable[ValidData2D[V, R1, R2]])(using
    Experimental
  ): DataIn2DBase[V, R1, R2]

trait DataIn2DConstructorParams:
  protected def constructorParams[V, R1, R2](
    initialData: Iterable[ValidData2D[V, R1, R2]]
  )(using experimental: Experimental, domainValue1: DomainValueLike[R1], domainValue2: DomainValueLike[R2]): (
    mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]],
    mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]],
    MultiMapSorted[V, ValidData2D[V, R1, R2]],
    BoxTree[ValidData2D[V, R1, R2]]
  ) =
    val dataByStartAsc: mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]] =
      mutable.TreeMap.from(initialData.map(_.withKey))

    val dataByStartDesc: mutable.TreeMap[Domain2D[R1, R2], ValidData2D[V, R1, R2]] =
      experimental.control("noSearchTree")(
        experimentalResult = mutable.TreeMap.from(dataByStartAsc.iterator)(summon[Ordering[Domain2D[R1, R2]]].reverse),
        nonExperimentalResult = mutable.TreeMap()(summon[Ordering[Domain2D[R1, R2]]].reverse) // not used
      )

    val dataByValue: MultiMapSorted[V, ValidData2D[V, R1, R2]] =
      collection.mutable.MultiMapSorted.from(initialData.map(v => v.value -> v))

    val (r1, r2) = (summon[DomainValueLike[R1]], summon[DomainValueLike[R2]])
    val minPoint = Coordinate(r1.minValue.orderedHashValue, r2.minValue.orderedHashValue)
    val maxPoint = Coordinate(r1.maxValue.orderedHashValue, r2.maxValue.orderedHashValue)
    val boundary = Box(minPoint, maxPoint)

    val dataInSearchTree: BoxTree[ValidData2D[V, R1, R2]] =
      experimental.control("noSearchTree")(
        experimentalResult = BoxTree[ValidData2D[V, R1, R2]](boundary),
        nonExperimentalResult = BoxTree.from[ValidData2D[V, R1, R2]](boundary, initialData.map(_.asBoxedPayload))
      )

    (dataByStartAsc, dataByStartDesc, dataByValue, dataInSearchTree)

/**
  * Like [[DataIn1DBase]], data here have different values in different intervals. But here data values vary in two
  * dimensions. For example, one may want to represent when the data are valid in time and over certain versions, or in
  * two dimensions of time, simultaneously.
  *
  * We can capture the dependency between various values and related two-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in two dimensions.
  * @note
  *   Visualizing two-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt
  *   chart and there is a simple Visualize2D tool provided (in the test package... though maybe this should be its own
  *   separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  */
trait DataIn2DBase[V, R1: DomainValueLike, R2: DomainValueLike](using experimental: Experimental)
  extends intervalidus.DimensionalBase[
    V,
    Domain2D[R1, R2],
    Interval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2],
    DataIn2DBase[V, R1, R2]
  ]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval2D.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  override protected def newValidData(value: V, interval: Interval2D[R1, R2]): ValidData2D[V, R1, R2] =
    interval -> value

  private def subIntervalsWith[B, R: DomainValueLike](
    that: DataIn2DBase[B, R1, R2],
    f: Interval2D[R1, R2] => Interval1D[R]
  ): Iterable[Interval1D[R]] = Interval1D.uniqueIntervals(
    this.getAll.map(d => f(d.interval)).toSet ++ that.getAll.map(d => f(d.interval))
  )

  protected def zipData[B](that: DataIn2DBase[B, R1, R2]): Iterable[ValidData2D[(V, B), R1, R2]] =
    for
      horizontalSubInterval <- subIntervalsWith(that, _.horizontal)
      verticalSubInterval <- subIntervalsWith(that, _.vertical)
      subInterval = horizontalSubInterval x verticalSubInterval
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

  protected def zipAllData[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData2D[(V, B), R1, R2]] =
    subIntervalsWith(that, _.horizontal).flatMap: horizontalSubInterval =>
      subIntervalsWith(that, _.vertical).flatMap: verticalSubInterval =>
        val subInterval = horizontalSubInterval x verticalSubInterval
        val vOption = getIntersecting(subInterval).headOption.map(_.value)
        val bOption = that.getIntersecting(subInterval).headOption.map(_.value)
        val valuePair = (vOption, bOption) match
          case (None, None)       => None
          case (Some(v), Some(b)) => Some((v, b))
          case (Some(v), None)    => Some((v, thatElem))
          case (None, Some(b))    => Some((thisElem, b))
        valuePair.map(subInterval -> _)

  protected def getByHorizontalIndexData(
    horizontalIndex: Domain1D[R1]
  ): Iterable[ValidData1D[V, R2]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(Interval1D.intervalAt(horizontalIndex) x unbounded[R2])
    )
    candidates.collect:
      case ValidData2D(value, interval) if horizontalIndex ∈ interval.horizontal =>
        interval.vertical -> value

  protected def getByVerticalIndexData(
    verticalIndex: Domain1D[R2]
  ): Iterable[ValidData1D[V, R1]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(unbounded[R1] x Interval1D.intervalAt(verticalIndex))
    )
    candidates.collect:
      case ValidData2D(value, interval) if verticalIndex ∈ interval.vertical =>
        interval.horizontal -> value

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
  def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2DBase[(V, B), R1, R2]

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
  def zipAll[B](that: DataIn2DBase[B, R1, R2], thisElem: V, thatElem: B): DataIn2DBase[(V, B), R1, R2]

  /**
    * Flips this by swapping the vertical and horizontal interval components with one another in all data.
    */
  def flip: DataIn2DBase[V, R2, R1]

  /**
    * Project as one-dimensional data based on a horizontal domain element
    *
    * @param horizontalIndex
    *   the horizontal domain element
    * @return
    *   a one-dimensional projection
    */
  def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn1DBase[V, R2]

  /**
    * Project as one-dimensional data based on a vertical domain element
    *
    * @param verticalIndex
    *   the vertical domain element
    * @return
    *   a one-dimensional projection
    */
  def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn1DBase[V, R1]

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object - use Visualize2D (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value} ${v.interval.vertical}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )

  // ---------- Implement methods from DimensionalBase ----------

  override def domain: Iterable[Interval2D[R1, R2]] =
    Interval2D.compress(Interval2D.uniqueIntervals(getAll.map(_.interval)).filter(intersects))

  override def domainComplement: Iterable[Interval2D[R1, R2]] =
    Interval2D.complement(domain)

  override def diffActionsFrom(old: DataIn2DBase[V, R1, R2]): Iterable[DiffAction2D[V, R1, R2]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction2D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction2D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction2D.Delete(oldData.interval.start))
        case _                                                    => None

  override protected def compressInPlace(value: V): Unit = Interval2D.compressGeneric(
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

  override protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- Interval2D.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // always returns either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    dataByValue.keySet.foreach(compressInPlace)

  /**
    * @inheritdoc
    *
    * More specifically, this gets a bit complicated because there are two dimensions. Exclusions in one dimension can
    * have three remainders: none (simple), single (partial), and split. But two-dimensional exclusions have these same
    * three remainders in each dimension, so there are a total of 3 x 3 = 9 remainder cases. But there are symmetric
    * partners for 3 of them, so actually there are only 6 unique cases:
    *   1. simple = none + none (1 of 9)
    *   1. corner = single + single (1 of 9)
    *   1. hole = split + split (1 of 9)
    *   1. edge = none + single or single + none (2 of 9)
    *   1. slice = none + split or split + none (2 of 9)
    *   1. bite = single + split or split + single (2 of 9)
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  override protected def updateOrRemove(
    targetInterval: Interval2D[R1, R2],
    updateValue: V => Option[V]
  ): Unit = experimental.control("bruteForceUpdate")(
    nonExperimentalResult = updateOrRemoveOptimized(targetInterval, updateValue),
    experimentalResult = updateOrRemoveBruteForce(targetInterval, updateValue)
  )

  /*
   * The normal, faster (1.4 - 1.9 times faster), but more complex implementation.
   * Considers each case separately.
   */
  private def updateOrRemoveOptimized(
    targetInterval: Interval2D[R1, R2],
    updateValue: V => Option[V]
  ): Unit = synchronized:
    import Interval1D.Remainder

    // Utility to find the complement of single remaining (intervals must either start or end together)
    def excludeRemaining[T: DomainValueLike](
      full: Interval1D[T],
      remaining: Interval1D[T]
    ): Interval1D[T] =
      if remaining hasSameStartAs full
      then full.fromAfter(remaining.end)
      else full.toBefore(remaining.start)

    val intersecting = getIntersecting(targetInterval)
    // These values are targets for compression later
    val potentiallyAffectedValues = intersecting.map(_.value).toSet ++ intersecting.map(_.value).flatMap(updateValue)

    intersecting.foreach: overlap =>
      val newValueOption = updateValue(overlap.value)

      /*
       * These methods are generic by dimension to accommodate symmetric operations without code duplication.
       * In general, "before" can be left or below, and "after" can be right or above.
       */

      // total overlap in one dimension with a single overlap in the other - adjust in the other
      def edge[T: DomainValueLike](
        remaining1D: Interval1D[T],
        extractOverlap1D: Interval2D[R1, R2] => Interval1D[T],
        modify2D: (Interval2D[R1, R2], Interval1D[T]) => Interval2D[R1, R2]
      ): Unit =
        if remaining1D hasSameStartAs extractOverlap1D(overlap.interval)
        then // same start: shorten
          updateValidData(modify2D(overlap.interval, remaining1D) -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(
              modify2D(
                overlap.interval,
                extractOverlap1D(overlap.interval).fromAfter(remaining1D.end)
              ) -> newValue
            )
        else // different start: remove and add remaining after, new before
          removeValidData(overlap)
          addValidData(modify2D(overlap.interval, remaining1D) -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(
              modify2D(
                overlap.interval,
                extractOverlap1D(overlap.interval).toBefore(remaining1D.start)
              ) -> newValue
            )

      // total overlap in one dimension with a split in the other - shorten before, add remaining after
      def slice[T: DomainValueLike](
        remainingBefore1D: Interval1D[T],
        remainingAfter1D: Interval1D[T],
        modify2D: (Interval2D[R1, R2], Interval1D[T]) => Interval2D[R1, R2]
      ): Unit =
        updateValidData(modify2D(overlap.interval, remainingBefore1D) -> overlap.value)
        newValueOption.foreach: newValue =>
          val sliceBit = between(remainingBefore1D, remainingAfter1D)
          addValidData(modify2D(overlap.interval, sliceBit) -> newValue)
        addValidData(modify2D(overlap.interval, remainingAfter1D) -> overlap.value)

      overlap.interval \ targetInterval match

        /*
         * One kind of simple, corner, and hole
         */

        // simple: total overlap in dim 1 and 2 (no remainders) - just update/remove the whole thing
        case (Remainder.None, Remainder.None) =>
          newValueOption match
            case Some(newValue) => updateValidData(overlap.interval -> newValue)
            case None           => removeValidData(overlap)

        // corner: partial overlap in both dim 1 and 2 - remove and add 2 remaining (compress later)
        case (Remainder.Single(horizontalRemaining), Remainder.Single(verticalRemaining)) =>
          // can be above or below.
          val verticalCornerBit = excludeRemaining(overlap.interval.vertical, verticalRemaining)
          // can be to the left or right.
          val horizontalCornerBit = excludeRemaining(overlap.interval.horizontal, horizontalRemaining)
          removeValidData(overlap)
          addValidData(overlap.interval.withVertical(verticalRemaining) -> overlap.value)
          addValidData((horizontalRemaining x verticalCornerBit) -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData((horizontalCornerBit x verticalCornerBit) -> newValue)

        //  hole: split in dim 1 and 2 - shorten below, add two sides, and the bit above
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          // Equivalent to ((overlap.interval.vertical \ bottomRemaining).head \ topRemaining).head
          val verticalHoleBit = between(belowRemaining, aboveRemaining)
          updateValidData(overlap.interval.withVertical(belowRemaining) -> overlap.value) // shorten below the hole
          addValidData((leftRemaining x verticalHoleBit) -> overlap.value) // left side of the hole
          addValidData((rightRemaining x verticalHoleBit) -> overlap.value) // right side of the hole
          addValidData(overlap.interval.withVertical(aboveRemaining) -> overlap.value) // above the hole
          newValueOption.foreach: newValue =>
            val horizontalHoleBit = between(leftRemaining, rightRemaining)
            addValidData((horizontalHoleBit x verticalHoleBit) -> newValue) // the hole is updated

        /*
         * Two kinds of edges
         */

        // total overlap in dim 1 with partial overlap in dim 2 - adjust in dim 2
        case (Remainder.None, Remainder.Single(remainingVertical)) =>
          edge(remainingVertical, _.vertical, _.withVertical(_))

        // partial overlap in dim 1 with total overlap in dim 2 - adjust in dim 1
        case (Remainder.Single(remainingHorizontal), Remainder.None) =>
          edge(remainingHorizontal, _.horizontal, _.withHorizontal(_))

        /*
         * Two kinds of slices
         */

        // total overlap in dim 1 with split in dim 2 - shorten bottom, add top remaining
        case (Remainder.None, Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          slice(belowRemaining, aboveRemaining, _.withVertical(_))

        // split in dim 1 with no remainder in dim 2 - shorten left, add right remaining
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.None) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          slice(leftRemaining, rightRemaining, _.withHorizontal(_))

        /*
         * Two kinds of bites
         */

        // partial overlap in dim 1 with split in dim 2 - shorten bottom and add 2 remaining
        case (Remainder.Single(remainingHorizontal), Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          val biteBitHorizontal = excludeRemaining(overlap.interval.horizontal, remainingHorizontal)
          val biteBitVertical = between(belowRemaining, aboveRemaining)
          if overlap.interval.horizontal hasSameStartAs remainingHorizontal
          then
            // shorten on left to before bite
            updateValidData(overlap.interval.withHorizontal(remainingHorizontal) -> overlap.value)
            // below the bite
            addValidData(
              overlap.interval.withHorizontal(biteBitHorizontal).withVertical(belowRemaining) -> overlap.value
            )
            // above the bite
            addValidData(
              overlap.interval.withHorizontal(biteBitHorizontal).withVertical(aboveRemaining) -> overlap.value
            )
          else
            // shorten on left to below the bite
            updateValidData(
              overlap.interval.withHorizontal(biteBitHorizontal).withVertical(belowRemaining) -> overlap.value
            )
            // above the bite
            addValidData(
              overlap.interval.withHorizontal(biteBitHorizontal).withVertical(aboveRemaining) -> overlap.value
            )
            // on the right of the bite
            addValidData(overlap.interval.withHorizontal(remainingHorizontal) -> overlap.value)

          newValueOption.foreach: newValue =>
            // the bite is updated
            addValidData(
              overlap.interval.withHorizontal(biteBitHorizontal).withVertical(biteBitVertical) -> newValue
            )

        // split in dim 1 with partial overlap in dim 2 - shorten left and add 2 remaining
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.Single(remainingVertical)) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          val biteBitHorizontal = between(leftRemaining, rightRemaining)
          val biteBitVertical = excludeRemaining(overlap.interval.vertical, remainingVertical)
          // shorten on left to before bite
          updateValidData(overlap.interval.withHorizontal(leftRemaining) -> overlap.value)
          // above/below the bite to the end
          addValidData(
            overlap.interval
              .withHorizontal(biteBitHorizontal ∪ rightRemaining)
              .withVertical(remainingVertical) -> overlap.value
          )
          // to the right of the bite
          addValidData(overlap.interval.withHorizontal(rightRemaining).withVertical(biteBitVertical) -> overlap.value)

          newValueOption.foreach: newValue =>
            // the bite is updated
            addValidData(overlap.interval.withVertical(biteBitVertical).withHorizontal(biteBitHorizontal) -> newValue)

    potentiallyAffectedValues.foreach(compressInPlace)

  /*
   * The simpler but slower and less efficient implementation.
   * Benchmarks show this makes the remove operation 1.4 to 1.9 times slower.
   */
  private def updateOrRemoveBruteForce(
    targetInterval: Interval2D[R1, R2],
    updateValue: V => Option[V]
  ): Unit = synchronized:
    import Interval1D.Remainder

    val intersecting = getIntersecting(targetInterval)
    val potentiallyAffectedValues = intersecting.map(_.value).toSet ++ intersecting.map(_.value).flatMap(updateValue)

    intersecting.foreach: overlap =>
      val newValueOption = updateValue(overlap.value)

      def excludeOverlapRemainder1D[T: DomainValueLike](
        extractFromOverlap: Interval2D[R1, R2] => Interval1D[T],
        remainder: Remainder[Interval1D[T]]
      ): Interval1D[T] =
        val full = extractFromOverlap(overlap.interval)
        remainder match
          case Remainder.None =>
            full
          case Remainder.Single(remaining) if remaining hasSameStartAs full =>
            full.fromAfter(remaining.end)
          case Remainder.Single(remaining) =>
            full.toBefore(remaining.start)
          case Remainder.Split(leftRemaining, rightRemaining) =>
            between(leftRemaining, rightRemaining)

      def allOverlapSubintevals1D[T: DomainValueLike](
        extractFromOverlap: Interval2D[R1, R2] => Interval1D[T],
        remainder: Remainder[Interval1D[T]]
      ): Seq[Interval1D[T]] =
        val excluded = excludeOverlapRemainder1D(extractFromOverlap, remainder)
        remainder match
          case Remainder.None                                 => Seq(excluded)
          case Remainder.Single(remaining)                    => Seq(remaining, excluded).sorted
          case Remainder.Split(leftRemaining, rightRemaining) => Seq(leftRemaining, excluded, rightRemaining)

      val (horizontalRemainder, verticalRemainder) = overlap.interval \ targetInterval
      val excludedSubinterval =
        excludeOverlapRemainder1D(_.horizontal, horizontalRemainder) x
          excludeOverlapRemainder1D(_.vertical, verticalRemainder)
      if excludedSubinterval hasSameStartAs overlap.interval then removeValidData(overlap)
      for
        horizontal <- allOverlapSubintevals1D(_.horizontal, horizontalRemainder)
        vertical <- allOverlapSubintevals1D(_.vertical, verticalRemainder)
      do
        val subinterval = horizontal x vertical
        if subinterval != excludedSubinterval
        then
          if subinterval hasSameStartAs overlap.interval
          then updateValidData(subinterval -> overlap.value)
          else addValidData(subinterval -> overlap.value)

      newValueOption.foreach: newValue =>
        addValidData(excludedSubinterval -> newValue)

    potentiallyAffectedValues.foreach(compressInPlace)

  override protected def fillInPlace[B <: V](interval: Interval2D[R1, R2], value: B): Unit = synchronized:
    val intersectingIntervals = getIntersecting(interval).map(_.interval)
    Interval2D
      .uniqueIntervals(intersectingIntervals.toSeq :+ interval)
      .foreach: i =>
        if interval.intersects(i) && !this.intersects(i) then addValidData(i -> value)
    compressInPlace(value)
