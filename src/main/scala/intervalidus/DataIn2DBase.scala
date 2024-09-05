package intervalidus

import DiscreteDomain1D.Bottom
import DiscreteInterval1D.interval

import scala.annotation.tailrec
import scala.collection.mutable

trait DataIn2DBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DataIn2DBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](
    data: ValidData2D[V, R1, R2]
  ): DataIn2DBase[V, R1, R2]

  /**
    * Shorthand constructor for a single initial value that is valid in both full interval domains.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn2DBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue](value: V): DataIn2DBase[V, R1, R2]

/**
  * Like [[DataIn1DBase]], data here have different values in different discrete intervals. But here data values vary in
  * two dimensions. For example, one may want to represent when the data are valid in time and over certain versions
  * simultaneously.
  *
  * We can capture the dependency between various values and related two-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in two dimensions. Note that visualizing
  * two-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt chart and there is
  * a simple Visualize tool provided (in the test package... though maybe this should be its own separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  * @param initialData
  *   (optional) a collection of valid data to start with -- intervals must be disjoint in two dimensions.
  */
trait DataIn2DBase[V, R1: DiscreteValue, R2: DiscreteValue](
  initialData: Iterable[ValidData2D[V, R1, R2]]
) extends DimensionalBase[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2], ValidData2D[V, R1, R2]]:

  override protected def newValidData(value: V, interval: DiscreteInterval2D[R1, R2]): ValidData2D[V, R1, R2] =
    interval -> value

  private def subIntervalsWith[B, R: DiscreteValue](
    that: DataIn2DBase[B, R1, R2],
    f: DiscreteInterval2D[R1, R2] => DiscreteInterval1D[R]
  ): Iterable[DiscreteInterval1D[R]] = DiscreteInterval1D.uniqueIntervals(
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
    * Project as 1-dimensional data based on a horizontal domain element
    *
    * @param horizontalIndex
    *   the horizontal domain element
    * @return
    *   a 1-dimensional projection
    */
  def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn1DBase[V, R2]

  /**
    * Project as 1-dimensional data based on a vertical domain element
    *
    * @param verticalIndex
    *   the vertical domain element
    * @return
    *   a 1-dimensional projection
    */
  def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1DBase[V, R1]

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object - use Visualize (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value} ${v.interval.vertical}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )

  /**
    * Internal method, to compress in place. Structure is parameterized to support both mutable and immutable
    * compression. (Immutable compression acts on a copy.) Assumes caller does synchronization (if needed). Assumes
    * underlying data are disjoint, so no need to address intersections.
    *
    * @param data
    *   structure that should be compressed
    * @param value
    *   value to be evaluate
    * @return
    *   this structure once compressed (not a copy)
    */
  protected def compressInPlace(data: DataIn2DBase[V, R1, R2])(value: V): Unit =
    /*
     * Each mutation gives rise to other compression possibilities. And applying a compression action can invalidate
     * the remainder of the actions (e.g., three-in-a-row). Unlike in one dimension, there is no safe order to fold
     * over to avoid these issues. So, instead, we evaluate every entry with every other entry, get the first
     * compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(): Unit =
      val firstActions = dataByStartAsc.values
        .filter(_.value == value)
        .map: r =>
          val rightKey = r.interval.horizontal.end.successor x r.key.verticalIndex
          val upperKey = r.key.horizontalIndex x r.interval.vertical.end.successor
          val rightAdjacent = dataByStartAsc
            .get(rightKey)
            .filter(s => s.value == value && (s.interval isRightAdjacentTo r.interval))
          val upperAdjacent = dataByStartAsc
            .get(upperKey)
            .filter(s => s.value == value && (s.interval isUpperAdjacentTo r.interval))
          rightAdjacent // preferred
            .orElse(upperAdjacent)
            .map: s =>
              Seq(
                () => data.removeValidDataByKey(s.key),
                () => data.updateValidData(r.interval ∪ s.interval -> value)
              )
        .collectFirst:
          case Some(actions) => actions
      firstActions match
        case None => ()
        case Some(actions) =>
          actions.foreach(_.apply())
          compressRecursively()

    compressRecursively()

  /**
    * Unlike in 1D, there is no unique compression in 2D. For example {[1..5], [1..2]} + {[1..2], [3..4]} could also be
    * represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data so there is a unique arrangement of "atomic" intervals. In the above example, that
    * would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it
    * recompresses the data, which results in a unique physical representation. It may be useful when comparing two
    * structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- DiscreteInterval2D.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // will always return either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    getAll.map(_.value).toSet.foreach(compressInPlace(this))

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn2DBase[V, R1, R2]): Iterable[DiffAction2D[V, R1, R2]] =
    (dataByStartDesc.keys.toSet ++ old.dataByStartDesc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartDesc.get(key), dataByStartDesc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction2D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction2D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction2D.Delete(oldData.key))
        case _                                                    => None

  protected def getByHorizontalIndexData(
    horizontalIndex: DiscreteDomain1D[R1]
  ): Iterable[ValidData1D[V, R2]] = getAll.collect:
    case ValidData2D(value, interval) if horizontalIndex ∈ interval.horizontal =>
      interval.vertical -> value

  protected def getByVerticalIndexData(
    verticalIndex: DiscreteDomain1D[R2]
  ): Iterable[ValidData1D[V, R1]] = getAll.collect:
    case ValidData2D(value, interval) if verticalIndex ∈ interval.vertical =>
      interval.horizontal -> value

  // ---------- Implement methods from DimensionalBase ----------

  protected override val dataByStartAsc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[V, R1, R2]] =
    mutable.TreeMap.from(initialData.map(v => v.key -> v))
  require(DiscreteInterval2D.isDisjoint(getAll.map(_.interval)))

  protected override val dataByStartDesc: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[V, R1, R2]] =
    mutable.TreeMap.from(dataByStartAsc.iterator)(summon[Ordering[DiscreteDomain2D[R1, R2]]].reverse)

  /**
    * @inheritdoc
    *
    * More specifically, this gets a bit complicated because there are two dimensions. Exclusions in one dimension can
    * have three remainders: none (simple), single (partial), and split. But two dimensional exclusions have these same
    * three remainders in each dimension, so there are a total of 3 x 3 = 9 remainder cases. But there are symmetric
    * partners for 3 of them, so actually there are only 6 unique cases:
    *   1. simple = none + none (1 of 9)
    *   1. corner = single + single (1 of 9)
    *   1. hole = split + split (1 of 9)
    *   1. edge = none + single or single + none (2 of 9)
    *   1. slice = none + split or split + none (2 of 9)
    *   1. bite = single + split or split + single (2 of 9)
    * @param targetInterval
    *   the interval where any valid values are removed/updated.
    * @param newValueOption
    *   when defined, the value to be stored as part of an update
    */
  protected override def updateOrRemove(
    targetInterval: DiscreteInterval2D[R1, R2],
    newValueOption: Option[V]
  ): Unit = synchronized:
    import DiscreteInterval1D.Remainder

    // Utility to find the complement of split remaining
    def between[T: DiscreteValue](
      beforeRemaining: DiscreteInterval1D[T],
      afterRemaining: DiscreteInterval1D[T]
    ): DiscreteInterval1D[T] = interval(beforeRemaining.end.successor, afterRemaining.start.predecessor)

    // Utility to find the complement of single remaining (intervals must either start or end together)
    def excludeRemaining[T: DiscreteValue](
      full: DiscreteInterval1D[T],
      remaining: DiscreteInterval1D[T]
    ): DiscreteInterval1D[T] =
      if remaining hasSameStartAs full
      then full.startingAfter(remaining.end)
      else full.endingBefore(remaining.start)

    getIntersecting(targetInterval).foreach: overlap =>

      /*
       * These methods are generic by dimension to accommodate symmetric operations without code duplication.
       * In general, "before" can be left or below, and "after" can be right or above.
       */

      // total overlap in one dimension with a single overlap in the other - adjust in the other
      def edge[T: DiscreteValue](
        remaining1D: DiscreteInterval1D[T],
        extractOverlap1D: DiscreteInterval2D[R1, R2] => DiscreteInterval1D[T],
        modify2D: (DiscreteInterval2D[R1, R2], DiscreteInterval1D[T]) => DiscreteInterval2D[R1, R2]
      ): Unit =
        if remaining1D hasSameStartAs extractOverlap1D(overlap.interval)
        then // same start: shorten
          updateValidData(modify2D(overlap.interval, remaining1D) -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(
              modify2D(
                overlap.interval,
                extractOverlap1D(overlap.interval).startingAfter(remaining1D.end)
              ) -> newValue
            )
        else // different start: remove and add remaining after, new before
          removeValidDataByKey(overlap.key)
          addValidData(modify2D(overlap.interval, remaining1D) -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(
              modify2D(
                overlap.interval,
                extractOverlap1D(overlap.interval).endingBefore(remaining1D.start)
              ) -> newValue
            )

      // total overlap in one dimension with a split in the other - shorten before, add remaining after
      def slice[T: DiscreteValue](
        remainingBefore1D: DiscreteInterval1D[T],
        remainingAfter1D: DiscreteInterval1D[T],
        modify2D: (DiscreteInterval2D[R1, R2], DiscreteInterval1D[T]) => DiscreteInterval2D[R1, R2]
      ): Unit =
        updateValidData(modify2D(overlap.interval, remainingBefore1D) -> overlap.value)
        newValueOption.foreach: newValue =>
          val sliceBit = interval(remainingBefore1D.end.successor, remainingAfter1D.start.predecessor)
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
            case None           => removeValidDataByKey(overlap.key)

        // corner: partial overlap in both dim 1 and 2 - remove and add 2 remaining (compress later)
        case (Remainder.Single(horizontalRemaining), Remainder.Single(verticalRemaining)) =>
          // can be above or below.
          val verticalCornerBit = excludeRemaining(overlap.interval.vertical, verticalRemaining)
          // can be to the left or right.
          val horizontalCornerBit = excludeRemaining(overlap.interval.horizontal, horizontalRemaining)
          removeValidDataByKey(overlap.key)
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

    newValueOption.foreach(compressInPlace(this))

  override def getIntersecting(interval: DiscreteInterval2D[R1, R2]): Iterable[ValidData2D[V, R1, R2]] =
    getAll.filter(_.interval intersects interval)

  override infix def intersects(interval: DiscreteInterval2D[R1, R2]): Boolean =
    getAll.exists(_.interval intersects interval)
