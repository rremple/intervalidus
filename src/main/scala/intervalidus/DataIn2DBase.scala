package intervalidus

import scala.annotation.tailrec
import scala.collection.mutable

object DataIn2DBase:
  /**
    * A value that is valid in a two-dimensional discrete interval. Conceptually, this defines a partial function where
    * all domain elements that are part of the 2D interval map to the value.
    *
    * @tparam V
    *   the type of the value managed as data (the codomain).
    * @tparam R1
    *   the type of the horizontal discrete interval.
    * @tparam R2
    *   the type of the vertical discrete interval.
    * @param value
    *   value that is valid in this 2D interval.
    * @param interval
    *   the discrete interval in which the value is valid.
    */
  case class ValidData2D[V, R1: DiscreteValue, R2: DiscreteValue](
    value: V,
    interval: DiscreteInterval2D[R1, R2]
  ) extends DimensionalBase.DataLike[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2]]:

    override def toString: String = s"$interval -> $value"

  /**
    * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
    * information. Note that this is not an event-sourced data structure, and history of mutations are not maintained.
    *
    * @tparam V
    *   the type of the value managed as data (not used in Delete).
    * @tparam R1
    *   the type of discrete value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical interval assigned to each value.
    */
  enum DiffAction2D[V, R1, R2]:
    case Create(validData: ValidData2D[V, R1, R2])
    case Update(validData: ValidData2D[V, R1, R2])
    case Delete(key: DiscreteDomain2D[R1, R2])

import intervalidus.DataIn2DBase.{DiffAction2D, ValidData2D}
import DiscreteInterval1D.interval

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
  * especially important for managing mutation, which can be a bit complex in two dimensions. Note that visualizing two
  * dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt chart and there is a
  * simple Visualize tool provided (in the test package... though maybe this should be its own separate subproject).
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

  private def horizontalSubIntervalsWith[B](that: DataIn2DBase[B, R1, R2]) = DiscreteInterval1D.uniqueIntervals(
    this.getAll.map(_.interval.horizontal).toSet ++ that.getAll.map(_.interval.horizontal)
  )

  private def verticalSubIntervalsWith[B](that: DataIn2DBase[B, R1, R2]) = DiscreteInterval1D.uniqueIntervals(
    this.getAll.map(_.interval.vertical).toSet ++ that.getAll.map(_.interval.vertical)
  )

  protected def zipData[B](that: DataIn2DBase[B, R1, R2]): Iterable[ValidData2D[(V, B), R1, R2]] =
    for
      horizontalSubInterval <- horizontalSubIntervalsWith(that)
      verticalSubInterval <- verticalSubIntervalsWith(that)
      subInterval = horizontalSubInterval x verticalSubInterval
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

  protected def zipAllData[B](
    that: DataIn2DBase[B, R1, R2],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData2D[(V, B), R1, R2]] =
    horizontalSubIntervalsWith(that).flatMap: horizontalSubInterval =>
      verticalSubIntervalsWith(that).flatMap: verticalSubInterval =>
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

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object - use Visualize (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value.toString} ${v.interval.vertical.toString}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )

  /**
    * Internal method, to compress in place. Structure is parameterized to support both mutable and immutable
    * compression. (Immutable compression acts on a copy.) Assumes caller does synchronization (if needed). TODO:
    * address intersections.
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
     * Each mutation gives rise to other compression possibilities. And applying a compression action
     * can invalidate the remainder of the actions (e.g., three-in-a-row). Unlike in one dimension, there
     * is no safe order to fold over to avoid these issues. So, instead, we evaluate every entry with every
     * other entry, get the first compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(): Unit =
      val firstActions = data.getAll
        .filter(_.value == value)
        .flatMap: r =>
          data.getAll
            .filter(s => s.value == value && !(r.interval equiv s.interval))
            .collect:
              case s if (s.interval isLeftAdjacentTo r.interval) || (s.interval isLowerAdjacentTo r.interval) =>
                Seq(
                  () => data.removeValidDataByKey(r.key),
                  () => data.updateValidData(s.interval ∪ r.interval -> value)
                )
        .headOption
      firstActions match
        case None => ()
        case Some(actions) =>
          actions.foreach(_.apply())
          compressRecursively()

    compressRecursively()

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn2DBase[V, R1, R2]): Iterable[DiffAction2D[V, R1, R2]] =
    (dataByStart.keys.toSet ++ old.dataByStart.keys).toList.sorted.flatMap: key =>
      (old.dataByStart.get(key), dataByStart.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction2D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction2D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction2D.Delete(oldData.key))
        case _                                                    => None

  protected def getByHorizontalIndexData(
    horizontalIndex: DiscreteDomain1D[R1]
  ): Iterable[DataIn1DBase.ValidData1D[V, R2]] = getAll.collect:
    case ValidData2D(value, interval) if horizontalIndex ∈ interval.horizontal =>
      interval.vertical -> value

  protected def getByVerticalIndexData(
    verticalIndex: DiscreteDomain1D[R2]
  ): Iterable[DataIn1DBase.ValidData1D[V, R1]] = getAll.collect:
    case ValidData2D(value, interval) if verticalIndex ∈ interval.vertical =>
      interval.horizontal -> value

  // ---------- Implement methods from DimensionalBase ----------

  protected override val dataByStart: mutable.TreeMap[DiscreteDomain2D[R1, R2], ValidData2D[V, R1, R2]] =
    mutable.TreeMap.from(initialData.map(v => v.key -> v))
  require(DiscreteInterval2D.isDisjoint(getAll.map(_.interval)))

  /**
    * @inheritdoc
    *
    * More specifically, this gets a bit complicated because there are two dimensions. Exclusions in one dimension can
    * have three remainders: none (simple), single (partial), and split. But two dimensional exclusions have these same
    * three remainders in each dimension, so there are a total of 3 x 3 = 9 remainder cases. But there are symmetric
    * partners for 3 of them, so actually there are only 6 unique cases:
    *   1. simple = none + none (1 of 9)
    *   1. edge = none + single or single + none (2 of 9)
    *   1. slice = single + split or split + single (2 of 9)
    *   1. corner = single + single (1 of 9)
    *   1. bite = single + split or split + single (2 of 9)
    *   1. hole = split + split (1 of 9)
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
    getIntersecting(targetInterval).foreach: overlap =>
      overlap.interval \ targetInterval match

        // simple: total overlap in dim 1 and 2 (no remainders) - just update/remove the whole thing
        case (Remainder.None, Remainder.None) =>
          newValueOption match
            case Some(newValue) => updateValidData(overlap.interval -> newValue)
            case None           => removeValidDataByKey(overlap.key)

        // edge: total overlap in dim 1 with partial overlap in dim 2 - adjust in dim 2
        case (Remainder.None, Remainder.Single(remainingVertical)) =>
          if remainingVertical hasSameStartAs overlap.interval.vertical
          then // same start in dim 2: shorten
            updateValidData(overlap.interval.withVertical(remainingVertical) -> overlap.value)
            newValueOption.foreach: newValue =>
              addValidData(
                overlap.interval.withVerticalUpdate(_.startingAfter(remainingVertical.end)) -> newValue
              )
          else // different start in dim 2: remove and add remaining on right, new on left
            removeValidDataByKey(overlap.key)
            addValidData(overlap.interval.withVertical(remainingVertical) -> overlap.value)
            newValueOption.foreach: newValue =>
              addValidData(
                overlap.interval.withVerticalUpdate(_.endingBefore(remainingVertical.start)) -> newValue
              )

        // slice: total overlap in dim 1 with split in dim 2 - shorten bottom, add top remaining
        case (Remainder.None, Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          updateValidData(overlap.interval.withVertical(belowRemaining) -> overlap.value)
          addValidData(overlap.interval.withVertical(aboveRemaining) -> overlap.value)
          newValueOption.foreach: newValue =>
            val verticalSliceBit = interval(belowRemaining.end.successor, aboveRemaining.start.predecessor)
            addValidData(
              overlap.interval.withVertical(verticalSliceBit) -> newValue
            )

        // edge: partial overlap in dim 1 with total overlap in dim 2 - adjust in dim 1
        // [symmetric with (Remainder.None, Remainder.Single(remainingVertical)) above]
        case (Remainder.Single(remainingHorizontal), Remainder.None) =>
          if remainingHorizontal hasSameStartAs overlap.interval.horizontal
          then // same start in dim 1: shorten
            updateValidData(overlap.interval.withHorizontal(remainingHorizontal) -> overlap.value)
            newValueOption.foreach: newValue =>
              addValidData(
                overlap.interval.withHorizontalUpdate(_.startingAfter(remainingHorizontal.end)) -> newValue
              )
          else // different start in dim 1: remove and then add remaining to change the start (PK)
            removeValidDataByKey(overlap.key)
            addValidData(overlap.interval.withHorizontal(remainingHorizontal) -> overlap.value)
            newValueOption.foreach: newValue =>
              addValidData(
                overlap.interval.withHorizontalUpdate(_.endingBefore(remainingHorizontal.start)) -> newValue
              )

        // corner: partial overlap in both dim 1 and 2 - remove and add 2 remaining (compress later)
        case (Remainder.Single(remainingHorizontal), Remainder.Single(remainingVertical)) =>
          // can be above or below. Equivalent to (overlap.interval.vertical \ remainingVertical).head
          val verticalCornerBit =
            if overlap.interval.vertical.start == remainingVertical.start
            then // missing bit is above
              overlap.interval.vertical.startingAfter(remainingVertical.end)
            else // missing bit is below
              overlap.interval.vertical.endingBefore(remainingVertical.start)
          removeValidDataByKey(overlap.key)
          addValidData(overlap.interval.withVertical(remainingVertical) -> overlap.value)
          addValidData((remainingHorizontal x verticalCornerBit) -> overlap.value)
          newValueOption.foreach: newValue =>
            // can be to the left or right. Equivalent to (overlap.interval.horizontal \ remainingHorizontal).head
            val horizontalCornerBit =
              if overlap.interval.horizontal.start == remainingHorizontal.start
              then // missing bit is to the right
                overlap.interval.horizontal.startingAfter(remainingHorizontal.end)
              else // missing bit is to the left
                overlap.interval.horizontal.endingBefore(remainingHorizontal.start)
            addValidData((horizontalCornerBit x verticalCornerBit) -> newValue)

        // bite: partial overlap in dim 1 with split in dim 2 - shorten bottom and add 2 remaining
        case (Remainder.Single(remainingHorizontal), Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          // Equivalent to ((overlap.interval.vertical \ bottomRemaining).head \ topRemaining).head
          val verticalBiteBit = interval(belowRemaining.end.successor, aboveRemaining.start.predecessor)
          updateValidData(overlap.interval.withVertical(belowRemaining) -> overlap.value) // shorten to below bite
          addValidData((remainingHorizontal x verticalBiteBit) -> overlap.value) // through the bite
          addValidData(overlap.interval.withVertical(aboveRemaining) -> overlap.value) // above the bite
          newValueOption.foreach: newValue =>
            // can be to the left or right. Equivalent to (overlap.interval.horizontal \ remainingHorizontal).head
            val horizontalBiteBit =
              if overlap.interval.horizontal.start == remainingHorizontal.start
              then // missing bit is to the right
                overlap.interval.horizontal.startingAfter(remainingHorizontal.end)
              else // missing bit is to the left
                overlap.interval.horizontal.endingBefore(remainingHorizontal.start)
            addValidData((horizontalBiteBit x verticalBiteBit) -> newValue) // the bite is updated

        // slice: split in dim 1 with no remainder in dim 2 - shorten left, add right remaining
        // [symmetric with (Remainder.None, Remainder.Split(belowRemaining, aboveRemaining)) above]
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.None) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          updateValidData(overlap.interval.withHorizontal(leftRemaining) -> overlap.value)
          newValueOption.foreach: newValue =>
            val horizontalSliceBit = interval(leftRemaining.end.successor, rightRemaining.start.predecessor)
            addValidData(
              overlap.interval.withHorizontal(horizontalSliceBit) -> newValue
            )
          addValidData(overlap.interval.withHorizontal(rightRemaining) -> overlap.value)

        // bite: split in dim 1 with partial overlap in dim 2 - shorten left and add 2 remaining
        // [symmetric with Remainder.Single(remainingHorizontal), Remainder.Split(belowRemaining, aboveRemaining) above]
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.Single(remainingVertical)) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          // Equivalent to ((overlap.interval.horizontal \ leftRemaining).head \ rightRemaining).head
          val horizontalBiteBit = interval(leftRemaining.end.successor, rightRemaining.start.predecessor)
          updateValidData(overlap.interval.withHorizontal(leftRemaining) -> overlap.value) // shorten to left of bite
          addValidData((horizontalBiteBit x remainingVertical) -> overlap.value) // through the bite
          addValidData(overlap.interval.withHorizontal(rightRemaining) -> overlap.value) // right of the bite
          newValueOption.foreach: newValue =>
            // can be below or above. Equivalent to (overlap.interval.vertical \ remainingVertical).head
            val verticalBiteBit =
              if overlap.interval.vertical.start == remainingVertical.start
              then // missing bit is above
                overlap.interval.vertical.startingAfter(remainingVertical.end)
              else // missing bit is below
                overlap.interval.vertical.endingBefore(remainingVertical.start)
            addValidData((horizontalBiteBit x verticalBiteBit) -> newValue) // the bite is updated

        //  hole: split in dim 1 and 2 - shorten below, add two sides, and the bit above
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.Split(belowRemaining, aboveRemaining)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          // Equivalent to ((overlap.interval.vertical \ bottomRemaining).head \ topRemaining).head
          val verticalHoleBit = interval(belowRemaining.end.successor, aboveRemaining.start.predecessor)
          updateValidData(overlap.interval.withVertical(belowRemaining) -> overlap.value) // shorten below the hole
          addValidData((leftRemaining x verticalHoleBit) -> overlap.value) // left side of the hole
          addValidData((rightRemaining x verticalHoleBit) -> overlap.value) // right side of the hole
          addValidData(overlap.interval.withVertical(aboveRemaining) -> overlap.value) // above the hole
          newValueOption.foreach: newValue =>
            val horizontalHoleBit = interval(leftRemaining.end.successor, rightRemaining.start.predecessor)
            addValidData((horizontalHoleBit x verticalHoleBit) -> newValue) // the hole is updated

    newValueOption.foreach(compressInPlace(this))

  override def get: V = getAll.headOption match
    case Some(ValidData2D(value, interval)) if interval.isUnbounded => value
    case Some(_) => throw new NoSuchElementException("bounded DataIn2D.get")
    case None    => throw new NoSuchElementException("empty DataIn2D.get")

  override def getOption: Option[V] = getAll.headOption.filter(_.interval.isUnbounded).map(_.value)

  override def getAt(domainIndex: DiscreteDomain2D[R1, R2]): Option[V] =
    dataByStart
      .rangeTo(domainIndex) // can't be after this
      .values
      .find(domainIndex ∈ _.interval)
      .map(_.value)

  override def getIntersecting(interval: DiscreteInterval2D[R1, R2]): Iterable[ValidData2D[V, R1, R2]] =
    getAll.filter(_.interval intersects interval)

  override infix def intersects(interval: DiscreteInterval2D[R1, R2]): Boolean =
    getAll.exists(_.interval intersects interval)
