package intervalidus

import scala.collection.mutable

object DataIn1DBase:
  /**
    * A value that is valid in a one-dimensional discrete interval. Conceptually, this defines a partial function where
    * all domain elements that are part of the interval map to the value.
    *
    * @tparam V
    *   the type of the value managed as data (the codomain).
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value (the domain).
    * @param value
    *   value that is valid in this interval.
    * @param interval
    *   the discrete interval in which the value is valid.
    */
  case class ValidData1D[V, R: DiscreteValue](
    value: V,
    interval: DiscreteInterval1D[R]
  ) extends DimensionalBase.DataLike[V, DiscreteDomain1D[R], DiscreteInterval1D[R]]:
    override def toString: String = s"ValidData1D($value, $interval)"

  /**
    * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
    * information. Note that this is not an event-sourced data structure, and history of mutations are not maintained.
    *
    * @tparam V
    *   the type of the value managed as data (not used in Delete).
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value.
    */
  enum DiffAction1D[V, R]:
    case Create(validData: ValidData1D[V, R])
    case Update(validData: ValidData1D[V, R])
    case Delete(key: DiscreteDomain1D[R])

import intervalidus.DataIn1DBase.{DiffAction1D, ValidData1D}
import DiscreteInterval1D.interval

/**
  * Data that may have different values in different intervals. These intervals may represent when the data are valid in
  * time or over certain versions ranges or whatever. But we can capture the dependency between various values and
  * related intervals cohesively in this structure rather than in separate data structures using distributed (and
  * potentially inconsistent) logic.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of discrete domain used in the interval assigned to each value.
  * @param initialData
  *   (optional) a collection of valid data to start with -- intervals must be disjoint.
  */
// Base for all 1D data, both mutable and immutable
trait DataIn1DBase[V, R: DiscreteValue](
  initialData: Iterable[ValidData1D[V, R]]
) extends DimensionalBase[V, DiscreteDomain1D[R], DiscreteInterval1D[R], ValidData1D[V, R]]:

  private def subIntervalsWith[B](that: DataIn1DBase[B, R]) = DiscreteInterval1D.uniqueIntervals(
    this.getAll.map(_.interval).toSet ++ that.getAll.map(_.interval)
  )

  protected def zipData[B](that: DataIn1DBase[B, R]): Iterable[ValidData1D[(V, B), R]] =
    for
      subInterval <- subIntervalsWith(that)
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield ValidData1D((v, b), subInterval)

  protected def zipAllData[B](
    that: DataIn1DBase[B, R],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData1D[(V, B), R]] = subIntervalsWith(that).flatMap: subInterval =>
    val vOption = getIntersecting(subInterval).headOption.map(_.value)
    val bOption = that.getIntersecting(subInterval).headOption.map(_.value)
    val valuePair = (vOption, bOption) match
      case (None, None)       => None
      case (Some(v), Some(b)) => Some((v, b))
      case (Some(v), None)    => Some((v, thatElem))
      case (None, Some(b))    => Some((thisElem, b))
    valuePair.map(ValidData1D(_, subInterval))

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
  def zip[B](that: DataIn1DBase[B, R]): DataIn1DBase[(V, B), R]

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
  def zipAll[B](that: DataIn1DBase[B, R], thisElem: V, thatElem: B): DataIn1DBase[(V, B), R]

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object - print a uniform grid representing the data.
  override def toString: String =
    def dataToString(v: ValidData1D[V, R]): String = v.value.toString

    val validData = getAll.toList
    val maxDataSize = validData.map(dataToString).map(_.length + 3).maxOption.getOrElse(3)
    val horizontalIntervals = DiscreteInterval1D.uniqueIntervals(validData.map(_.interval))
    val horizontalSpacer = "| " // 2 +
    val horizontalDots = " .. " // 4 = 6 + end space = 7
    val maxHorizontalIntervalsSize = horizontalIntervals
      .map(i => i.start.toString.length + i.end.toString.length + 7)
      .maxOption
      .getOrElse(7)
    val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

    def pad(chars: Int, p: String = " "): String = p * chars

    val (horizontalStringBuilder, horizontalPositionBuilder) =
      horizontalIntervals.zipWithIndex.foldLeft((StringBuilder(), Map.newBuilder[DiscreteDomain1D[R], Int])):
        case ((stringBuilder, positionBuilder), (interval, index)) =>
          val barString = s"$horizontalSpacer${interval.start}$horizontalDots${interval.end} "
          positionBuilder.addOne(interval.start, stringBuilder.size)
          stringBuilder.append(barString)
          val padTo = cellSize * (index + 1)
          if stringBuilder.size < padTo then stringBuilder.append(pad(padTo - stringBuilder.size))
          (stringBuilder, positionBuilder)

    val horizontalPosition = horizontalPositionBuilder.result()
    horizontalStringBuilder.append("|\n")

    validData
      .sortBy(_.interval.end)
      .foreach: v =>
        val valueString = dataToString(v)
        val leftPosition = horizontalPosition(v.interval.start)
        val valuePadding = cellSize - valueString.length - 2
        horizontalStringBuilder.append(
          s"${pad(leftPosition)}| $valueString${pad(valuePadding)}|\n"
        )

    horizontalStringBuilder.result()

  /**
    * Internal method, to compress in place. Structure is parameterized to support both mutable and immutable
    * compression. (Immutable compression acts on a copy.) Assumes caller does synchronization (if needed).
    *
    * @param data
    *   structure that should be compressed
    * @param value
    *   value to be evaluate
    * @return
    *   this structure once compressed (not a copy)
    * @note
    *   This method doesn't act on this, but it is defined in the class rather than in the companion object to allow it
    *   to access the protected mutator methods.
    */
  protected def compressInPlace(data: DataIn1DBase[V, R])(value: V): Unit =
    data.getAll
      .filter(_.value == value)
      .foldLeft(None: Option[ValidData1D[V, R]]):
        case (Some(left), right) if left.interval isLeftAdjacentTo right.interval =>
          val newLeft = ValidData1D(value, left.interval ∪ right.interval)
          data.updateValidData(newLeft)
          data.removeValidDataByKey(right.key)
          Some(newLeft)
        case (_, right) =>
          Some(right)
    ()

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn1DBase[V, R]): Iterable[DiffAction1D[V, R]] =
    (dataByStart.keys.toSet ++ old.dataByStart.keys).toList.sorted.flatMap: key =>
      (old.dataByStart.get(key), dataByStart.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData =>
          Some(DiffAction1D.Update(newData))
        case (None, Some(newData)) => Some(DiffAction1D.Create(newData))
        case (Some(oldData), None) => Some(DiffAction1D.Delete(oldData.key))
        case _                     => None

  // ---------- Implement methods from DimensionalBase ----------

  protected override val dataByStart: mutable.TreeMap[DiscreteDomain1D[R], ValidData1D[V, R]] =
    mutable.TreeMap.from(initialData.map(v => v.key -> v))
  require(DiscreteInterval1D.isDisjoint(getAll.map(_.interval)))

  /**
    * @inheritdoc
    *
    * Both have to deal with exclusions, which can have three cases: simple, partial, and split.
    * @param targetInterval
    *   the new value existing data in the interval should take on
    * @param newValueOption
    *   when defined, the value to be stored as part of an update
    */
  protected override def updateOrRemove(
    targetInterval: DiscreteInterval1D[R],
    newValueOption: Option[V]
  ): Unit = synchronized:
    import DiscreteInterval1D.Remainder
    getIntersecting(targetInterval).foreach: overlap =>
      overlap.interval \ targetInterval match

        // overlap is subset, just update/remove entirely
        case Remainder.None =>
          newValueOption match
            case Some(newValue) => updateValidData(ValidData1D(newValue, overlap.interval))
            case None           => removeValidDataByKey(overlap.key)

        // no split: adjust on left
        case Remainder.Single(remaining) if remaining hasSameStartAs overlap.interval =>
          updateValidData(ValidData1D(overlap.value, remaining)) // shortened on left
          newValueOption.foreach: newValue =>
            addValidData(ValidData1D(newValue, overlap.interval.startingAfter(remaining.end)))

        // no split: adjust on right
        case Remainder.Single(remaining) =>
          removeValidDataByKey(overlap.key) // remove and re-add to shorten
          addValidData(ValidData1D(overlap.value, remaining)) // shortened on right
          newValueOption.foreach: newValue =>
            addValidData(ValidData1D(newValue, overlap.interval.endingBefore(remaining.start)))

        // split: shorten left, add right remaining
        case Remainder.Split(leftRemaining, rightRemaining) =>
          // assert(leftRemaining hasSameStart overlap.interval)
          updateValidData(ValidData1D(overlap.value, leftRemaining))
          newValueOption.foreach: newValue =>
            addValidData(
              ValidData1D(newValue, interval(leftRemaining.end.successor, rightRemaining.start.predecessor))
            )
          addValidData(ValidData1D(overlap.value, rightRemaining))

    newValueOption.foreach(compressInPlace(this))

  override def get: V = getAll.headOption match
    case Some(ValidData1D(value, interval)) if interval.isUnbounded => value
    case Some(_) => throw new NoSuchElementException("bounded DataIn1D.get")
    case None    => throw new NoSuchElementException("empty DataIn1D.get")

  override def getOption: Option[V] = getAll.headOption.filter(_.interval.isUnbounded).map(_.value)

  override def getAt(domainIndex: DiscreteDomain1D[R]): Option[V] =
    dataByStart
      .rangeTo(domainIndex) // can't be after r
      .lastOption // can't be before the last one
      .collect:
        case (_, data) if domainIndex ∈ data.interval => data.value

  override def getIntersecting(interval: DiscreteInterval1D[R]): Iterable[ValidData1D[V, R]] =
    getAll.filter(_.interval intersects interval)

  override infix def intersects(interval: DiscreteInterval1D[R]): Boolean =
    getAll.exists(_.interval intersects interval)

  override def domain: Iterable[DiscreteInterval1D[R]] = DiscreteInterval1D.compress(getAll.map(_.interval))
