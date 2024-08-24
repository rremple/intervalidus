package intervalidus

import scala.collection.mutable
import DiscreteInterval1D.interval

trait DataIn1DBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DataIn1DBase]] structure with a single valid value.
    */
  def of[V, R: DiscreteValue](
    data: ValidData1D[V, R]
  ): DataIn1DBase[V, R]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn1DBase]] structure with a single valid value.
    */
  def of[V, R: DiscreteValue](value: V): DataIn1DBase[V, R]

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

  override protected def newValidData(value: V, interval: DiscreteInterval1D[R]): ValidData1D[V, R] =
    interval -> value

  private def subIntervalsWith[B](that: DataIn1DBase[B, R]) = DiscreteInterval1D.uniqueIntervals(
    this.getAll.map(_.interval).toSet ++ that.getAll.map(_.interval)
  )

  protected def zipData[B](that: DataIn1DBase[B, R]): Iterable[ValidData1D[(V, B), R]] =
    for
      subInterval <- subIntervalsWith(that)
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

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

  // from Object
  override def toString: String = toStringGrid(
    dataToString = _.value.toString,
    dataToInterval = _.interval,
    dataToSortBy = _.interval.end
  )

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
          val newLeft = left.interval ∪ right.interval -> value
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
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction1D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction1D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction1D.Delete(oldData.key))
        case _                                                    => None

  // ---------- Implement methods from DimensionalBase ----------

  protected override val dataByStart: mutable.TreeMap[DiscreteDomain1D[R], ValidData1D[V, R]] =
    mutable.TreeMap.from(initialData.map(v => v.key -> v))
  require(DiscreteInterval1D.isDisjoint(getAll.map(_.interval)))

  /**
    * @inheritdoc
    *
    * Both have to deal with exclusions, which can have three cases: simple, partial, and split.
    *
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
            case Some(newValue) => updateValidData(overlap.interval -> newValue)
            case None           => removeValidDataByKey(overlap.key)

        // no split: adjust on left
        case Remainder.Single(remaining) if remaining hasSameStartAs overlap.interval =>
          updateValidData(remaining -> overlap.value) // shortened on left
          newValueOption.foreach: newValue =>
            addValidData(overlap.interval.startingAfter(remaining.end) -> newValue)

        // no split: adjust on right
        case Remainder.Single(remaining) =>
          removeValidDataByKey(overlap.key) // remove and re-add to shorten
          addValidData(remaining -> overlap.value) // shortened on right
          newValueOption.foreach: newValue =>
            addValidData(overlap.interval.endingBefore(remaining.start) -> newValue)

        // split: shorten left, add right remaining
        case Remainder.Split(leftRemaining, rightRemaining) =>
          // assert(leftRemaining hasSameStart overlap.interval)
          updateValidData(leftRemaining -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(
              interval(leftRemaining.end.successor, rightRemaining.start.predecessor) -> newValue
            )
          addValidData(rightRemaining -> overlap.value)

    newValueOption.foreach(compressInPlace(this))

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
