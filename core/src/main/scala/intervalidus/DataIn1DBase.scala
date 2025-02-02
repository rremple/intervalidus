package intervalidus

import intervalidus.Interval1D.between
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.collection.{Box, Coordinate}

import scala.collection.mutable

/**
  * Constructs data in one-dimensional intervals.
  */
trait DataIn1DBaseObject extends DataIn1DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of domain value used in the interval assigned to each value.
    * @param data
    *   valid data to start with.
    * @return
    *   [[DataIn1DBase]] structure with a single valid value.
    */
  def of[V, R: DomainValueLike](
    data: ValidData1D[V, R]
  )(using Experimental): DataIn1DBase[V, R]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of domain value used in the interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn1DBase]] structure with a single valid value.
    */
  def of[V, R: DomainValueLike](value: V)(using Experimental): DataIn1DBase[V, R]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R
    *   the type of domain value used in the interval assigned to each value.
    * @return
    *   [[DataIn1DBase]] structure with zero or more valid values.
    */
  def apply[V, R: DomainValueLike](initialData: Iterable[ValidData1D[V, R]])(using Experimental): DataIn1DBase[V, R]

trait DataIn1DConstructorParams:
  protected def constructorParams[V, R](
    initialData: Iterable[ValidData1D[V, R]]
  )(using experimental: Experimental, domainValue: DomainValueLike[R]): (
    mutable.TreeMap[Domain1D[R], ValidData1D[V, R]],
    mutable.TreeMap[Domain1D[R], ValidData1D[V, R]],
    MultiMapSorted[V, ValidData1D[V, R]],
    BoxTree[ValidData1D[V, R]]
  ) =
    val dataByStartAsc: mutable.TreeMap[Domain1D[R], ValidData1D[V, R]] =
      mutable.TreeMap.from(initialData.map(_.withKey))

    val dataByStartDesc: mutable.TreeMap[Domain1D[R], ValidData1D[V, R]] =
      experimental.control("noSearchTree")(
        experimentalResult = mutable.TreeMap.from(dataByStartAsc.iterator)(summon[Ordering[Domain1D[R]]].reverse),
        nonExperimentalResult = mutable.TreeMap()(summon[Ordering[Domain1D[R]]].reverse) // not used
      )

    val dataByValue: MultiMapSorted[V, ValidData1D[V, R]] =
      collection.mutable.MultiMapSorted.from(initialData.map(v => v.value -> v))

    val minPoint = Coordinate(domainValue.minValue.orderedHashValue)
    val maxPoint = Coordinate(domainValue.maxValue.orderedHashValue)
    val boundary = Box(minPoint, maxPoint)
    val dataInSearchTree: BoxTree[ValidData1D[V, R]] =
      experimental.control("noSearchTree")(
        experimentalResult = BoxTree[ValidData1D[V, R]](boundary), // not used
        nonExperimentalResult = BoxTree.from[ValidData1D[V, R]](boundary, initialData.map(_.asBoxedPayload))
      )

    (dataByStartAsc, dataByStartDesc, dataByValue, dataInSearchTree)

/**
  * Data that may have different values in different intervals. These intervals may represent when the data are valid in
  * time or over certain versions ranges or whatever. But we can capture the dependency between various values and
  * related intervals cohesively in this structure rather than in separate data structures using distributed (and
  * potentially inconsistent) logic.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R
  *   the type of domain value used in the interval assigned to each value.
  */
// Base for all 1D data, both mutable and immutable
trait DataIn1DBase[V, R: DomainValueLike](using experimental: Experimental)
  extends DimensionalBase[
    V,
    Domain1D[R],
    Interval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R],
    DataIn1DBase[V, R]
  ]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval1D.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  override protected def newValidData(value: V, interval: Interval1D[R]): ValidData1D[V, R] =
    interval -> value

  private def subIntervalsWith[B](that: DataIn1DBase[B, R]) = Interval1D.uniqueIntervals(
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

  override protected def compressInPlace(value: V): Unit =
    dataByValue
      .get(value)
      .foldLeft(None: Option[ValidData1D[V, R]]):
        case (Some(left), right) if left.interval isLeftAdjacentTo right.interval =>
          val newLeft = left.interval ∪ right.interval -> value
          updateValidData(newLeft)
          removeValidData(right)
          Some(newLeft)
        case (_, right) =>
          Some(right)
    ()

  override protected def recompressInPlace(): Unit =
    // no decompressing to do in 1D
    dataByValue.keySet.foreach(compressInPlace)

  // ---------- Implement methods from DimensionalBase ----------

  override def diffActionsFrom(old: DataIn1DBase[V, R]): Iterable[DiffAction1D[V, R]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction1D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction1D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction1D.Delete(oldData.interval.start))
        case _                                                    => None

  override def domain: Iterable[Interval1D[R]] = Interval1D.compress(getAll.map(_.interval))

  override def domainComplement: Iterable[Interval1D[R]] = Interval1D.complement(domain)

  /**
    * @inheritdoc
    *
    * Exclusions in one dimension can have three remainders: none (simple), single (partial), and split.
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  override protected def updateOrRemove(
    targetInterval: Interval1D[R],
    updateValue: V => Option[V]
  ): Unit = synchronized:
    import Interval1D.Remainder

    val intersecting = getIntersecting(targetInterval)
    val potentiallyAffectedValues = intersecting.map(_.value).toSet ++ intersecting.map(_.value).flatMap(updateValue)

    intersecting.foreach: overlap =>
      val newValueOption = updateValue(overlap.value)
      overlap.interval \ targetInterval match

        // overlap is subset, just update/remove entirely
        case Remainder.None =>
          newValueOption match
            case Some(newValue) => updateValidData(overlap.interval -> newValue)
            case None           => removeValidData(overlap)

        // no split: adjust on left
        case Remainder.Single(remaining) if remaining hasSameStartAs overlap.interval =>
          updateValidData(remaining -> overlap.value) // shortened on left
          newValueOption.foreach: newValue =>
            addValidData(overlap.interval.fromAfter(remaining.end) -> newValue)

        // no split: adjust on right
        case Remainder.Single(remaining) =>
          removeValidData(overlap) // remove and re-add to shorten
          addValidData(remaining -> overlap.value) // shortened on right
          newValueOption.foreach: newValue =>
            addValidData(overlap.interval.toBefore(remaining.start) -> newValue)

        // split: shorten left, add right remaining
        case Remainder.Split(leftRemaining, rightRemaining) =>
          // assert(leftRemaining hasSameStart overlap.interval)
          updateValidData(leftRemaining -> overlap.value)
          newValueOption.foreach: newValue =>
            addValidData(between(leftRemaining, rightRemaining) -> newValue)
          addValidData(rightRemaining -> overlap.value)

    potentiallyAffectedValues.foreach(compressInPlace)

  override protected def fillInPlace[B <: V](interval: Interval1D[R], value: B): Unit = synchronized:
    val intersectingIntervals = getIntersecting(interval).map(_.interval)
    Interval1D
      .uniqueIntervals(intersectingIntervals.toSeq :+ interval)
      .foreach: i =>
        if interval.intersects(i) && !this.intersects(i) then addValidData(i -> value)
    compressInPlace(value)
