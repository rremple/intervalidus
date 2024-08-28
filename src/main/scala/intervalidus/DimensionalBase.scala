package intervalidus

object DimensionalBase:
  /**
    * A discrete domain used to define intervals.
    */
  trait DomainLike

  /**
    * A interval over a contiguous set of ordered elements of a discrete domain.
    *
    * @tparam D
    *   the type of discrete domain used in the discrete interval (e.g., DiscreteDomain1D[Int]).
    */
  trait IntervalLike[D <: DomainLike: Ordering]:
    /**
      * When stored in a collection, this aspect of the interval can be used as the key. (E.g., the start of a 1D
      * interval).
      *
      * @return
      *   a domain-like key
      */
    def key: D

    /**
      * Tests if this interval contains a specific element of the domain.
      *
      * @param domainIndex
      *   domain element to test.
      * @return
      *   true only if the domain element is contained in this interval.
      */
    infix def contains(domainIndex: D): Boolean

    /**
      * Returns true only if there is no fixed start or end - spans the entire domain.
      */
    infix def isUnbounded: Boolean

  /**
    * A value that is valid in some discrete interval. This defines a partial function where all domain elements that
    * are part of the interval map to the specified value.
    *
    * @tparam V
    *   the type of the value managed as data (the codomain).
    * @tparam D
    *   the type of discrete domain used in the discrete interval assigned to each value (the domain).
    * @tparam I
    *   the type of discrete interval in which the value is valid.
    */
  trait DataLike[V, D <: DomainLike, I <: IntervalLike[D]] extends PartialFunction[D, V]:
    /**
      * The value valid in this interval
      */
    def value: V

    /**
      * The interval in which the value is valid
      */
    def interval: I

    /**
      * When stored in a collection, this aspect of the data can be used as the key. (E.g., the start of the
      * corresponding 1D interval).
      *
      * @return
      *   a domain-like key
      */
    def key: D = interval.key

    override def apply(domainIndex: D): V =
      if isDefinedAt(domainIndex) then value else throw new Exception(s"Not defined at $domainIndex")

    override def isDefinedAt(d: D): Boolean = interval contains d

import DimensionalBase.{DataLike, DomainLike, IntervalLike}

/**
  * Base for all dimensional data, both mutable and immutable, both 1D and 2D.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals. Must be `DomainLike` and have an `Ordering`.
  * @tparam I
  *   the interval type, based on the domain type. Must be `IntervalLike` based on D.
  * @tparam ValidData
  *   the valid data type. Must be `DataLike` based on V, D, and I.
  */
trait DimensionalBase[V, D <: DomainLike, I <: IntervalLike[D], ValidData <: DataLike[V, D, I]]
  extends PartialFunction[D, V]:

  protected def newValidData(value: V, interval: I): ValidData

  // For defining 1D and 2D toString methods - print a uniform grid representing the data.
  protected def toStringGrid[R1: DiscreteValue, R2: DiscreteValue](
    dataToString: ValidData => String,
    dataToInterval: ValidData => DiscreteInterval1D[R1],
    dataToSortBy: ValidData => DiscreteDomain1D[R2]
  ): String =

    val validData = getAll.toList
    val maxDataSize = validData.map(dataToString).map(_.length + 3).maxOption.getOrElse(3)
    val horizontalIntervals = DiscreteInterval1D.uniqueIntervals(validData.map(dataToInterval))
    val horizontalSpacer = "| " // 2 +
    val horizontalDots = " .. " // 4 = 6 + end space = 7
    val maxHorizontalIntervalsSize = horizontalIntervals
      .map(i => i.start.toString.length + i.end.toString.length + 7)
      .maxOption
      .getOrElse(7)
    val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

    def pad(chars: Int, p: String = " "): String = p * chars

    val (horizontalStringBuilder, horizontalStartPositionBuilder, horizontalEndPositionBuilder) =
      horizontalIntervals.zipWithIndex.foldLeft(
        (StringBuilder(), Map.newBuilder[DiscreteDomain1D[R1], Int], Map.newBuilder[DiscreteDomain1D[R1], Int])
      ):
        case ((stringBuilder, startPositionBuilder, endPositionBuilder), (interval, index)) =>
          val barString = s"$horizontalSpacer${interval.start}$horizontalDots${interval.end} "
          startPositionBuilder.addOne(interval.start, stringBuilder.size)
          stringBuilder.append(barString)
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
    * maintains interval start order.
    */
  protected def dataByStart: scala.collection.mutable.TreeMap[D, ValidData]

  /**
    * Remove, and possibly update, valid values on the target interval. If there are values valid on portions of the
    * interval, those values have their interval adjusted (e.g., shortened, shifted, split) accordingly. The logic of
    * remove and update are similar, and this method supports both.
    *
    * @param targetInterval
    *   the new value existing data in the interval should take on
    * @param newValueOption
    *   when defined, the value to be stored as part of an update
    */
  protected def updateOrRemove(targetInterval: I, newValueOption: Option[V]): Unit

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: DimensionalBase[V, D, I, ValidData]

  /**
    * Returns a value that is valid in the specified interval domain element. That is, where the specified domain
    * element is a member of some valid data interval. If no such valid data exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. Note that the domain element can be a specific data point or the
    *   special notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D): Option[V]

  /**
    * Returns all the intervals (compressed) in which there are valid values.
    */
  def domain: Iterable[I]

  /**
    * Returns all data that are valid on some or all of the provided interval.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: I): Iterable[ValidData]

  /**
    * Are there values that are valid on some or all of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: I): Boolean

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: DimensionalBase[V, D, I, ValidData]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: DimensionalBase[V, D, I, ValidData]

  // ---------- Implemented here based on the above ----------

  /**
    * Internal mutator to add, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData): Unit =
    // assert(!dataByStart.isDefinedAt(data.key))
    dataByStart.addOne(data.key -> data)

  /**
    * Internal mutator to update, where a value with v.key already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData): Unit =
    // assert(dataByStart.isDefinedAt(data.key))
    dataByStart.update(data.key, data)

  /**
    * Internal mutator to remove, where a value with key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D): Unit =
    val previous = dataByStart.remove(key)
    // assert(previous.isDefined)

  // from PartialFunction
  override def isDefinedAt(key: D): Boolean = getAt(key).isDefined

  // from PartialFunction
  override def apply(domainIndex: D): V = getAt(domainIndex).getOrElse(
    throw Exception(s"Not defined at $domainIndex")
  )

  /**
    * Returns a the value if a single, unbounded valid value. Otherwise throws an exception.
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
    * Returns a Some value if a single, unbounded valid value. Otherwise returns None.
    */
  def getOption: Option[V] = getAll.headOption.filter(_.interval.isUnbounded).map(_.value)

  /**
    * Returns true when there are no valid data in this structure, otherwise false.
    */
  def isEmpty: Boolean = dataByStart.isEmpty

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
    * Get all valid data.
    *
    * @return
    *   all valid data in interval order
    */
  def getAll: Iterable[ValidData] = dataByStart.values
