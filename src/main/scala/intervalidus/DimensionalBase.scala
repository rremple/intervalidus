package intervalidus

import intervalidus.collection.*
import intervalidus.collection.mutable.{MultiDictSorted, MutableBoxTreeLike}

import scala.math.Ordering.Implicits.infixOrderingOps

object DimensionalBase:
  /**
    * A discrete domain used to define intervals.
    * @tparam Self
    *   F-bounded self type.
    */
  trait DomainLike[+Self <: DomainLike[Self]]:

    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String

  /**
    * A interval over a contiguous set of ordered elements of a discrete domain.
    *
    * @tparam D
    *   the type of discrete domain used in the discrete interval (e.g., DiscreteDomain1D[Int]).
    * @tparam Self
    *   F-bounded self type.
    */
  trait IntervalLike[D <: DomainLike[D]: Ordering, Self <: IntervalLike[D, Self]]:
    /**
      * The "infimum", i.e., the lower/left/behind boundary of the interval (inclusive). When stored in a collection,
      * this aspect of the interval can be used as the key. (E.g., the start of a 1D interval, the lower/left corner of
      * a 2D interval).
      */
    def start: D

    /**
      * The "supremum", i.e., the upper/right/front boundary of the interval (inclusive). Must be greater than or equal
      * to the start.
      */
    def end: D

    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String

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
      * Returns individual discrete domain points in this interval.
      */
    def points: Iterable[D]

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
    * @tparam Self
    *   F-bounded self type.
    */
  trait DataLike[V, D <: DomainLike[D], I <: IntervalLike[D, I], Self <: DataLike[V, D, I, Self]]
    extends PartialFunction[D, V]:
    /**
      * The value valid in this interval
      */
    def value: V

    /**
      * The interval in which the value is valid
      */
    def interval: I

    override def toString: String = s"$interval -> $value"

    // by default, put the interval in parens to x function works
    protected def qualifiedInterval: String = s"(${interval.toCodeLikeString})"

    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String = s"$qualifiedInterval -> ${value match
        case _: String => s"\"$value\""
        case _         => value.toString
      }"

    /**
      * When stored in a collection, this aspect of the data can be used as the key. (E.g., the start of the
      * corresponding 1D interval).
      *
      * @return
      *   a domain-like key
      */
    def key: D = interval.start

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
  * @tparam Self
  *   F-bounded self type.
  */
trait DimensionalBase[
  V,
  D <: DomainLike[D]: Ordering,
  I <: IntervalLike[D, I],
  ValidData <: DataLike[V, D, I, ValidData],
  Self <: DimensionalBase[V, D, I, ValidData, Self]
](using
  experimental: Experimental
) extends PartialFunction[D, V]:

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
    * maintains interval key order.
    */
  protected def dataByStartAsc: scala.collection.mutable.TreeMap[D, ValidData]

  /**
    * Internal shadow data structure where all the interval-bounded data are also stored, but using the interval key in
    * reverse order (for much quicker range lookups -- validated in microbenchmarks).
    */
  protected def dataByStartDesc: scala.collection.mutable.TreeMap[D, ValidData]

  /**
    * Another internal shadow data structure where all the interval-bounded data are also stored, but using the value
    * itself as the key (for faster compression, which is done by value). The ValidData are stored in a sorted set, so
    * they are retrieved in key order, making compression operations repeatable.
    */
  protected def dataByValue: MultiDictSorted[V, ValidData]

  /*
   * Because the type parameters are a bit wonky, the search tree is instantiated in the subclass, and accessed here
   * using only the following methods
   */
  protected def dataInSearchTreeAdd(data: ValidData): Unit
  protected def dataInSearchTreeRemove(data: ValidData): Unit
  protected def dataInSearchTreeClear(): Unit
  protected def dataInSearchTreeAddAll(data: Iterable[ValidData]): Unit
  protected def dataInSearchTreeGet(interval: I): Iterable[ValidData]
  protected def dataInSearchTreeGetByDomain(domainIndex: D): Option[ValidData]
  protected def dataInSearchTreeIntersects(interval: I): Boolean

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
    * Internal method, to compress in place. Structure is parameterized to support both mutable and immutable
    * compression. (Immutable compression acts on a copy.) Assumes caller does synchronization (if needed). Assumes
    * underlying data are disjoint, so no need to address intersections.
    *
    * @param value
    *   value to be evaluate
    * @return
    *   this structure once compressed (not a copy)
    */
  protected def compressInPlace(value: V): Unit

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: Self

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
  def toMutable: Self

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: Self

  // ---------- Implemented here based on the above ----------

  /**
    * Internal mutator to add, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData): Unit =
    // assert(!dataByStartAsc.isDefinedAt(data.key))
    dataByStartAsc.addOne(data.key -> data)
    dataByValue.addOne(data.value -> data)
    experimental.control("noSearchTree")(
      experimental = {
        // assert(!dataByStartDesc.isDefinedAt(data.key))
        dataByStartDesc.addOne(data.key -> data)
      },
      nonExperimental = dataInSearchTreeAdd(data)
    )

  /**
    * Internal mutator to update, where a value with v.key already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData): Unit =
    // assert(dataByStartAsc.isDefinedAt(data.key))
    val oldData = dataByStartAsc(data.key)
    dataByValue.subtractOne(oldData.value -> oldData)
    dataByValue.addOne(data.value -> data)
    dataByStartAsc.update(data.key, data)
    experimental.control("noSearchTree")(
      experimental = {
        // assert(dataByStartDesc.isDefinedAt(data.key))
        dataByStartDesc.update(data.key, data)
      },
      nonExperimental = {
        dataInSearchTreeRemove(oldData)
        dataInSearchTreeAdd(data)
      }
    )

  /**
    * Internal mutator to remove, where a known value already exists.
    *
    * @param oldData
    *   valid data to remove.
    */
  protected def removeValidData(oldData: ValidData): Unit =
    val key = oldData.key
    dataByValue.subtractOne(oldData.value -> oldData)
    val previousAsc = dataByStartAsc.remove(key)
    // assert(previousDesc.isDefined)
    experimental.control("noSearchTree")(
      experimental = {
        val previousDesc = dataByStartDesc.remove(key)
        // assert(previousAsc.isDefined)
      },
      nonExperimental = dataInSearchTreeRemove(oldData)
    )

  /**
    * Internal mutator to remove, where a value with a known key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D): Unit =
    removeValidData(dataByStartAsc(key))

  protected def clearValidData(): Unit =
    dataByStartAsc.clear()
    dataByValue.clear()
    experimental.control("noSearchTree")(
      experimental = dataByStartDesc.clear(),
      nonExperimental = dataInSearchTreeClear()
    )

  protected def replaceValidData(data: Iterable[ValidData]): Unit =
    clearValidData()
    dataByStartAsc.addAll(data.map(d => d.key -> d))
    dataByValue.addAll(data.map(d => d.value -> d))
    experimental.control("noSearchTree")(
      experimental = dataByStartDesc.addAll(dataByStartAsc),
      nonExperimental = dataInSearchTreeAddAll(data)
    )

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
    * Returns a value that is valid in the specified interval domain element. That is, where the specified domain
    * element is a member of some valid data interval. If no such valid data exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. Note that the domain element can be a specific data point or the
    *   special notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D): Option[V] =
    experimental.control("noSearchTree")(
      experimental = dataByStartDesc // Using reverse-key order allows us nearly O(1) for hits
        .valuesIteratorFrom(domainIndex) // (starting at or before the index)
        .filter(_.interval.end >= domainIndex) // but misses are still slow - this slightly improves miss performance
        .collectFirst:
          case d if d.interval.contains(domainIndex) => d.value,
      nonExperimental = dataInSearchTreeGetByDomain(domainIndex).map(_.value)
    )

  /**
    * Returns true when there are no valid data in this structure, otherwise false.
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