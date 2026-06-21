package intervalidus

import intervalidus.Domain1D.*
import intervalidus.Interval1D.{intervalFrom, intervalTo}

import scala.annotation.nowarn
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A one-dimensional interval over a contiguous set of domain values in T. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @tparam T
  *   a domain value type for this interval's domain (e.g., Int, LocalDate) -- boundaries of the interval are defined in
  *   terms of `Domain1D[T]` given the type class `DomainValueLike[T]`.
  * @param start
  *   the "infimum", i.e., the left boundary of the interval
  * @param end
  *   the "supremum", i.e., the right boundary of the interval -- must be greater than or equal to the start
  */
case class Interval1D[T](
  start: Domain1D[T],
  end: Domain1D[T]
)(using domainValue: DomainValueLike[T])
  extends IntervalBase[Domain1D[T], Domain.In1D[T], Interval1D.Remainder[Interval1D[T]], Interval1D[T]]:

  require(Interval1D.validBounds(start, end), s"Interval $this invalid")

  /*
   * General IntervalLike behaviors/definitions
   */

  override infix def withValue[V](value: V): ValidData.In1D[V, T] = ValidData(value, this.tupled)

  override def points: Iterable[Domain1D[T]] = start.pointsTo(end)

  override def vertices: List[Domain1D[T]] = List(start, end)

  override infix def isLeftAdjacentTo(that: Interval1D[T]): Boolean = this.end isLeftAdjacentTo that.start

  override infix def intersectionWith(that: Interval1D[T]): Option[Interval1D[T]] =
    val (maxStart, minEnd) = (this.start maxStart that.start, this.end minEnd that.end)
    if Interval1D.validBounds(maxStart, minEnd) then Some(Interval1D(maxStart, minEnd)) else None

  override infix def joinedWith(that: Interval1D[T]): Interval1D[T] =
    Interval1D(this.start minStart that.start, this.end maxEnd that.end)

  override def after: Option[Interval1D[T]] = end.rightAdjacent match
    case Top           => None
    case endComplement => Some(intervalFrom(endComplement))

  override def before: Option[Interval1D[T]] = start.leftAdjacent match
    case Bottom          => None
    case startComplement => Some(intervalTo(startComplement))

  override def from(newStart: Domain1D[T]): Interval1D[T] = copy(start = newStart)

  override def fromAfter(adjacentDomain: Domain1D[T]): Interval1D[T] = from(adjacentDomain.rightAdjacent)

  override def fromBottom: Interval1D[T] = from(Bottom)

  override def to(newEnd: Domain1D[T]): Interval1D[T] = copy(end = newEnd)

  override def toBefore(adjacentDomain: Domain1D[T]): Interval1D[T] = to(adjacentDomain.leftAdjacent)

  override def toTop: Interval1D[T] = to(Top)

  override def atStart: Interval1D[T] = to(start)

  override def atEnd: Interval1D[T] = from(end)

  override infix def excluding(that: Interval1D[T]): Interval1D.Remainder[Interval1D[T]] =
    this ∩ that match
      case None => // no intersection, nothing to exclude
        Interval1D.Remainder.Single(this)
      case Some(commonBit) =>
        commonBit match
          case Interval1D(midStart, midEnd) if (midStart afterStart start) && (midEnd beforeEnd end) => // split
            Interval1D.Remainder.Split(toBefore(midStart), fromAfter(midEnd))
          case Interval1D(midStart, _) if midStart afterStart start => // later start, common end
            Interval1D.Remainder.Single(toBefore(midStart))
          case Interval1D(_, midEnd) if midEnd beforeEnd end => // common start, earlier end
            Interval1D.Remainder.Single(fromAfter(midEnd))
          case _ => // common start and end -- nothing remains
            Interval1D.Remainder.None

  override infix def separateUsing(that: Interval1D[T]): Iterable[Interval1D[T]] =
    this ∩ that match
      case None => // no intersection, nothing to separate
        Seq(this)
      case Some(commonBit) =>
        commonBit match
          case Interval1D(midStart, midEnd) if (midStart afterStart start) && (midEnd beforeEnd end) => // split
            Seq(toBefore(midStart), commonBit, fromAfter(midEnd))
          case Interval1D(midStart, _) if midStart afterStart start => // later start, common end
            Seq(toBefore(midStart), commonBit)
          case Interval1D(_, midEnd) if midEnd beforeEnd end => // common start, earlier end
            Seq(commonBit, fromAfter(midEnd))
          case _ => // common start and end -- nothing to separate
            Seq(commonBit)

  override infix def gapWith(that: Interval1D[T]): Option[Interval1D[T]] =
    if (this intersects that) || (this ~ that) then None
    else Some(Interval1D((this.end minEnd that.end).rightAdjacent, (this.start maxStart that.start).leftAdjacent))

  override infix def equiv(that: Interval1D[T]): Boolean = (start equiv that.start) && (end equiv that.end)

  override infix def contains(domain: Domain1D[T]): Boolean =
    domain.isClosedOrUnbounded && // strictly speaking, open points are not "contained" in anything
      (domain afterOrAtStart start) && (domain beforeOrAtEnd end)

  override infix def touches(that: Interval1D[T]): Boolean =
    this isAdjacentTo that

  override def isUnbounded: Boolean =
    start.isUnbounded && end.isUnbounded

  override def isBounded: Boolean =
    !start.isUnbounded && !end.isUnbounded

  override infix def sharesBoundaryWith(that: Interval1D[T]): Boolean =
    def tailToTip(ds: Domain1D[T], de: Domain1D[T]): Boolean = (ds, de) match
      case (Point(s), Point(e)) if s == e => true // one closed point in common
      case _                              => false
    (start equiv that.start) || (end equiv that.end) || tailToTip(end, that.start) || tailToTip(that.end, start)

  override infix def relationWith(that: Interval1D[T]): SpatialRelation =
    val hasSharedBoundary = sharesBoundaryWith(that)
    // Overlap: This is the "parent" for the six overlap relations:
    if this intersects that then
      // EQ (Equal): All boundaries match exactly.
      if this equiv that then SpatialRelation.EQ
      // TPP/NTPP (Inside, touching/not touching): a is subset of b and they share/don't share boundaries.
      else if this ⊆ that then if hasSharedBoundary then SpatialRelation.TPP else SpatialRelation.NTPP
      // TPPi/NTPPi (Inside, touching/not touching): b is subset of a and they share/don't share boundaries.
      else if that ⊆ this then if hasSharedBoundary then SpatialRelation.TPPi else SpatialRelation.NTPPi
      // PO (Partial Overlap): They share volume but neither is a subset of the other.
      else SpatialRelation.PO
    // EC (Externally Connected): no gaps, same as our isConnectedTo definition.
    else if this touches that then SpatialRelation.EC
    // DC (Disconnected): At least one dimension has a gap.
    else SpatialRelation.DC

  // Use mathematical interval notation -- default.
  override def toString: String = s"${start.leftBrace}$start${domainValue.bracePunctuation}$end${end.rightBrace}"

  override def toCodeLikeString: String =
    import Domain1D.codeLikeValue
    // Exhaustivity would require matching the seven invalid cases related to (Top, _) and (_, Bottom)
    ((start, end): @nowarn("msg=match may not be exhaustive")) match
      case (Bottom, Top)                  => "unbounded"
      case (Bottom, Point(e))             => s"intervalTo(${codeLikeValue(e)})"
      case (Bottom, OpenPoint(e))         => s"intervalToBefore(${codeLikeValue(e)})"
      case (Point(s), Top)                => s"intervalFrom(${codeLikeValue(s)})"
      case (Point(s), Point(e)) if s == e => s"intervalAt(${codeLikeValue(s)})"
      case (Point(s), Point(e))           => s"interval(${codeLikeValue(s)}, ${codeLikeValue(e)})"
      case (Point(s), OpenPoint(e))       => s"intervalFrom(${codeLikeValue(s)}).toBefore(${codeLikeValue(e)})"
      case (OpenPoint(s), Top)            => s"intervalFromAfter(${codeLikeValue(s)})"
      case (OpenPoint(s), Point(e))       => s"intervalFromAfter(${codeLikeValue(s)}).to(${codeLikeValue(e)})"
      case (OpenPoint(s), OpenPoint(e))   => s"intervalFromAfter(${codeLikeValue(s)}).toBefore(${codeLikeValue(e)})"

  /*
   * Behaviors/definitions specific to a fixed one-dimensional interval
   */

  /**
    * Cross this interval with that interval to arrive at a new two-dimensional interval.
    *
    * @param that
    *   a one-dimensional interval to be used in the vertical dimension
    * @tparam X
    *   domain value type for that interval
    * @return
    *   a new two-dimensional interval with this interval as the horizontal component and that interval as the vertical
    *   component.
    */
  infix def x[X: DomainValueLike](that: Interval1D[X]): Interval.In2D[T, X] =
    Interval((this.start, that.start), (this.end, that.end))

  /**
    * Returns this specialized one-dimensional interval as a general interval based on domain tuples.
    */
  def tupled: Interval.In1D[T] = Interval.in1D(this)

  /**
    * Used internally when formatting a single interval for the Data.toString grid.
    */
  def formatForGrid: String = domainValue match
    case dvl: ContinuousValue[T] => s"${start.leftBrace} $start${dvl.bracePunctuation}$end ${end.rightBrace} "
    case dvl: DiscreteValue[T]   => s"| $start ${dvl.bracePunctuation} $end "

/**
  * Common definitions for the one-dimensional intervals.
  *
  * @define domainValueType
  *   domain value type of these intervals
  */
object Interval1D:
  /**
    * Checks if using these start and end domains would form a valid one-dimensional interval. To be valid, one of the
    * following must be true:
    *   1. any kind of bounds (i.e., open, closed, or unbounded) where the start is strictly less than the end.
    *   1. closed and equal bounds (an interval of a single point).
    * Otherwise, the interval will be invalid: either the end is before start, or they're equal with one (or both) of
    * the bounds open.
    * @note
    *   This means that intervals of the form `(Top, Top)` and `(Bottom, Bottom)` are not allowed.
    */
  def validBounds[T: DomainValueLike](start: Domain1D[T], end: Domain1D[T]): Boolean = (start, end) match
    case _ if (start beforeEnd end) && (end afterStart start) => true
    case (Point(startValue), Point(endValue))                 => startValue == endValue
    case _                                                    => false

  // used in return type of excluding
  enum Remainder[+G]:
    case None
    case Single(g: G)
    case Split(left: G, right: G)

  /**
    * Used internally when formatting a group of intervals for the Data.toString grid.
    */
  def preprocessForGrid[T: DomainValueLike](
    intervals: IterableOnce[Interval1D[T]]
  ): Iterable[(String, String, String)] =
    uniqueIntervals(intervals).map: interval =>
      val headStartString = interval.start.toString
      val headEndString = interval.end.toString
      val intervalString = interval.formatForGrid
      (headStartString, headEndString, intervalString)

  /*
   * Helpers, to avoid using top and bottom literals everywhere when constructing intervals!
   */

  /**
    * Returns an interval from the input value that is unbounded on the right.
    */
  def intervalFrom[T: DomainValueLike](s: Domain1D[T]): Interval1D[T] = apply(s, Top)

  /**
    * Returns an interval from after the input value that is unbounded on the right.
    */
  def intervalFromAfter[T: DomainValueLike](s: Domain1D[T]): Interval1D[T] = apply(s.rightAdjacent, Top)

  /**
    * Returns an interval to the input value that is unbounded on the left.
    */
  def intervalTo[T: DomainValueLike](e: Domain1D[T]): Interval1D[T] = apply(Bottom, e)

  /**
    * Returns an interval to before the input value that is unbounded on the left.
    */
  def intervalToBefore[T: DomainValueLike](e: Domain1D[T]): Interval1D[T] = apply(Bottom, e.leftAdjacent)

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[T: DomainValueLike](s: Domain1D[T]): Interval1D[T] =
    val closestPoint = s.closeIfOpen
    interval(closestPoint, closestPoint)

  /**
    * Returns an interval that starts and ends at different values.
    */
  def interval[T: DomainValueLike](s: Domain1D[T], e: Domain1D[T]): Interval1D[T] = apply(s, e)

  /**
    * Returns the interval between `before` and `after`. This is equivalent to `before.gapWith(after).get`, but without
    * intersection and adjacency checks. Only use this function if you know there is a gap between `before` and `after`,
    * e.g., they are exclusion remainders.
    *
    * @param before
    *   interval on the left side
    * @param after
    *   interval on the right side
    * @tparam T
    *   $domainValueType
    * @return
    *   the interval made from the gap between the two inputs
    */
  def between[T: DomainValueLike](
    before: Interval1D[T],
    after: Interval1D[T]
  ): Interval1D[T] = interval(before.end.rightAdjacent, after.start.leftAdjacent)

  /**
    * Returns an interval unbounded on both the left and right.
    */
  def unbounded[T: DomainValueLike]: Interval1D[T] = Interval1D(Bottom, Top)

  /*
   * These methods operate on collections of intervals.
   */

  /**
    * Compresses a collection of intervals by joining all adjacent and intersecting intervals.
    *
    * Performance: O(|intervals|)
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   $domainValueType
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T: DomainValueLike](
    intervals: Iterable[Interval1D[T]]
  ): Iterable[Interval1D[T]] = intervals.foldRight(List.empty[Interval1D[T]]):
    case (left, right :: tail) if (left ~> right) || (left intersects right) => (left ∪ right) :: tail
    case (left, right :: tail)                                               => left :: right :: tail
    case (left, Nil)                                                         => List(left)

  /**
    * Checks if the collection of intervals is compressible. That is, are there any intervals that are adjacent to, or
    * intersecting with, their neighbors. If true, calling [[compress]] on the collection results in a smaller
    * collection covering the same domain.
    *
    * Performance: O(|intervals|)
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   $domainValueType
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Boolean =
    if intervals.isEmpty then false
    else
      intervals.iterator
        .zip(intervals.iterator.drop(1))
        .exists((left, right) => (left ~> right) || (left intersects right))

  /**
    * Checks if the collection of intervals is disjoint. That is, all neighboring intervals do not intersect. See
    * [[https://en.wikipedia.org/wiki/Disjoint_sets]].
    *
    * Performance: O(|intervals|)
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   $domainValueType
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Boolean =
    intervals.isEmpty || intervals.iterator
      .zip(intervals.iterator.drop(1))
      .forall((left, right) => !(left intersects right))

  /**
    * Finds all intervals, including all overlaps and gaps between intervals, as intervals. Inputs may be overlapping.
    * The result is disjoint and covers the span of the input intervals.
    *
    * @note
    *   Continuous domains are well-behaved at the boundaries of their finite ranges, but discrete domains are not. If a
    *   discrete interval starts at the domain minValue, and it is combined with an interval without a lower bound
    *   (starting at -∞), the result will be truncated to minValue due to finite range boundary collapse. For example,
    *   using uniqueIntervals to combine [minValue..0] with (-∞..-10] yields only two intervals: [minValue..-10] and
    *   [-9..0], so the extent from -∞ is completely lost. Similarly, if an interval ends at the domain maxValue, and it
    *   is combined with another interval overlapping it without an upper bound (ending at +∞), the result will be
    *   coalesced with maxValue and the break at maxValue will be lost. For example, using uniqueIntervals to combine
    *   [0..Int.MaxValue] with [10..+∞) yields only two intervals: [0..9] and [10..+∞), so the break at maxValue is
    *   lost.
    *
    * Performance: O(|intervals|) plus sorting/distinct time (O(n*log(n))?).
    *
    * @param intervals
    *   collection of intervals
    * @tparam T
    *   $domainValueType
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T: DomainValueLike](intervals: IterableOnce[Interval1D[T]]): Iterable[Interval1D[T]] =
    val intervalsIterator = intervals.iterator
    if intervalsIterator.isEmpty then Iterable.empty
    else
      // using a tree set is faster than some other sequence that has to be sorted later
      val starts = mutable.TreeSet.newBuilder[Domain1D[T]]
      val resultBuilder = Iterable.newBuilder[Interval1D[T]]

      intervalsIterator.foreach: i =>
        starts += i.start
        starts += i.end.rightAdjacent

      val startResults = starts.result()
      startResults.iterator
        .zip(startResults.iterator.drop(1))
        .foreach: (s, s2) =>
          val e = s2.leftAdjacent
          if Interval1D.validBounds(s, e) then resultBuilder += Interval1D(s, e)
      resultBuilder.result()

  /**
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps). See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]]. Result invariants:
    *   - `isDisjoint(intervals ++ complement(intervals)) == true`
    *   - `compress(intervals ++ complement(intervals)).toSeq == Seq(unbounded)`
    *
    * Performance: O(|intervals|)
    *
    * @param intervals
    *   a collection of intervals -- must be disjoint and ordered by start.
    * @tparam T
    *   $domainValueType
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def complement[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Iterable[Interval1D[T]] =
    val (lastEnd, result) = intervals.foldLeft((Bottom: Domain1D[T], Vector.empty[Interval1D[T]])):
      case ((priorEnd, acc), next) =>
        val nextAcc =
          if next.start.leftAdjacent == priorEnd then acc
          else acc.appended(interval(priorEnd.rightAdjacent, next.start.leftAdjacent))
        (next.end, nextAcc)
    if lastEnd == Top then result else result.appended(apply(lastEnd.rightAdjacent, Top))

  /**
    * Intervals are ordered by start
    */
  given [T](using domainOrder: Ordering[Domain1D[T]]): Ordering[Interval1D[T]] with
    override def compare(x: Interval1D[T], y: Interval1D[T]): Int = domainOrder.compare(x.start, y.start)

  /**
    * So a fixed one-dimensional interval can be used when the general notion of a multidimensional interval is needed.
    */
  given [T: DomainValueLike]: Conversion[Interval1D[T], Interval.In1D[T]] = _.tupled
