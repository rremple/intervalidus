package intervalidus

import intervalidus.Domain1D.{Bottom, OpenPoint, Point, Top}
import intervalidus.Interval1D.{interval, intervalFrom, intervalTo}

import java.time.{LocalDate, LocalDateTime}
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A one-dimensional interval over a contiguous set of domain values in T. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @tparam T
  *   a domain value type for this interval's domain (e.g., Int, LocalDate) -- boundaries of the interval are defined in
  *   terms of `Domain1D[T]` given the type class `DomainValueLike[T]`.
  * @param start
  *   the "infimum", i.e., the left (or below or back, depending on context) boundary of the interval
  * @param end
  *   the "supremum", i.e., the right (or above or front, depending on context) boundary of the interval -- must be
  *   greater than or equal to the start
  */
case class Interval1D[T](
  start: Domain1D[T],
  end: Domain1D[T]
)(using domainValue: DomainValueLike[T])
  extends IntervalLike[Domain1D[T], Interval1D[T]]:

  /**
    * Either start is before end (by both start and end ordering), or, when equal, both bounds must be closed (i.e.,
    * interval at a single point), both Top, or both Bottom.
    */
  private def validIntervalBounds(
    s: Domain1D[T],
    e: Domain1D[T]
  ): Boolean = (s, e) match
    case _ if (s beforeEnd e) && (e afterStart s) => true // strictly less than - excludes when values are equal
    case (Point(cs), Point(ce)) if cs == ce       => true // interval at a point is valid when bounds are closed,
    case (Top, Top) | (Bottom, Bottom) => true // or the special cases where it is an interval at top or bottom
    case _                             => false // otherwise invalid: e is before s, or they're equal with open bound(s)

  require(validIntervalBounds(start, end), s"Interval $this invalid")

  import Interval1D.Remainder

  override type ExclusionRemainder = Remainder[Interval1D[T]]

  override infix def withValue[V](value: V): ValidData1D[V, T] = ValidData1D(value, this)

  override infix def contains(domainElement: Domain1D[T]): Boolean = domainElement match
    case OpenPoint(_) => false // strictly speaking, open points are not "contained" in anything
    case d            => (d afterOrAtStart start) && (d beforeOrAtEnd end)

  override def points: Iterable[Domain1D[T]] = domainValue match
    case _: ContinuousValue[T] => Iterable.empty // undefined for continuous
    case discrete: DiscreteValue[T] =>
      def nearest(d: Domain1D[T]): Domain1D[T] = d match
        case Bottom => Point(discrete.minValue)
        case Top    => Point(discrete.maxValue)
        case _      => d

      Iterable.unfold(Some(interval(nearest(start), nearest(end))): Option[Interval1D[T]]):
        case None => None
        case Some(prevRemaining) =>
          val nextRemaining =
            if prevRemaining.start equiv prevRemaining.end then None
            else Some(prevRemaining.from(prevRemaining.start.rightAdjacent))
          Some(prevRemaining.start, nextRemaining)

  override infix def isAdjacentTo(that: Interval1D[T]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that)

  override infix def intersects(that: Interval1D[T]): Boolean =
    intersectionWith(that).isDefined

  override infix def intersectionWith(that: Interval1D[T]): Option[Interval1D[T]] =
    val (maxStart, minEnd) = (this.start maxStart that.start, this.end minEnd that.end)
    if validIntervalBounds(maxStart, minEnd) then Some(Interval1D(maxStart, minEnd)) else None

  override infix def joinedWith(that: Interval1D[T]): Interval1D[T] =
    Interval1D(this.start minStart that.start, this.end maxEnd that.end)

  override infix def contains(that: Interval1D[T]): Boolean =
    intersectionWith(that).contains(that)

  override def after: Option[Interval1D[T]] = end.rightAdjacent match
    case Top           => None
    case endComplement => Some(intervalFrom(endComplement))

  override def before: Option[Interval1D[T]] = start.leftAdjacent match
    case Bottom          => None
    case startComplement => Some(intervalTo(startComplement))

  // Use mathematical interval notation -- default.
  override def toString: String =
    s"${start.leftBrace}$start${domainValue.bracePunctuation}$end${end.rightBrace}"

  // Use method-like interval notation -- useful when constructing tests.
  override def toCodeLikeString: String =
    def valueCode(value: T): String = value match
      case d: LocalDate => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
      case d: LocalDateTime =>
        s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})" +
          s".atTime(${d.getHour},${d.getMinute},${d.getSecond},${d.getNano})"
      case _ => value.toString

    def boundCode(bound: Domain1D[T]): String = bound match
      case Bottom       => s"Bottom"
      case Top          => s"Top"
      case OpenPoint(s) => s"open(${valueCode(s)})"
      case Point(s)     => s"${valueCode(s)}"

    (start, end) match
      case (Bottom, Top) => "unbounded"
      case (Bottom, endPoint) =>
        endPoint match
          case OpenPoint(s) => s"intervalToBefore(${valueCode(s)})"
          case _            => s"intervalTo(${boundCode(endPoint)})"
      case (startPoint, Top) =>
        startPoint match
          case OpenPoint(s) => s"intervalFromAfter(${valueCode(s)})"
          case _            => s"intervalFrom(${boundCode(startPoint)})"
      case (Point(s), Point(e)) if s == e => s"intervalAt(${valueCode(s)})"
      case (sb, eb)                       => s"interval(${boundCode(sb)}, ${boundCode(eb)})"

  override def from(newStart: Domain1D[T]): Interval1D[T] = copy(start = newStart)

  override def to(newEnd: Domain1D[T]): Interval1D[T] = copy(end = newEnd)

  override def fromBottom: Interval1D[T] = from(Bottom)

  override def toTop: Interval1D[T] = to(Top)

  override infix def isLeftAdjacentTo(that: Interval1D[T]): Boolean = this.end isLeftAdjacentTo that.start

  /**
    * Excludes that interval from this one. There are three possible outcomes:
    *   1. this interval is a subset of that one, so once it is excluded, nothing remains. Remainder.None is returned.
    *   1. that either lies outside of this or has a simple edge intersection (only contains the start or the end, but
    *      not both), and a single interval remains. Remainder.Single is returned.
    *   1. that interval is a proper subset of this one, containing neither the start nor the end of this, so this
    *      interval gets split, and two intervals remain. Remainder.Split is returned
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder after exclusion.
    */
  override infix def excluding(that: Interval1D[T]): ExclusionRemainder =
    this intersectionWith that match
      case None => // no intersection, nothing to exclude
        Remainder.Single(this)
      case Some(commonBit) =>
        commonBit match
          case Interval1D(midStart, midEnd) if (midStart afterStart start) && (midEnd beforeEnd end) => // split
            Remainder.Split(toBefore(midStart), fromAfter(midEnd))
          case Interval1D(midStart, _) if midStart afterStart start => // later start, common end
            Remainder.Single(toBefore(midStart))
          case Interval1D(_, midEnd) if midEnd beforeEnd end => // common start, earlier end
            Remainder.Single(fromAfter(midEnd))
          case _ => // common start and end -- nothing remains
            Remainder.None

  /**
    * Similar to [[excluding]], separates the current interval into the intersection with that interval along with the
    * remainder intervals covering this, i.e., the union of all the returned intervals will match this. The interval
    * collection returned will be ordered by interval starts and will be disjoint. There are three possible outcomes:
    *   1. just one interval (this interval) is returned if this is a subset of that or this doesn't intersect that.
    *   1. two intervals (the intersection with that along with the portion of this lying outside of that) are returned
    *      if this has a simple edge intersection.
    *   1. three intervals (the intersection with that along with both portions of this that are before and after that)
    *      are returned if that is a proper subset of this, containing neither the start nor the end of this.
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder after exclusion.
    */
  infix def separateUsing(that: Interval1D[T]): Iterable[Interval1D[T]] =
    this intersectionWith that match
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
    if this intersects that then None
    else if this isAdjacentTo that then None
    else Some(Interval1D((this.end minEnd that.end).rightAdjacent, (this.start maxStart that.start).leftAdjacent))

  /**
    * Cross this interval with that interval to arrive at a new two-dimensional interval.
    * @param that
    *   a one-dimensional interval to be used in the vertical dimension
    * @tparam T2
    *   domain value type for that interval
    * @return
    *   a new two-dimensional interval with this interval as the horizontal component and that interval as the vertical
    *   component.
    */
  infix def x[T2: DomainValueLike](that: Interval1D[T2]): Interval2D[T, T2] =
    Interval2D(this, that)

  // equivalent symbolic method names

  override infix def ->[V](value: V): ValidData1D[V, T] = withValue(value)

/**
  * Companion for the one-dimensional interval used in defining and operating on valid data.
  */
object Interval1D:
  // used in return type of excluding
  enum Remainder[+G]:
    case None
    case Single(g: G)
    case Split(left: G, right: G)

  /*
   * Helpers, to avoid using "Top" and "Bottom" literals everywhere when constructing intervals!
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
  def intervalAt[T: DomainValueLike](s: Domain1D[T]): Interval1D[T] = s match
    case op: OpenPoint[T] @unchecked => apply(op.leftAdjacent, op.rightAdjacent)
    case _                           => apply(s, s)

  /**
    * Returns an interval that starts and ends at the different values.
    */
  def interval[T: DomainValueLike](s: Domain1D[T], e: Domain1D[T]): Interval1D[T] = apply(s, e)

  /**
    * Returns the interval between `before` and `after`. This is equivalent to `before.gapWith(after).get`, but without
    * intersection and adjacency checks. Only use this function if you know there is a gap between `before` and `after`,
    * e.g., they are exclusion remainders.
    *
    * @param before
    *   interval on the left/bottom/back side
    * @param after
    *   interval on the right/top/front side
    * @tparam T
    *   domain value for interval
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
  def unbounded[T: DomainValueLike]: Interval1D[T] = apply(Bottom, Top)

  /*
   * These methods operate on collections of intervals.
   */

  /**
    * Compresses a collection of intervals by joining all adjacent and intersecting intervals.
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   a domain value type for this interval's domain.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Iterable[Interval1D[T]] =
    intervals.toList.foldRight(List.empty[Interval1D[T]]): (r, acc) =>
      acc match
        case head :: tail =>
          if (r isLeftAdjacentTo head) || (r intersects head) then (r ∪ head) :: tail
          else r :: head :: tail
        case Nil => List(r)

  /**
    * Checks if the collection of intervals is compressible. That is, are there any intervals that are adjacent to, or
    * intersecting with, their neighbors. If true, calling [[compress]] on the collection results in a smaller
    * collection covering the same domain.
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   a domain value type for this interval's domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Boolean =
    if intervals.isEmpty then false
    else
      intervals
        .zip(intervals.drop(1))
        .exists((left, right) => (left isLeftAdjacentTo right) || (left intersects right))

  /**
    * Checks if the collection of intervals is disjoint. That is, all neighboring intervals do not intersect. See
    * [[https://en.wikipedia.org/wiki/Disjoint_sets]].
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   a domain value type for this interval's domain.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Boolean =
    intervals.isEmpty || intervals
      .zip(intervals.drop(1))
      .forall((left, right) => !(left intersects right))

  /**
    * Finds all intervals, including all overlaps and gaps between intervals, as intervals. Inputs may be overlapping.
    * The result is disjoint and covers the span of the input intervals.
    *
    * @param intervals
    *   collection of intervals
    * @tparam T
    *   a domain value type for this interval's domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T: DomainValueLike](intervals: Iterable[Interval1D[T]]): Iterable[Interval1D[T]] =
    if intervals.isEmpty then intervals
    else
      val starts = (intervals.map(_.start) ++ intervals.map(_.end.rightAdjacent)).toList.sorted.distinct.dropRight(1)
      val ends = (intervals.map(_.end) ++ intervals.map(_.start.leftAdjacent)).toList
        .sorted(using Domain1D.endOrdering)
        .distinct
        .drop(1)
      // assert(starts.size == ends.size)
      starts.zip(ends).map(apply)

  /**
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps). See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]]. Result invariants:
    *   - `isDisjoint(intervals ++ complement(intervals)) == true`
    *   - `compress(intervals ++ complement(intervals)).toSeq == Seq(unbounded)`
    *
    * @param intervals
    *   a collection of intervals -- must be disjoint and ordered by start.
    * @tparam T
    *   a domain value type for this interval's domain.
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
