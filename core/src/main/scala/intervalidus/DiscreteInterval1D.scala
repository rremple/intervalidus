package intervalidus

import intervalidus.DiscreteDomain1D.{Bottom, Point, Top}
import intervalidus.DiscreteInterval1D.{interval, intervalFrom, intervalTo}

import java.time.LocalDate
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A one-dimensional interval over a contiguous set of discrete values in T. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @tparam T
  *   a discrete value type for this interval's domain (e.g., Int, LocalDate) -- boundaries of the interval are defined
  *   in terms of `DiscreteDomain1D[T]` given the type class `DiscreteValue[T]`.
  * @param start
  *   the "infimum", i.e., the left (or below or back, depending on context) boundary of the interval (inclusive)
  * @param end
  *   the "supremum", i.e., the right (or above or front, depending on context) boundary of the interval (inclusive) --
  *   must be greater than or equal to the start
  */
case class DiscreteInterval1D[T: DiscreteValue](
  start: DiscreteDomain1D[T],
  end: DiscreteDomain1D[T]
) extends DiscreteIntervalLike[DiscreteDomain1D[T], DiscreteInterval1D[T]]:
  require(start <= end, s"Interval $this invalid")

  import DiscreteInterval1D.Remainder

  override type ExclusionRemainder = Remainder[DiscreteInterval1D[T]]

  override infix def withValue[V](value: V): ValidData1D[V, T] = ValidData1D(value, this)

  override infix def contains(domainElement: DiscreteDomain1D[T]): Boolean =
    start <= domainElement && domainElement <= end

  override def points: Iterable[DiscreteDomain1D[T]] =
    val discreteValue = summon[DiscreteValue[T]]

    def nearest(d: DiscreteDomain1D[T]): DiscreteDomain1D[T] =
      d match
        case point @ Point(_) => point
        case Bottom           => Point(discreteValue.minValue)
        case Top              => Point(discreteValue.maxValue)

    Iterable.unfold(Some(interval(nearest(start), nearest(end))): Option[DiscreteInterval1D[T]]):
      case None => None
      case Some(prevRemaining) =>
        val nextRemaining =
          if prevRemaining.start equiv prevRemaining.end then None
          else Some(prevRemaining.fromAfter(prevRemaining.start))
        Some(prevRemaining.start, nextRemaining)

  override infix def isAdjacentTo(that: DiscreteInterval1D[T]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that)

  override infix def intersects(that: DiscreteInterval1D[T]): Boolean = this.start <= that.end && that.start <= this.end

  override infix def intersectionWith(that: DiscreteInterval1D[T]): Option[DiscreteInterval1D[T]] =
    if !(this intersects that) then None
    else Some(DiscreteInterval1D(this.start max that.start, this.end min that.end))

  override infix def joinedWith(that: DiscreteInterval1D[T]): DiscreteInterval1D[T] =
    DiscreteInterval1D(this.start min that.start, this.end max that.end)

  override infix def contains(that: DiscreteInterval1D[T]): Boolean = start <= that.start && that.end <= end

  override def after: Option[DiscreteInterval1D[T]] = end.successor match
    case Top          => None
    case endSuccessor => Some(intervalFrom(endSuccessor))

  override def before: Option[DiscreteInterval1D[T]] = start.predecessor match
    case Bottom           => None
    case startPredecessor => Some(intervalTo(startPredecessor))

  // Use mathematical interval notation -- default.
  override def toString: String =
    val leftBrace = start match
      case Point(_) => "["
      case _        => "("

    val rightBrace = end match
      case Point(_) => "]"
      case _        => ")"

    s"$leftBrace$start..$end$rightBrace"

  // Use method-like interval notation -- useful when constructing tests.
  override def toCodeLikeString: String =
    def codeFor(value: T): String = value match
      case d: LocalDate => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
      case _            => value.toString
    (start, end) match
      case (Bottom, Top)                  => "unbounded"
      case (Bottom, Point(e))             => s"intervalTo(${codeFor(e)})"
      case (Point(s), Top)                => s"intervalFrom(${codeFor(s)})"
      case (Point(s), Point(e)) if s == e => s"intervalAt(${codeFor(s)})"
      case (Point(s), Point(e))           => s"interval(${codeFor(s)}, ${codeFor(e)})"
      case (s, e)                         => s"interval(${s.toCodeLikeString}, ${e.toCodeLikeString})"

  override def from(newStart: DiscreteDomain1D[T]): DiscreteInterval1D[T] = copy(start = newStart)

  override def to(newEnd: DiscreteDomain1D[T]): DiscreteInterval1D[T] = copy(end = newEnd)

  override def fromBottom: DiscreteInterval1D[T] = from(Bottom)

  override def toTop: DiscreteInterval1D[T] = to(Top)

  override infix def isLeftAdjacentTo(that: DiscreteInterval1D[T]): Boolean = this.end.successor equiv that.start

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
  override infix def excluding(that: DiscreteInterval1D[T]): ExclusionRemainder =
    this intersectionWith that match
      case None => // no intersection, nothing to exclude
        Remainder.Single(this)
      case Some(commonBit) =>
        commonBit match
          case DiscreteInterval1D(midStart, midEnd) if midStart > start && midEnd < end => // split
            Remainder.Split(toBefore(midStart), fromAfter(midEnd))
          case DiscreteInterval1D(midStart, _) if midStart > start => // later start, common end
            Remainder.Single(toBefore(midStart))
          case DiscreteInterval1D(_, midEnd) if midEnd < end => // common start, earlier end
            Remainder.Single(fromAfter(midEnd))
          case _ => // common start and end -- nothing remains
            Remainder.None

  override infix def gapWith(that: DiscreteInterval1D[T]): Option[DiscreteInterval1D[T]] =
    if this intersects that then None
    else if this isAdjacentTo that then None
    else Some(DiscreteInterval1D((this.end min that.end).successor, (this.start max that.start).predecessor))

  /**
    * Cross this interval with that interval to arrive at a new two-dimensional interval.
    * @param that
    *   a one-dimensional interval to be used in the vertical dimension
    * @tparam T2
    *   discrete value type for that interval
    * @return
    *   a new two-dimensional interval with this interval as the horizontal component and that interval as the vertical
    *   component.
    */
  infix def x[T2: DiscreteValue](that: DiscreteInterval1D[T2]): DiscreteInterval2D[T, T2] =
    DiscreteInterval2D(this, that)

  // equivalent symbolic method names

  override infix def ->[V](value: V): ValidData1D[V, T] = withValue(value)

/**
  * Companion for the one-dimensional interval used in defining and operating on valid data.
  */
object DiscreteInterval1D:
  // used in return type of excluding
  enum Remainder[+G]:
    case None
    case Single(g: G)
    case Split(left: G, right: G)

  /*
   * Helpers, to avoid using "Top" and "Bottom" literals everywhere when constructing discrete intervals!
   */

  /**
    * Returns an interval from the input value that is unbounded on the right.
    */
  def intervalFrom[T: DiscreteValue](s: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s, Top)

  /**
    * Returns an interval from after the input value that is unbounded on the right.
    */
  def intervalFromAfter[T: DiscreteValue](s: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s.successor, Top)

  /**
    * Returns an interval from before the input value that is unbounded on the right.
    */
  def intervalFromBefore[T: DiscreteValue](s: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s.predecessor, Top)

  /**
    * Returns an interval to the input value that is unbounded on the left.
    */
  def intervalTo[T: DiscreteValue](e: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(Bottom, e)

  /**
    * Returns an interval to after the input value that is unbounded on the left.
    */
  def intervalToAfter[T: DiscreteValue](e: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(Bottom, e.successor)

  /**
    * Returns an interval to before the input value that is unbounded on the left.
    */
  def intervalToBefore[T: DiscreteValue](e: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(Bottom, e.predecessor)

  /**
    * Returns an interval that starts and ends at the same value.
    */
  def intervalAt[T: DiscreteValue](s: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s, s)

  /**
    * Returns an interval that starts and ends at the different values.
    */
  def interval[T: DiscreteValue](s: DiscreteDomain1D[T], e: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s, e)

  /**
    * Returns an interval that starts and ends at the different optional values. In the input, Some(_) represents being
    * bounded where None represents being unbounded.
    */
  def interval[T: DiscreteValue](s: Option[T], e: Option[T]): DiscreteInterval1D[T] =
    apply(
      s.map(Point(_)).getOrElse(Bottom),
      e.map(Point(_)).getOrElse(Top)
    )

  /**
    * Returns the interval between `before` and `after`. This is equivalent to `before.gapWith(after).get`, but without
    * intersection and adjacency checks. Only use this function if you know `before.end.successor < after.start`, e.g.,
    * `before` and `after` are exclusion remainders.
    *
    * @param before
    *   interval on the left/bottom/back side
    * @param after
    *   interval on the right/top/front side
    * @tparam T
    *   discrete value for interval
    * @return
    *   the interval made from the gap between the two inputs
    */
  def between[T: DiscreteValue](
    before: DiscreteInterval1D[T],
    after: DiscreteInterval1D[T]
  ): DiscreteInterval1D[T] = interval(before.end.successor, after.start.predecessor)

  /**
    * Returns an interval unbounded on both the left and right.
    */
  def unbounded[T: DiscreteValue]: DiscreteInterval1D[T] = apply(Bottom, Top)

  /*
   * These methods operate on collections of intervals.
   */

  /**
    * Compresses a collection of intervals by joining all adjacent and intersecting intervals.
    *
    * @param intervals
    *   a collection of intervals -- must be ordered by start.
    * @tparam T
    *   a discrete value type for this interval's domain.
    * @return
    *   a new (possibly smaller) collection of intervals covering the same domain as the input.
    */
  def compress[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Iterable[DiscreteInterval1D[T]] =
    intervals.toList.foldRight(List.empty[DiscreteInterval1D[T]]): (r, acc) =>
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
    *   a discrete value type for this interval's domain.
    * @return
    *   true if the collection is compressible, false otherwise.
    */
  def isCompressible[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Boolean =
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
    *   a discrete value type for this interval's domain.
    * @return
    *   true if the collection is disjoint, false otherwise.
    */
  def isDisjoint[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Boolean =
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
    *   a discrete value type for this interval's domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def uniqueIntervals[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Iterable[DiscreteInterval1D[T]] =
    if intervals.isEmpty then intervals
    else
      val starts = (intervals.map(_.start) ++ intervals.map(_.end.successor)).toList.sorted.distinct.dropRight(1)
      val ends = (intervals.map(_.end) ++ intervals.map(_.start.predecessor)).toList.sorted.distinct.drop(1)
      // assert(starts.size == ends.size)
      starts.zip(ends).map(apply)

  /**
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps). See
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    *
    * @param intervals
    *   a collection of intervals -- must be disjoint and ordered by start.
    * @tparam T
    *   a discrete value type for this interval's domain.
    * @return
    *   a new collection of intervals representing disjoint intervals covering the span of the input.
    */
  def complement[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Iterable[DiscreteInterval1D[T]] =
    val (lastEnd, result) = intervals.foldLeft((Bottom: DiscreteDomain1D[T], List.empty[DiscreteInterval1D[T]])):
      case ((priorEnd, acc), next) =>
        val nextAcc =
          if next.start.predecessor equiv priorEnd then acc
          else interval(priorEnd.successor, next.start.predecessor) :: acc
        (next.end, nextAcc)
    (if lastEnd.successor equiv Top then result else apply(lastEnd.successor, Top) :: result).reverse
