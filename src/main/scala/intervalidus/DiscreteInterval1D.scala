package intervalidus

import intervalidus.DiscreteDomain1D.{Bottom, Point, Top}

import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A one-dimensional interval over a contiguous set of discrete values in T. See
  * [[https://en.wikipedia.org/wiki/Interval_(mathematics)]] for more information.
  *
  * @tparam T
  *   a discrete value type for this interval's domain (e.g., Int, LocalDate) -- boundaries of the interval are defined
  *   in terms of DiscreteDomain1D[T] given the type class DiscreteValue[T].
  * @param start
  *   the "infimum", i.e., the lower/left boundary of the interval (inclusive)
  * @param end
  *   the "supremum", i.e., the upper/right boundary of the interval (inclusive) -- must be greater than or equal to the
  *   start
  */
case class DiscreteInterval1D[T: DiscreteValue](
  start: DiscreteDomain1D[T],
  end: DiscreteDomain1D[T]
) extends DimensionalBase.IntervalLike[DiscreteDomain1D[T]]:
  require(start <= end, s"Interval $this invalid")

  /**
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the value type
    * @return
    *   valid data in this interval
    */
  infix def withValue[V](value: V): DataIn1DBase.ValidData1D[V, T] = DataIn1DBase.ValidData1D(value, this)

  override def key: DiscreteDomain1D[T] = start

  override infix def contains(domainElement: DiscreteDomain1D[T]): Boolean =
    start <= domainElement && domainElement <= end

  // Use mathematical interval notation -- default.
  override def toString: String =
    val leftBrace = start match
      case Point(_) => "["
      case _        => "("

    val rightBrace = end match
      case Point(_) => "]"
      case _        => ")"

    s"$leftBrace$start..$end$rightBrace"

  // Use method-like interval notation -- useful in debugging, but will cause some toString unit tests to fail.
  //  override def toString: String = //    (start, end) match
  //      case (Bottom, Top) => "unbounded"
  //      case (Bottom, Point(e)) => s"intervalTo($e)"
  //      case (Point(s), Top) => s"intervalFrom($s)"
  //      case (Point(s), Point(e)) if s == e => s"intervalAt($s)"
  //      case (s, e) => s"interval($s, $e)"

  /**
    * Returns a new interval starting at the provided value.
    *
    * @param newStart
    *   the start of the new interval
    */
  def startingWith(newStart: DiscreteDomain1D[T]): DiscreteInterval1D[T] = copy(start = newStart)

  /**
    * Returns a new interval starting at the successor of the provided value.
    *
    * @param newStartPredecessor
    *   the predecessor of the start of the new interval
    */
  def startingAfter(newStartPredecessor: DiscreteDomain1D[T]): DiscreteInterval1D[T] =
    startingWith(newStartPredecessor.successor)

  /**
    * Returns a new singleton interval containing only the end of this interval.
    */
  def atEnd: DiscreteInterval1D[T] = startingWith(end)

  /**
    * Returns a new interval with the same end as this interval but with an unbounded start.
    */
  def fromBottom: DiscreteInterval1D[T] = startingWith(Bottom)

  /**
    * Returns a new interval ending at the provided value.
    *
    * @param newEnd
    *   the end of the new interval
    */
  def endingWith(newEnd: DiscreteDomain1D[T]): DiscreteInterval1D[T] = copy(end = newEnd)

  /**
    * Returns a new interval ending at the predecessor of the provided value.
    *
    * @param newEndSuccessor
    *   the successor of the end of the new interval
    */
  def endingBefore(newEndSuccessor: DiscreteDomain1D[T]): DiscreteInterval1D[T] =
    endingWith(newEndSuccessor.predecessor)

  /**
    * Returns a new singleton interval containing only the start of this interval.
    */
  def atStart: DiscreteInterval1D[T] = endingWith(start)

  /**
    * Returns a new interval with the same start as this interval but with an unbounded end.
    */
  def toTop: DiscreteInterval1D[T] = endingWith(Top)

  /**
    * Returns true only if there this interval is to the left of that interval, and there is no gap between them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isLeftAdjacentTo(that: DiscreteInterval1D[T]): Boolean = this.end.successor equiv that.start

  /**
    * Returns true only if there this interval is to the right of that interval, and there is no gap between them.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isRightAdjacentTo(that: DiscreteInterval1D[T]): Boolean = that isLeftAdjacentTo this

  /**
    * Returns true only if there is no gap between this and that.
    *
    * @param that
    *   the interval to test for adjacency.
    */
  infix def isAdjacentTo(that: DiscreteInterval1D[T]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that)

  /**
    * Returns true only if this and that have the same start.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameStartAs(that: DiscreteInterval1D[T]): Boolean = this.start equiv that.start

  /**
    * Returns true only if this and that have the same end.
    *
    * @param that
    *   the interval to test.
    */
  infix def hasSameEndAs(that: DiscreteInterval1D[T]): Boolean = this.end equiv that.end

  /**
    * Returns true only if there is no fixed start or end - spans the entire domain.
    */
  infix def isUnbounded: Boolean = (this.start equiv Bottom) && (this.end equiv Top)

  /**
    * Returns true only if this and that have elements of the domain in common (not disjoint).
    *
    * @param that
    *   the interval to test.
    */
  infix def intersects(that: DiscreteInterval1D[T]): Boolean = this.start <= that.end && that.start <= this.end

  /**
    * Finds the intersection between this and that.
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and none otherwise.
    */
  infix def intersectionWith(that: DiscreteInterval1D[T]): Option[DiscreteInterval1D[T]] =
    if !(this intersects that) then None
    else Some(DiscreteInterval1D(this.start max that.start, this.end min that.end))

  import DiscreteInterval1D.Remainder

  /**
    * Excludes that interval from this one. There are three possible outcomes:
    *   1. this interval is a subset of that one, so once it is excluded, nothing remains. Remainder.None is returned.
    *   1. that either lies outside of this or has a simple edge intersection (only contains the start or the end, but
    *      not both), and a single interval remains. Remainder.Single is returned.
    *   1. that interval is a proper subset of this one, containing neither the start or the end of this, so this
    *      interval gets split, and two intervals remain. Remainder.Split is returned
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder after exclusion.
    */
  infix def excluding(that: DiscreteInterval1D[T]): Remainder[DiscreteInterval1D[T]] =
    this intersectionWith that match
      case None => // no intersection, nothing to exclude
        Remainder.Single(this)
      case Some(commonBit) =>
        commonBit match
          case DiscreteInterval1D(midStart, midEnd) if midStart > start && midEnd < end => // split
            Remainder.Split(endingBefore(midStart), startingAfter(midEnd))
          case DiscreteInterval1D(midStart, _) if midStart > start => // later start, common end
            Remainder.Single(endingBefore(midStart))
          case DiscreteInterval1D(_, midEnd) if midEnd < end => // common start, earlier end
            Remainder.Single(startingAfter(midEnd))
          case _ => // common start and end -- nothing remains
            Remainder.None

  /**
    * Returns gap between this and that interval, if one exists.
    *
    * @param that
    *   the interval to evaluate.
    * @return
    *   some interval representing the gap between this and that if one exists, otherwise none.
    */
  infix def gapWith(that: DiscreteInterval1D[T]): Option[DiscreteInterval1D[T]] =
    if this intersects that then None
    else if this isAdjacentTo that then None
    else Some(DiscreteInterval1D((this.end min that.end).successor, (this.start max that.start).predecessor))

  /**
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gap that may
    * exist between them. So it is a proper union in the cases where this and that intersect or are adjacent, and a bit
    * more than that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  infix def joinedWith(that: DiscreteInterval1D[T]): DiscreteInterval1D[T] =
    DiscreteInterval1D(this.start min that.start, this.end max that.end)

  /**
    * Test for equivalence by comparing the start and end of this and that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true only if this and that have the same start and end.
    */
  infix def equiv(that: DiscreteInterval1D[T]): Boolean = (this hasSameStartAs that) && (this hasSameEndAs that)

  /**
    * Tests if that is a subset (proper or improper) of this.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if that is a subset of this.
    */
  infix def contains(that: DiscreteInterval1D[T]): Boolean = start <= that.start && that.end <= end

  /**
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  infix def isSubsetOf(that: DiscreteInterval1D[T]): Boolean = that contains this

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

  /**
    * Same as [[withValue]]
    *
    * Construct new valid data from this interval.
    *
    * @param value
    *   the value in the valid data
    * @tparam V
    *   the value type
    * @return
    *   valid data in this interval
    */
  infix def ->[V](value: V): DataIn1DBase.ValidData1D[V, T] = withValue(value)

  /**
    * Same as [[intersectionWith]].
    *
    * Finds the intersection between this and that.
    *
    * @param that
    *   the interval to intersect.
    * @return
    *   some interval representing the intersection if there is one, and none otherwise.
    */
  def ∩(that: DiscreteInterval1D[T]): Option[DiscreteInterval1D[T]] = this intersectionWith that

  /**
    * Same as [[excluding]].
    *
    * Excludes that interval from this one. There are three possible outcomes:
    *   1. this interval is a subset of that one, so once it is excluded, nothing remains.
    *   1. that either lies outside of this or has a simple edge intersection (only contains the start or the end, but
    *      not both), and a single interval remains.
    *   1. that interval is a proper subset of this one, containing neither the start or the end of this, so this
    *      interval gets split, and two intervals remain.
    *
    * @param that
    *   the interval to exclude.
    * @return
    *   The remainder after exclusion. Either `None`, `Some(_, None)`, or `Some(_, Some(_))` based on the outcome
    *   described above.
    */
  def \(that: DiscreteInterval1D[T]): Remainder[DiscreteInterval1D[T]] = this excluding that

  /**
    * Same as [[joinedWith]].
    *
    * A kind of union between this and that interval. Includes the domain of both this and that plus any gap that may
    * exist between them. So it is a proper union in the cases where this and that intersect or are adjacent, and a bit
    * more than that otherwise.
    *
    * @param that
    *   the interval to join.
    * @return
    *   the smallest interval including both this and that.
    */
  def ∪(that: DiscreteInterval1D[T]): DiscreteInterval1D[T] = this joinedWith that

  /**
    * Same as [[isSubsetOf]].
    *
    * Tests if this is a subset (proper or improper) of that.
    *
    * @param that
    *   the interval to test.
    * @return
    *   true if this is a subset of that.
    */
  def ⊆(that: DiscreteInterval1D[T]): Boolean = this isSubsetOf that

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
    * Returns an interval from the input value that is unbounded on the top/right.
    */
  def intervalFrom[T: DiscreteValue](s: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(s, Top)

  /**
    * Returns an interval to the input value that is unbounded on the bottom/left.
    */
  def intervalTo[T: DiscreteValue](e: DiscreteDomain1D[T]): DiscreteInterval1D[T] = apply(Bottom, e)

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
    * Returns an interval unbounded on both the left/bottom and right/top.
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
    * intersecting with, their neighbors. If true, calling [[compress]] on the collection will result in a smaller
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
    * Checks if the collection of intervals is disjoint. That is, all neighboring intervals do not intersect.
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
    * Sort a collection of intervals by the interval key (i.e., start).
    *
    * @param intervals
    *   collection of intervals
    * @tparam T
    *   a discrete value type for this interval's domain.
    * @return
    *   a new collection of intervals sorted by the key.
    */
  def sort[T: DiscreteValue](intervals: Iterable[DiscreteInterval1D[T]]): Iterable[DiscreteInterval1D[T]] =
    intervals.toList.sortBy(_.key)

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
    * Given a collection of intervals, finds the complement intervals (i.e., the gaps).
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
          else apply(priorEnd.successor, next.start.predecessor) :: acc
        (next.end, nextAcc)
    (if lastEnd.successor equiv Top then result else apply(lastEnd.successor, Top) :: result).reverse
