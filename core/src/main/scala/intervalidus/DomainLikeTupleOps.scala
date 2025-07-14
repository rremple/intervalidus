package intervalidus

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Type class for operating on domains and related structures of arbitrary dimension.
  *
  * An n-dimensional domain is represented by a tuple of `Domain1D[T`<sup>i</sup>`]` values (where i varies from 1 to
  * n), and each `T`<sup>i</sup> is a (potentially different) domain value type that is `DomainValueLike`.
  *
  * This makes extensive use of Scala 3's generic programming techniques on tuples (for more information, see
  * [[https://www.scala-lang.org/2021/02/26/tuples-bring-generic-programming-to-scala-3.html]]). These operations are
  * meant to support [[DomainLike]] type classes and should not need to be called directly.
  * @tparam D
  *   Tuple of domain one-dimensional domains of various domain value types
  */
trait DomainLikeTupleOps[D <: NonEmptyTuple]:
  def unbounded(d: Domain1D[Nothing]): D

  def arity: Int

  /*
   * Basic domain-like capabilities
   */

  def isUnboundedFromDomain(domainTuple: D): Boolean

  def toCodeLikeStringsFromDomain(domainTuple: D): List[String]

  def orderedHashesFromDomain(domainTuple: D): List[Double]

  def rightAdjacentFromDomain(domainTuple: D): D

  def leftAdjacentFromDomain(domainTuple: D): D

  def closeIfOpenFromDomain(domainTuple: D): D

  def toStringsFromDomain(domainTuple: D): List[String]

  /*
   * Enhanced domain-like capabilities
   */
  def equivFromDomains(thisDomainTuple: D, thatDomainTuple: D): Boolean

  def pointsFromDomains(startDomainTuple: D, endDomainTuple: D): Iterable[D]

  def afterStartFromDomains(thisDomainTuple: D, thatDomainTuple: D): Boolean

  def afterOrAtStartFromDomains(thisDomainTuple: D, thatDomainTuple: D): Boolean

  def beforeEndFromDomains(thisDomainTuple: D, thatDomainTuple: D): Boolean

  def beforeOrAtEndFromDomains(thisDomainTuple: D, thatDomainTuple: D): Boolean

  def maxStartFromDomains(domainTuple1: D, domainTuple2: D): D

  def minEndFromDomains(domainTuple1: D, domainTuple2: D): D

  def minStartFromDomains(domainTuple1: D, domainTuple2: D): D

  def maxEndFromDomains(domainTuple1: D, domainTuple2: D): D

  def isClosedOrUnboundedFromDomain(domainTuple: D): Boolean

  def validIntervalBoundsFromDomains(startDomainTuple: D, endDomainTuple: D): Boolean

  def compareDomains(x: D, y: D): Int

  /*
   * Valid data-like capabilities
   */

  // first dimension start string, first dimension end string, value + remaining dimension string
  def preprocessForGridFromValidData[V](validData: ValidData[V, D]): (String, String, String)

  /*
   * Interval-like capabilities
   */

  def preprocessForGridFromIntervals(intervals: Iterable[Interval[D]]): Iterable[(String, String, String)]

  // (equivalent, adjacent, total)
  def equivalencyAndAdjacencyFromIntervals(beforeInterval: Interval[D], afterInterval: Interval[D]): (Int, Int, Int)

  def excludingFromIntervals(thisInterval: Interval[D], thatInterval: Interval[D]): NonEmptyTuple

  def separateUsingFromIntervals(thisInterval: Interval[D], thatInterval: Interval[D]): Iterable[Interval[D]]

  def gapWithFromIntervals(thisInterval: Interval[D], thatInterval: Interval[D]): Option[Interval[D]]

  def uniqueIntervalsFromInterval(intervals: Iterable[Interval[D]]): Iterable[Interval[D]]

  def potentiallyAdjacentKeysFromInterval(interval: Interval[D]): List[(D, Int)]

  def toStringsFromInterval(interval: Interval[D]): List[String]

  def toCodeLikeStringsFromInterval(interval: Interval[D]): List[String]

/**
  * Use recursive decomposition of tuples to provide domain-like capabilities to tuples.
  */
object DomainLikeTupleOps:
  private type OneDimDomain[DV] = Domain1D[DV] *: EmptyTuple
  private type MultiDimDomain[DV, DomainTail <: NonEmptyTuple] = Domain1D[DV] *: DomainTail

  /**
    * Base case, for a one-dimensional domain (empty tail)
    */
  given DomainLikeOneDimOps[DV: DomainValueLike]: DomainLikeTupleOps[OneDimDomain[DV]] with

    private inline def headInterval(interval: Interval[OneDimDomain[DV]]): Interval1D[DV] =
      Interval1D(interval.start.head, interval.end.head)

    private inline def from1d(interval: Interval1D[DV]): Interval[OneDimDomain[DV]] =
      Interval(interval.start, interval.end)

    inline override def unbounded(d: Domain1D[Nothing]): OneDimDomain[DV] = Domain.in1D(d)

    inline override def arity: Int = 1

    inline override def isUnboundedFromDomain(domainTuple: OneDimDomain[DV]): Boolean =
      domainTuple.head.isUnbounded

    inline override def toCodeLikeStringsFromDomain(domainTuple: OneDimDomain[DV]): List[String] =
      List(domainTuple.head.toCodeLikeString)

    inline override def orderedHashesFromDomain(domainTuple: OneDimDomain[DV]): List[Double] =
      List(domainTuple.head.orderedHash)

    inline override def rightAdjacentFromDomain(domainTuple: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(domainTuple.head.rightAdjacent)

    inline override def leftAdjacentFromDomain(domainTuple: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(domainTuple.head.leftAdjacent)

    inline override def closeIfOpenFromDomain(domainTuple: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(domainTuple.head.closeIfOpen)

    inline override def toStringsFromDomain(domainTuple: OneDimDomain[DV]): List[String] =
      List((domainTuple.head: Domain1D[DV] /*bug?*/ ).toString)

    inline override def equivFromDomains(
      thisDomainTuple: OneDimDomain[DV],
      thatDomainTuple: OneDimDomain[DV]
    ): Boolean = thisDomainTuple.head equiv thatDomainTuple.head

    inline override def pointsFromDomains(
      startDomainTuple: OneDimDomain[DV],
      endDomainTuple: OneDimDomain[DV]
    ): Iterable[OneDimDomain[DV]] = startDomainTuple.head.pointsTo(endDomainTuple.head).map(Domain.in1D)

    inline override def afterStartFromDomains(
      thisDomainTuple: OneDimDomain[DV],
      thatDomainTuple: OneDimDomain[DV]
    ): Boolean = thisDomainTuple.head afterStart thatDomainTuple.head

    inline override def afterOrAtStartFromDomains(
      thisDomainTuple: OneDimDomain[DV],
      thatDomainTuple: OneDimDomain[DV]
    ): Boolean = thisDomainTuple.head afterOrAtStart thatDomainTuple.head

    inline override def beforeEndFromDomains(
      thisDomainTuple: OneDimDomain[DV],
      thatDomainTuple: OneDimDomain[DV]
    ): Boolean = thisDomainTuple.head beforeEnd thatDomainTuple.head

    inline override def beforeOrAtEndFromDomains(
      thisDomainTuple: OneDimDomain[DV],
      thatDomainTuple: OneDimDomain[DV]
    ): Boolean = thisDomainTuple.head beforeOrAtEnd thatDomainTuple.head

    inline override def maxStartFromDomains(tuple1: OneDimDomain[DV], tuple2: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(tuple1.head maxStart tuple2.head)

    inline override def minEndFromDomains(tuple1: OneDimDomain[DV], tuple2: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(tuple1.head minEnd tuple2.head)

    inline override def minStartFromDomains(tuple1: OneDimDomain[DV], tuple2: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(tuple1.head minStart tuple2.head)

    inline override def maxEndFromDomains(tuple1: OneDimDomain[DV], tuple2: OneDimDomain[DV]): OneDimDomain[DV] =
      Domain.in1D(tuple1.head maxEnd tuple2.head)

    inline override def isClosedOrUnboundedFromDomain(domainTuple: OneDimDomain[DV]): Boolean =
      domainTuple.head.isClosedOrUnbounded

    inline override def validIntervalBoundsFromDomains(
      startDomainTuple: OneDimDomain[DV],
      endDomainTuple: OneDimDomain[DV]
    ): Boolean = Interval1D.validBounds(startDomainTuple.head, endDomainTuple.head)

    inline override def compareDomains(x: OneDimDomain[DV], y: OneDimDomain[DV]): Int =
      summon[Ordering[Domain1D[DV]]].compare(x.head, y.head)

    /*
     * Valid data-like capabilities
     */

    inline override def preprocessForGridFromValidData[V](
      validData: ValidData[V, OneDimDomain[DV]]
    ): (String, String, String) =
      val head = headInterval(validData.interval)
      (head.start.toString, head.end.toString, s"${validData.valueToString}") // just one dimension

    /*
     * Interval-like capabilities
     */

    inline override def preprocessForGridFromIntervals(
      intervals: Iterable[Interval[OneDimDomain[DV]]]
    ): Iterable[(String, String, String)] =
      Interval1D.preprocessForGrid(intervals.map(headInterval))

    inline override def equivalencyAndAdjacencyFromIntervals(
      beforeDomainTuple: Interval[OneDimDomain[DV]],
      afterDomainTuple: Interval[OneDimDomain[DV]]
    ): (Int, Int, Int) =
      val beforeHead = headInterval(beforeDomainTuple)
      val afterHead = headInterval(afterDomainTuple)
      (if beforeHead equiv afterHead then 1 else 0, if beforeHead isLeftAdjacentTo afterHead then 1 else 0, 1)

    inline override def excludingFromIntervals(
      thisInterval: Interval[OneDimDomain[DV]],
      thatInterval: Interval[OneDimDomain[DV]]
    ): NonEmptyTuple =
      headInterval(thisInterval).excluding(headInterval(thatInterval)) *: EmptyTuple

    inline override def separateUsingFromIntervals(
      thisInterval: Interval[OneDimDomain[DV]],
      thatInterval: Interval[OneDimDomain[DV]]
    ): Iterable[Interval[OneDimDomain[DV]]] =
      headInterval(thisInterval).separateUsing(headInterval(thatInterval)).map(from1d)

    inline override def gapWithFromIntervals(
      thisInterval: Interval[OneDimDomain[DV]],
      thatInterval: Interval[OneDimDomain[DV]]
    ): Option[Interval[OneDimDomain[DV]]] =
      headInterval(thisInterval).gapWith(headInterval(thatInterval)).map(from1d)

    inline override def uniqueIntervalsFromInterval(
      intervals: Iterable[Interval[OneDimDomain[DV]]]
    ): Iterable[Interval[OneDimDomain[DV]]] =
      Interval1D.uniqueIntervals(intervals.map(headInterval)).map(from1d)

    inline override def potentiallyAdjacentKeysFromInterval(
      interval: Interval[OneDimDomain[DV]]
    ): List[(OneDimDomain[DV], Int)] = List((interval.end.head.rightAdjacent, 1), (interval.start, 0))

    inline override def toStringsFromInterval(
      interval: Interval[OneDimDomain[DV]]
    ): List[String] = List(headInterval(interval).toString)

    inline override def toCodeLikeStringsFromInterval(
      interval: Interval[OneDimDomain[DV]]
    ): List[String] = List(headInterval(interval).toCodeLikeString)

  /**
    * Inductive case for a domain with two or more dimensions (non-empty tail)
    */
  given DomainLikeMultiDimOps[
    DV: DomainValueLike,
    DomainTail <: NonEmptyTuple
  ](using applyToTail: DomainLikeTupleOps[DomainTail]): DomainLikeTupleOps[Domain1D[DV] *: DomainTail] with

    private inline def headInterval(interval: Interval[MultiDimDomain[DV, DomainTail]]): Interval1D[DV] =
      Interval1D(interval.start.head, interval.end.head)

    private inline def tailInterval(interval: Interval[MultiDimDomain[DV, DomainTail]]): Interval[DomainTail] =
      Interval(interval.start.tail, interval.end.tail)

    inline override def unbounded(d: Domain1D[Nothing]): MultiDimDomain[DV, DomainTail] = d *: applyToTail.unbounded(d)

    inline override def arity: Int = 1 + applyToTail.arity

    inline override def isUnboundedFromDomain(domainTuple: MultiDimDomain[DV, DomainTail]): Boolean =
      domainTuple.head.isUnbounded && applyToTail.isUnboundedFromDomain(domainTuple.tail)

    inline override def toCodeLikeStringsFromDomain(domainTuple: MultiDimDomain[DV, DomainTail]): List[String] =
      domainTuple.head.toCodeLikeString :: applyToTail.toCodeLikeStringsFromDomain(domainTuple.tail)

    inline override def orderedHashesFromDomain(domainTuple: MultiDimDomain[DV, DomainTail]): List[Double] =
      domainTuple.head.orderedHash :: applyToTail.orderedHashesFromDomain(domainTuple.tail)

    inline override def rightAdjacentFromDomain(
      domainTuple: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple.head.rightAdjacent *:
      applyToTail.rightAdjacentFromDomain(domainTuple.tail)

    inline override def leftAdjacentFromDomain(
      domainTuple: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple.head.leftAdjacent *:
      applyToTail.leftAdjacentFromDomain(domainTuple.tail)

    inline override def closeIfOpenFromDomain(
      domainTuple: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple.head.closeIfOpen *:
      applyToTail.closeIfOpenFromDomain(domainTuple.tail)

    inline override def toStringsFromDomain(
      domainTuple: MultiDimDomain[DV, DomainTail]
    ): List[String] = (domainTuple.head: Domain1D[DV] /*bug?*/ ).toString ::
      applyToTail.toStringsFromDomain(domainTuple.tail)

    inline override def equivFromDomains(
      thisDomainTuple: MultiDimDomain[DV, DomainTail],
      thatDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = (thisDomainTuple.head equiv thatDomainTuple.head) &&
      applyToTail.equivFromDomains(thisDomainTuple.tail, thatDomainTuple.tail)

    inline override def pointsFromDomains(
      startDomainTuple: MultiDimDomain[DV, DomainTail],
      endDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Iterable[MultiDimDomain[DV, DomainTail]] =
      for
        head <- startDomainTuple.head.pointsTo(endDomainTuple.head)
        tail <- applyToTail.pointsFromDomains(startDomainTuple.tail, endDomainTuple.tail)
      yield head *: tail

    inline override def afterStartFromDomains(
      thisDomainTuple: MultiDimDomain[DV, DomainTail],
      thatDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = (thisDomainTuple.head afterStart thatDomainTuple.head) &&
      applyToTail.afterStartFromDomains(thisDomainTuple.tail, thatDomainTuple.tail)

    inline override def afterOrAtStartFromDomains(
      thisDomainTuple: MultiDimDomain[DV, DomainTail],
      thatDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = (thisDomainTuple.head afterOrAtStart thatDomainTuple.head) &&
      applyToTail.afterOrAtStartFromDomains(thisDomainTuple.tail, thatDomainTuple.tail)

    inline override def beforeEndFromDomains(
      thisDomainTuple: MultiDimDomain[DV, DomainTail],
      thatDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = (thisDomainTuple.head beforeEnd thatDomainTuple.head) &&
      applyToTail.beforeEndFromDomains(thisDomainTuple.tail, thatDomainTuple.tail)

    inline override def beforeOrAtEndFromDomains(
      thisDomainTuple: MultiDimDomain[DV, DomainTail],
      thatDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = (thisDomainTuple.head beforeOrAtEnd thatDomainTuple.head) &&
      applyToTail.beforeOrAtEndFromDomains(thisDomainTuple.tail, thatDomainTuple.tail)

    inline override def maxStartFromDomains(
      domainTuple1: MultiDimDomain[DV, DomainTail],
      domainTuple2: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple1.head.maxStart(domainTuple2.head) *:
      applyToTail.maxStartFromDomains(domainTuple1.tail, domainTuple2.tail)

    inline override def minEndFromDomains(
      domainTuple1: MultiDimDomain[DV, DomainTail],
      domainTuple2: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple1.head.minEnd(domainTuple2.head) *:
      applyToTail.minEndFromDomains(domainTuple1.tail, domainTuple2.tail)

    inline override def minStartFromDomains(
      domainTuple1: MultiDimDomain[DV, DomainTail],
      domainTuple2: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple1.head.minStart(domainTuple2.head) *:
      applyToTail.minStartFromDomains(domainTuple1.tail, domainTuple2.tail)

    inline override def maxEndFromDomains(
      domainTuple1: MultiDimDomain[DV, DomainTail],
      domainTuple2: MultiDimDomain[DV, DomainTail]
    ): MultiDimDomain[DV, DomainTail] = domainTuple1.head.maxEnd(domainTuple2.head) *:
      applyToTail.maxEndFromDomains(domainTuple1.tail, domainTuple2.tail)

    inline override def isClosedOrUnboundedFromDomain(domainTuple: MultiDimDomain[DV, DomainTail]): Boolean =
      domainTuple.head.isClosedOrUnbounded && applyToTail.isClosedOrUnboundedFromDomain(domainTuple.tail)

    inline override def validIntervalBoundsFromDomains(
      startDomainTuple: MultiDimDomain[DV, DomainTail],
      endDomainTuple: MultiDimDomain[DV, DomainTail]
    ): Boolean = Interval1D.validBounds(startDomainTuple.head, endDomainTuple.head) &&
      applyToTail.validIntervalBoundsFromDomains(startDomainTuple.tail, endDomainTuple.tail)

    inline override def compareDomains(x: MultiDimDomain[DV, DomainTail], y: MultiDimDomain[DV, DomainTail]): Int =
      val headCompare: Int = summon[Ordering[Domain1D[DV]]].compare(x.head, y.head)
      if headCompare != 0 then headCompare else applyToTail.compareDomains(x.tail, y.tail)

    /*
     * Valid data-like capabilities
     */

    // not recursive
    // first dimension start string, first dimension end string, value + remaining dimension string
    inline override def preprocessForGridFromValidData[V](
      validData: ValidData[V, MultiDimDomain[DV, DomainTail]]
    ): (String, String, String) =
      val head = headInterval(validData.interval)
      val tailNoBraces = applyToTail.toStringsFromInterval(tailInterval(validData.interval)).mkString(" x ")
      (head.start.toString, head.end.toString, s"${validData.valueToString} $tailNoBraces")

    /*
     * Interval-like capabilities
     */

    // not recursive
    // first dimension start string, first dimension end string, interval grid-formatted string
    inline override def preprocessForGridFromIntervals(
      intervals: Iterable[Interval[MultiDimDomain[DV, DomainTail]]]
    ): Iterable[(String, String, String)] =
      Interval1D.preprocessForGrid(intervals.map(headInterval))

    inline override def equivalencyAndAdjacencyFromIntervals(
      beforeDomainTuple: Interval[MultiDimDomain[DV, DomainTail]],
      afterDomainTuple: Interval[MultiDimDomain[DV, DomainTail]]
    ): (Int, Int, Int) =
      val beforeHead = headInterval(beforeDomainTuple)
      val afterHead = headInterval(afterDomainTuple)
      val equivalent = beforeHead equiv afterHead
      val adjacent = beforeHead isLeftAdjacentTo afterHead
      val (tailEquivalency, tailAdjacency, tailTotal) =
        applyToTail.equivalencyAndAdjacencyFromIntervals(
          tailInterval(beforeDomainTuple),
          tailInterval(afterDomainTuple)
        )
      (tailEquivalency + (if equivalent then 1 else 0), tailAdjacency + (if adjacent then 1 else 0), tailTotal + 1)

    inline override def excludingFromIntervals(
      thisInterval: Interval[MultiDimDomain[DV, DomainTail]],
      thatInterval: Interval[MultiDimDomain[DV, DomainTail]]
    ): NonEmptyTuple = headInterval(thisInterval).excluding(headInterval(thatInterval)) *:
      applyToTail.excludingFromIntervals(tailInterval(thisInterval), tailInterval(thatInterval))

    def separateUsingFromIntervals(
      thisInterval: Interval[MultiDimDomain[DV, DomainTail]],
      thatInterval: Interval[MultiDimDomain[DV, DomainTail]]
    ): Iterable[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        headInterval1D <- headInterval(thisInterval).separateUsing(headInterval(thatInterval))
        tailInterval <- applyToTail.separateUsingFromIntervals(tailInterval(thisInterval), tailInterval(thatInterval))
      yield tailInterval withHead headInterval1D

    def gapWithFromIntervals(
      thisInterval: Interval[MultiDimDomain[DV, DomainTail]],
      thatInterval: Interval[MultiDimDomain[DV, DomainTail]]
    ): Option[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        headInterval1D <- headInterval(thisInterval).gapWith(headInterval(thatInterval))
        tailInterval <- applyToTail.gapWithFromIntervals(tailInterval(thisInterval), tailInterval(thatInterval))
      yield tailInterval withHead headInterval1D

    def uniqueIntervalsFromInterval(
      intervals: Iterable[Interval[MultiDimDomain[DV, DomainTail]]]
    ): Iterable[Interval[MultiDimDomain[DV, DomainTail]]] =
      for
        headInterval1D <- Interval1D.uniqueIntervals(intervals.map(headInterval))
        tailInterval <- applyToTail.uniqueIntervalsFromInterval(intervals.map(tailInterval))
      yield tailInterval withHead headInterval1D

    inline override def potentiallyAdjacentKeysFromInterval(
      interval: Interval[MultiDimDomain[DV, DomainTail]]
    ): List[(MultiDimDomain[DV, DomainTail], Int)] =
      for
        (head, headSwaps) <- List((interval.end.head.rightAdjacent, 1), (interval.start.head, 0))
        (tail, tailSwaps) <- applyToTail.potentiallyAdjacentKeysFromInterval(tailInterval(interval))
        if headSwaps + tailSwaps <= 1
      yield (head *: tail, headSwaps + tailSwaps)

    inline override def toStringsFromInterval(
      interval: Interval[MultiDimDomain[DV, DomainTail]]
    ): List[String] = headInterval(interval).toString ::
      applyToTail.toStringsFromInterval(tailInterval(interval))

    inline override def toCodeLikeStringsFromInterval(
      interval: Interval[MultiDimDomain[DV, DomainTail]]
    ): List[String] = headInterval(interval).toCodeLikeString ::
      applyToTail.toCodeLikeStringsFromInterval(tailInterval(interval))
