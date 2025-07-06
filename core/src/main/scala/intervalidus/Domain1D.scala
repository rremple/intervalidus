package intervalidus

import java.time.{LocalDate, LocalDateTime}
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Domains are based on an underlying domain value type and used to define the boundaries of an interval. It describes
  * specific data points in the domain value range as well as the special "`Bottom`" and "`Top`" cases which
  * conceptually lie below and above this finite range of data points (logically below and above `minValue` and
  * `maxValue` respectively). Domains can be based on domain values that are discrete or continuous. When continuous, a
  * boundary point can either be open or closed, where discrete points must always be closed. This also gives a way to
  * completely describe adjacency. When domain values are discrete, the left and right adjacent domains of a point are
  * the respective predecessors and successors of the domain value -- this also gives us a way to accommodate having a
  * predecessor or successor on a boundary (i.e., `maxValue.rightAdjacent == Top` and `minValue.leftAdjacent ==
  * Bottom`). When domains are continuous, the left and right adjacent domains are always the same: open if the point is
  * closed and closed if the point is open.
  *
  * @tparam T
  *   expected to be a domain value (i.e., `DomainValueLike[T]` should be given).
  */
enum Domain1D[+T]:
  /**
    * Smaller than smallest data point (like -∞)
    */
  case Bottom

  /**
    * A single data point in the finite range of this domain
    */
  case Point[P: DomainValueLike](value: P) extends Domain1D[P]

  /**
    * Exclude a single data point in the finite range of this domain. This extends the closed point domain with all the
    * points complements, and suitable for the start or end of a continuous interval. (This case is not used in discrete
    * intervals.)
    */
  case OpenPoint[P: DomainValueLike](value: P) extends Domain1D[P]

  /**
    * Larger than largest data point (like +∞)
    */
  case Top

  override def toString: String = this match
    case Bottom       => "-∞"
    case Point(t)     => t.toString
    case OpenPoint(t) => t.toString
    case Top          => "+∞"

  def toCodeLikeString: String =
    def codeFor(value: T): String = value match
      case d: LocalDate => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
      case d: LocalDateTime =>
        s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})" +
          s".atTime(${d.getHour},${d.getMinute},${d.getSecond},${d.getNano})"
      case _ => value.toString

    this match
      case Bottom       => "Bottom"
      case Point(t)     => s"Point(${codeFor(t)})"
      case OpenPoint(t) => s"OpenPoint(${codeFor(t)})"
      case Top          => "Top"

  def isUnbounded: Boolean = this match
    case Point(_) | OpenPoint(_) => false
    case _                       => true

  def leftBrace: String = this match
    case Point(_) => "["
    case _        => "("

  def rightBrace: String = this match
    case Point(_) => "]"
    case _        => ")"

  /**
    * Cross this domain element with that domain element to arrive at a new two-dimensional domain tuple.
    *
    * @param that
    *   a one-dimensional domain element to be used as the vertical dimension.
    * @tparam T2
    *   domain value type for that domain.
    * @return
    *   a new two-dimensional domain tuple with this as the horizontal component and that as the vertical component.
    */
  infix def x[T2: DomainValueLike](that: Domain1D[T2]): Domain.In2D[T, T2] =
    (this, that)

import intervalidus.Domain1D.{Bottom, OpenPoint, Point, Top}

/**
  * Companion for the one-dimensional domain used in defining and operating on intervals.
  */
object Domain1D:
  /**
    * Construct a domain point (closed) based on a domain value.
    *
    * @param t
    *   the domain value
    * @tparam T
    *   a domain value type
    * @return
    *   the closed domain point of the domain value
    */
  def domain[T: DomainValueLike](t: T): Domain1D[T] = t

  /**
    * Construct a domain point (open) based on a domain value.
    *
    * @param t
    *   the domain value
    * @tparam T
    *   a domain value type
    * @return
    *   the open domain point of the domain value
    */
  def open[T: DomainValueLike](t: T): Domain1D[T] = summon[DomainValueLike[T]] match
    case _: ContinuousValue[T] => OpenPoint(t)
    case _: DiscreteValue[T]   => throw new IllegalArgumentException("discrete domains can't have open points")

  /**
    * Methods on Domain1D supportingType class instance for one-dimensional domains.
    */
  extension [T](domain: Domain1D[T])(using domainValueType: DomainValueLike[T])
    def isClosedOrUnbounded: Boolean =
      domain match
        case OpenPoint(_) => false
        case _            => true

    def pointsTo(end: Domain1D[T]): Iterable[Domain1D[T]] = domainValueType match
      case _: ContinuousValue[T] => Iterable.empty // undefined for continuous
      case discrete: DiscreteValue[T] =>
        def nearest(d: Domain1D[T]): Domain1D[T] = d match
          case Bottom => Point(discrete.minValue)
          case Top    => Point(discrete.maxValue)
          case _      => d

        val nearestStart: Option[Domain1D[T]] = Some(nearest(domain))
        val nearestEnd = nearest(end)
        Iterable.unfold(nearestStart):
          case None => None
          case Some(prevRemainingStart) =>
            val nextStart =
              if prevRemainingStart equiv nearestEnd then None
              else Some(prevRemainingStart.rightAdjacent)
            Some(prevRemainingStart, nextStart)

    def leftAdjacent: Domain1D[T] = domainValueType match
      case _: ContinuousValue[T] =>
        domain match
          case cp: Point[T] @unchecked     => OpenPoint(cp.value)
          case op: OpenPoint[T] @unchecked => Point(op.value)
          case noBound                     => noBound
      case discrete: DiscreteValue[T] =>
        domain match
          case point: Point[T] @unchecked =>
            discrete.predecessorOf(point.value).map(Point(_)).getOrElse(Bottom)
          case topOrBottom => topOrBottom

    def closeIfOpen: Domain1D[T] = domain match
      case op: OpenPoint[T] @unchecked => Point(op.value)
      case other                       => other

    def rightAdjacent: Domain1D[T] = domainValueType match
      case _: ContinuousValue[T] => domain.leftAdjacent // before and after are the same for continuous
      case discrete: DiscreteValue[T] =>
        domain match
          case point: Point[T] @unchecked => discrete.successorOf(point.value).map(Point(_)).getOrElse(Top)
          case topOrBottom                => topOrBottom

    /**
      * A special totally-ordered hash of a domain used for mapping intervals to box search trees in double space. If
      * `x1 < x2` (i.e., there are some number of `successorOf` functions that can be applied to `x1` to reach `x2`),
      * then `orderedHashOf(x1) ≤ orderedHashOf(x2)`.
      *
      * @note
      *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
      *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
      */
    def orderedHash: Double =
      domain match
        case Point(p)     => p.orderedHashValue
        case OpenPoint(p) => p.orderedHashValue
        case Top          => domainValueType.maxValue.orderedHashValue
        case Bottom       => domainValueType.minValue.orderedHashValue

    infix def isAdjacentTo(that: Domain1D[T]): Boolean =
      (domain isLeftAdjacentTo that) || (domain isRightAdjacentTo that)

    infix def isLeftAdjacentTo(that: Domain1D[T]): Boolean =
      that.leftAdjacent == domain

    infix def isRightAdjacentTo(that: Domain1D[T]): Boolean =
      that.rightAdjacent == domain

    // using default ordering to compare starts
    infix def afterStart(that: Domain1D[T]): Boolean =
      domain > that

    // using default ordering to compare starts
    infix def afterOrAtStart(that: Domain1D[T]): Boolean =
      domain >= that

    // using non-default ordering to compare ends
    infix def beforeEnd(that: Domain1D[T]): Boolean =
      given Ordering[Domain1D[T]] = Domain1D.endOrdering[T]
      domain < that

    // using non-default ordering to compare ends
    infix def beforeOrAtEnd(that: Domain1D[T]): Boolean =
      given Ordering[Domain1D[T]] = Domain1D.endOrdering[T]
      domain <= that

    // treat open > closed at the same point by using the default ordering
    infix def maxStart(that: Domain1D[T]): Domain1D[T] =
      domain max that

    // treat open < closed at the same point
    infix def maxEnd(that: Domain1D[T]): Domain1D[T] =
      List(domain, that).max(using Domain1D.endOrdering)

    // treat open > closed at the same point by using the default ordering
    infix def minStart(that: Domain1D[T]): Domain1D[T] = domain min that

    // treat open < closed at the same point
    infix def minEnd(that: Domain1D[T]): Domain1D[T] =
      List(domain, that).min(using Domain1D.endOrdering)

    def validIntervalStartWithEnd[D: DomainValueLike](end: Domain1D[T]): Boolean =
      (domain, end) match
        // strictly less than - excludes when values are equal
        case _ if (domain beforeEnd end) && (end afterStart domain) => true
        case (Point(cs), Point(ce)) if cs == ce => true // interval at a point is valid when bounds are closed,
        // case (Top, Top) | (Bottom, Bottom)      => true // or the special cases where it is an interval at top or
        // bottom
        case _ => false // otherwise invalid: e is before s, or they're equal with open bound(s)

  /**
    * This ordering sorts Bottoms and Tops correctly and leverages the domain value ordering for the data points in
    * between. When open and closed points are at the same value, sorts according to asStart parameter.
    *
    * @note
    *   Because `T` is covariant in the enum definition, this ordering will not get summoned automatically for specific
    *   instance values of the enum. For example, `Point(3).leftAdjacent equiv Point(2)` will summon the ordering since
    *   `Point(3).leftAdjacent` has the type `Domain1D[Int]`, but `Point(2) equiv Point(3).leftAdjacent` will not summon
    *   the ordering since `Point(2)` returns the type `Domain1D.Point[Int]`.
    *
    * One workaround is to safe cast as supertype, e.g., `(Point(2): Domain1D[Int]) equiv Point(3).leftAdjacent`
    */
  private def ordering[T](
    asStart: Boolean
  )(using domainValue: DomainValueLike[T]): Ordering[Domain1D[T]] =
    case (Bottom, Bottom)             => 0
    case (Bottom, _)                  => -1
    case (_, Bottom)                  => 1
    case (OpenPoint(x), OpenPoint(y)) => domainValue.compare(x, y)
    case (Point(x), Point(y))         => domainValue.compare(x, y)
    case (Point(x), OpenPoint(y)) =>
      val domainCompare = domainValue.compare(x, y)
      if domainCompare == 0 then if asStart then -1 else 1
      else domainCompare
    case (OpenPoint(x), Point(y)) =>
      val domainCompare = domainValue.compare(x, y)
      if domainCompare == 0 then if asStart then 1 else -1
      else domainCompare
    case (Top, Top) => 0
    case (_, Top)   => -1
    case (Top, _)   => 1

  /**
    * The default ordering for continuous domains is to treat them like interval starts. This will satisfy the use of
    * domains in the tree map returning them in the correct order. If you need ordering where it is more like the way
    * you would expect with interval ends, then use the non-default [[endOrdering]].
    * @note
    *   This difference is only important for continuous domains that leverage open points. The start and end ordering
    *   for discrete domain values are the same and can be used interchangeably.
    *
    * @tparam T
    *   domain value type
    * @return
    *   the default ordering of continuous domains
    */
  given startOrdering[T: DomainValueLike]: Ordering[Domain1D[T]] = ordering(asStart = true)

  /**
    * An alternate ordering for continuous domains that treats them like interval ends. If you need ordering where it is
    * more like the way you would expect with interval starts, then use the default [[startOrdering]].
    * @note
    *   This difference is only important for continuous domains that leverage open points. The start and end ordering
    *   for discrete domain values are the same and can be used interchangeably.
    */
  def endOrdering[T: DomainValueLike]: Ordering[Domain1D[T]] = ordering(asStart = false)

  /**
    * This allows a client to use domain values in methods requiring a domain element by implicitly converting them to a
    * `Point`. For example, a client can write `dataIn1D.getAt(Point(1))` or `dataIn1D.getAt(1)`. It is nice to not have
    * to wrap all domain values as `Point`s all the time, and cleaner to have one implicit conversion rather than a
    * multitude of overloaded methods (which are especially problematic when combined with default parameters).
    */
  given [T](using domainValue: DomainValueLike[T]): Conversion[T, Domain1D[T]] = Point(_)

  /**
    * Wrap 1D domain as a tuple so 1D doesn't have to be domain-like itself to be used as a domain
    */
  given [T]: Conversion[Domain1D[T], Domain.In1D[T]] = _ *: EmptyTuple

  /**
    * Other conversions take domain values `T => Domain1D[T]` and `Domain1D[T] => Domain.In1D[T]` (which is
    * `DomainLike`). But these conversions don't seem to stack, i.e., they aren't applied when a domain value of `T` is
    * available in a context requiring something that is `DomainLike`. So this one goes strait from a domain value `T`
    * directly to a domain, i.e., `T => Domain.In1D[T]`
    */
  given [T](using domainValue: DomainValueLike[T]): Conversion[T, Domain.In1D[T]] = Point(_) *: EmptyTuple
