package intervalidus

import java.time.{LocalDate, LocalDateTime}
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * A one-dimensional domain based on an underlying domain value type. One-dimensional domains are used to define the
  * boundaries of one-dimensional intervals, and tuples of one-dimensional domains are used to define the boundaries of
  * multidimensional intervals. It describes specific data points in the domain value range as well as the special
  * `Bottom` and `Top` cases which conceptually lie below and above this finite range of data points (logically below
  * and above `minValue` and `maxValue` respectively). Domains can be based on domain values that are discrete or
  * continuous. When continuous, a boundary point can either be open or closed, where discrete points must always be
  * closed.
  *
  * This also gives a way to completely describe adjacency.
  *   - When domain values are discrete, the left and right adjacent domains of a point are the respective predecessors
  *     and successors of the domain value. This also gives us a way to accommodate having predecessors/successors on
  *     the boundaries, i.e., `domain(maxValue).rightAdjacent == Top` and `domain(minValue).leftAdjacent == Bottom`.
  *   - When domain values are continuous, the left and right adjacent domains are always the same: open if the point is
  *     closed and closed if the point is open.
  *   - In both discrete and continuous domains, `Top` and `Bottom` are considered self-adjacent.
  *
  * @tparam D
  *   expected to be a domain value (i.e., `DomainValueLike[D]` should be given).
  */
sealed trait Domain1D[+D]:

  /**
    * Methods on Domain1D similar to (but mostly a subset of) DomainLike methods.
    */

  import intervalidus.Domain1D.{Bottom, OpenPoint, Point, Top, endOrdering, startOrdering}

  /**
    * Alternative to toString for something that looks more like code
    */
  def toCodeLikeString: String

  /**
    * True if unbounded: Top or Bottom.
    */
  def isUnbounded: Boolean = true // default for Top and Bottom, overridden in Point and OpenPoint

  /**
    * Test if the domain is closed or unbounded.
    */
  def isClosedOrUnbounded: Boolean = true // default for Top, Bottom, and Point, overridden in OpenPoint

  /**
    * If this is an open points, convert it to a closed points at the same value. Unbounded and already closed points
    * are left unchanged.
    */
  def closeIfOpen: Domain1D[D] = this // default for Top, Bottom, and Point, overridden in OpenPoint

  /**
    * Every possible point bounded by this domain and the end domain.
    *
    * @note
    *   if this domain is continuous, no points are returned.
    */
  def pointsTo[T >: D](
    end: Domain1D[T]
  )(using domainValue: DomainValueLike[T]): Iterable[Domain1D[T]] = domainValue match
    case _: ContinuousValue[T]      => Iterable.empty // undefined for continuous
    case discrete: DiscreteValue[T] =>
      def nearest(d: Domain1D[T]): Domain1D[T] = d match
        case Bottom => Point(discrete.minValue)
        case Top    => Point(discrete.maxValue)
        case _      => d

      val nearestStart: Option[Domain1D[T]] = Some(nearest(this))
      val nearestEnd = nearest(end)
      Iterable.unfold(nearestStart):
        case None                     => None
        case Some(prevRemainingStart) =>
          val nextStart =
            if prevRemainingStart equiv nearestEnd then None
            else Some(prevRemainingStart.rightAdjacent)
          Some(prevRemainingStart, nextStart)

  /**
    * Domain adjacent to this domain from the "left", where `Bottom` and `Top` are considered self-adjacent. For
    * discrete domains, this is the predecessor, where the left adjacent of `minValue` is `Bottom` -- see
    * [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]]. For continuous domains, this maps open
    * points to closed ones, and closed points to open ones (right and left adjacent domains are the same).
    *
    * @return
    *   left complement of this
    */
  def leftAdjacent[T >: D](using domainValue: DomainValueLike[T]): Domain1D[T] = domainValue match
    case _: ContinuousValue[T] =>
      this match
        case Point(value)     => OpenPoint(value: T)
        case OpenPoint(value) => Point(value: T)
        case topOrBottom      => topOrBottom
    case discrete: DiscreteValue[T] =>
      this match
        case Point(value) => discrete.predecessorOf(value).map(Point(_)).getOrElse(Bottom)
        case topOrBottom  => topOrBottom

  /**
    * Domain adjacent to this domain from the "right", where `Bottom` and `Top` are considered self-adjacent. For
    * discrete domains, this is the successor, where the right adjacent of `maxValue` is `Top` -- see
    * [[https://en.wikipedia.org/wiki/Successor_function]]. For continuous domains, this maps open points to closed
    * ones, and closed points to open ones (right and left adjacent domains are the same).
    *
    * @return
    *   right complement of this
    */
  def rightAdjacent[T >: D](using domainValue: DomainValueLike[T]): Domain1D[T] = domainValue match
    case _: ContinuousValue[T]      => leftAdjacent // left and right are the same for continuous
    case discrete: DiscreteValue[T] =>
      this match
        case Point(value) => discrete.successorOf(value).map(Point(_)).getOrElse(Top)
        case topOrBottom  => topOrBottom

  /**
    * Is this start after that start, using default ordering to compare starts?
    */
  infix def afterStart[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = startOrdering.gt(this, that)

  /**
    * Is this start after or equal to that start, using default ordering to compare starts?
    */
  infix def afterOrAtStart[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = startOrdering.gteq(this, that)

  /**
    * Is this end before that end, using non-default ordering to compare ends?
    */
  infix def beforeEnd[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = endOrdering[T].lt(this, that)

  /**
    * Is this end before or equal to that end, using non-default ordering to compare ends?
    */
  infix def beforeOrAtEnd[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = endOrdering[T].lteq(this, that)

  /**
    * Find the max domain of two starts, treating open > closed at the same point by using the default ordering.
    */
  infix def maxStart[T >: D: DomainValueLike](that: Domain1D[T]): Domain1D[T] = startOrdering.max(this, that)

  /**
    * Find the max domain of two ends, treating open < closed at the same point by using the non-default ordering.
    */
  infix def maxEnd[T >: D: DomainValueLike](that: Domain1D[T]): Domain1D[T] = endOrdering.max(this, that)

  /**
    * Find the min domain of two starts, treating open > closed at the same point by using the default ordering.
    */
  infix def minStart[T >: D: DomainValueLike](that: Domain1D[T]): Domain1D[T] = startOrdering.min(this, that)

  /**
    * Find the min domain of two ends, treating open < closed at the same point by using the non-default ordering.
    */
  infix def minEnd[T >: D: DomainValueLike](that: Domain1D[T]): Domain1D[T] = endOrdering.min(this, that)

  /**
    * Cross this domain with that domain to arrive at a new two-dimensional domain tuple.
    *
    * @param that
    *   a one-dimensional domain to be used as the second (vertical) dimension component.
    * @tparam X
    *   domain value type for that domain.
    * @return
    *   a new two-dimensional domain tuple with this as the head (horizontal) dimension component and that as the second
    *   (vertical) dimension component.
    */
  infix def x[X: DomainValueLike](that: Domain1D[X]): Domain.In2D[D, X] = (this, that)

  /**
    * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo[T >: D: DomainValueLike](interval: Interval1D[T]): Boolean = interval contains this

  /*
   * Specific to one-dimensional domains.
   */

  /**
    * Returns this specialized one-dimensional domain as a general domain tuple.
    */
  def tupled[T >: D: DomainValueLike]: Domain.In1D[T] = Domain.in1D[T](this)

  /**
    * A special totally ordered hash of this domain used for mapping intervals to box search trees in double space. If
    * `x1 < x2` (based on default ordering), then `orderedHashOf(x1) ≤ orderedHashOf(x2)`.
    *
    * @note
    *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If the
    *   `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
    */
  def orderedHashFixed[T >: D](using domainValue: DomainValueLike[T]): Double = this match
    case Point(p)     => p.orderedHashValue
    case OpenPoint(p) => p.orderedHashValue
    case Top          => domainValue.maxValue.orderedHashValue
    case Bottom       => domainValue.minValue.orderedHashValue

  /**
    * Similar to [[orderedHashFixed]], this is a special totally ordered hash of this domain used for mapping intervals
    * to box search trees in double space. We avoid representing unbounded data with fixed double value that have very
    * large magnitudes, which are proven to be a performance issue.
    */
  def orderedHashUnfixed: Option[Double] = None // default for Top and Bottom, overridden in Point and OpenPoint

  /**
    * Left brace for interval notation. Square when closed, and paren otherwise.
    */
  def leftBrace: String = "(" // default for Top, Bottom, and OpenPoint, overridden in Point

  /**
    * Right brace for interval notation. Square when closed, and paren otherwise.
    */
  def rightBrace: String = ")" // default for Top, Bottom, and OpenPoint, overridden in Point

  /**
    * Tests if this domain is adjacent to that domain on the left.
    */
  infix def isLeftAdjacentTo[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = that.leftAdjacent == this

  /**
    * Tests if this domain is adjacent to that domain on the right.
    */
  infix def isRightAdjacentTo[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = that.rightAdjacent == this

  /**
    * Tests if this domain is adjacent to that domain on the left or right.
    */
  infix def isAdjacentTo[T >: D: DomainValueLike](that: Domain1D[T]): Boolean =
    (this isLeftAdjacentTo that) || (this isRightAdjacentTo that)

  /*
   * Equivalent symbolic method names
   */

  /**
    * Same as [[belongsTo]]
    *
    * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  def ∈[T >: D: DomainValueLike](interval: Interval1D[T]): Boolean = this belongsTo interval

  /**
    * Same as [[isAdjacentTo]]
    *
    * Tests if this domain is adjacent to that domain on the left or right.
    */
  infix def ~[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = this isAdjacentTo that

  /**
    * Same as [[isLeftAdjacentTo]]
    *
    * Tests if this domain is adjacent to that domain on the left.
    */
  infix def ~>[T >: D: DomainValueLike](that: Domain1D[T]): Boolean = this isLeftAdjacentTo that

/**
  * Companion for the one-dimensional domain used in defining and operating on intervals.
  */
object Domain1D:
  /**
    * Larger than the largest data point (like +∞)
    */
  case object Top extends Domain1D[Nothing]:
    override def toCodeLikeString: String = "Top"
    override def toString: String = "+∞"

  /**
    * Smaller than the smallest data point (like -∞)
    */
  case object Bottom extends Domain1D[Nothing]:
    override def toCodeLikeString: String = "Bottom"
    override def toString: String = "-∞"

  /**
    * A single closed data point in the finite range of this domain
    */
  case class Point[P: DomainValueLike](value: P) extends Domain1D[P]:
    // override defaults
    override def orderedHashUnfixed: Option[Double] = Some(value.orderedHashValue)
    override def isUnbounded: Boolean = false
    override def leftBrace: String = "["
    override def rightBrace: String = "]"

    override def toCodeLikeString: String = s"Point(${codeLikeValue(value)})"
    override def toString: String = value.toString

  /**
    * A single open data point in the finite range of this domain. This is suitable for the start or end of an interval
    * over continuous values. This case is not used in intervals over discrete values.
    */
  case class OpenPoint[P: DomainValueLike](value: P) extends Domain1D[P]:
    // override defaults
    override def orderedHashUnfixed: Option[Double] = Some(value.orderedHashValue)
    override def isUnbounded: Boolean = false
    override def isClosedOrUnbounded: Boolean = false
    override def closeIfOpen: Domain1D[P] = Point(value)

    override def toCodeLikeString: String = s"OpenPoint(${codeLikeValue(value)})"
    override def toString: String = value.toString

  /**
    * Code-like strings for some value, with special handling for dates and date-times.
    *
    * @param value
    *   any value
    * @tparam T
    *   type of the value
    * @return
    *   a code-like string constructing the value
    */
  def codeLikeValue[T](value: T): String = value match
    case d: LocalDate     => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
    case d: LocalDateTime =>
      s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})" +
        s".atTime(${d.getHour},${d.getMinute},${d.getSecond},${d.getNano})"
    case _ => value.toString

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
  def domain[T: DomainValueLike](t: T): Domain1D[T] = Point(t)

  /**
    * Construct a domain point (open) based on a domain value.
    *
    * @throws IllegalArgumentException
    *   if applied to a discrete value
    * @param t
    *   the domain value
    * @tparam T
    *   a domain value type
    * @return
    *   the open domain point of the domain value
    */
  def open[T](t: T)(using domainValue: DomainValueLike[T]): Domain1D[T] = domainValue match
    case _: ContinuousValue[T] => OpenPoint(t)
    case _: DiscreteValue[T]   => throw IllegalArgumentException("discrete domains can't have open points")

  /**
    * This ordering sorts Bottoms and Tops correctly and leverages the domain value ordering for the data points in
    * between. When open and closed points are at the same value, sorts according to asStart parameter.
    *
    * @note
    *   This ordering will not get summoned automatically for specific instance values of the trait. For example,
    *   `Point(3).leftAdjacent equiv Point(2)` will summon the ordering since `Point(3).leftAdjacent` has the type
    *   `Domain1D[Int]`, but `Point(2) equiv Point(3).leftAdjacent` will not summon the ordering since `Point(2)`
    *   returns the type `Domain1D.Point[Int]`.
    *
    * One workaround is to use methods that construct as the supertype, e.g., `domain(2) equiv domain(3).leftAdjacent`
    */
  private def ordering[T](
    asStart: Boolean
  )(using domainValue: DomainValueLike[T]): Ordering[Domain1D[T]] =
    case (Bottom, Bottom)             => 0
    case (Bottom, _)                  => -1
    case (_, Bottom)                  => 1
    case (OpenPoint(x), OpenPoint(y)) => domainValue.compare(x, y)
    case (Point(x), Point(y))         => domainValue.compare(x, y)
    case (Point(x), OpenPoint(y))     =>
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
    * The default ordering for one-dimensional domains is to treat them like interval starts. This will satisfy the use
    * of domains in the tree map returning them in the correct order. If you need ordering where it is more like the way
    * you would expect with interval ends, then use the non-default [[endOrdering]].
    * @note
    *   This difference is only important for continuous domains that leverage open points. The start and end ordering
    *   for discrete domain values are the same and can be used interchangeably.
    *
    * @tparam T
    *   domain value type
    * @return
    *   the default ordering of one-dimensional domains
    */
  given startOrdering[T: DomainValueLike]: Ordering[Domain1D[T]] = ordering(asStart = true)

  /**
    * An alternate ordering for one-dimensional domains that treats them like interval ends. If you need ordering where
    * it is more like the way you would expect with interval starts, then use the default [[startOrdering]].
    * @note
    *   This difference is only important for continuous domains that leverage open points. The start and end ordering
    *   for discrete domain values are the same and can be used interchangeably.
    */
  def endOrdering[T: DomainValueLike]: Ordering[Domain1D[T]] = ordering(asStart = false)

  /**
    * This allows a client to use domain values in methods requiring a one-dimensional domain by implicitly converting
    * them to a `Point`. For example, a client can write `dataIn1D.getAt(Point(1))` or `dataIn1D.getAt(1)`. It is nice
    * to not have to wrap all domain values as `Point`s all the time, and cleaner to have one implicit conversion rather
    * than a multitude of overloaded methods (which are especially problematic when combined with default parameters).
    */
  given [T: DomainValueLike]: Conversion[T, Domain1D[T]] = domain

  /**
    * Wrap 1D domain as a tuple so 1D doesn't have to be domain-like itself to be used as a domain
    */
  given [T: DomainValueLike]: Conversion[Domain1D[T], Domain.In1D[T]] = _.tupled

  /**
    * Other conversions take domain values `T => Domain1D[T]` and `Domain1D[T] => Domain.In1D[T]` (which is
    * [[DomainLike]]). But these conversions don't seem to stack. I.e., they aren't applied when a domain value of `T`
    * is available in a context requiring something that is [[DomainLike]]. So this converts strait from a domain value
    * `T` directly to a domain, i.e., `T => Domain.In1D[T]`
    */
  given [T: DomainValueLike]: Conversion[T, Domain.In1D[T]] = domain(_).tupled
