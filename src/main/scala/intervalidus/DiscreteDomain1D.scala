package intervalidus

/**
  * Domain used in defining and operating on a discrete interval. It describes specific discrete data points as well as
  * the special "`Bottom`" and "`Top`" cases which conceptually lie below and above the finite range of data points
  * (logically below and above `minValue` and `maxValue` respectively). This also gives us a way to accommodate having a
  * predecessor or successor on a boundary, i.e., `maxValue.successor == Top` and `minValue.predecessor == Bottom`. Note
  * this allows predecessor and successor to be closed, where `Top.successor == Top.predecessor == Top`, and
  * `Bottom.predecessor == Bottom.successor == Bottom`.
  *
  * @tparam T
  *   expected to be a discrete value (i.e., `DiscreteValue[T]` should be given).
  */
enum DiscreteDomain1D[+T] extends DimensionalBase.DomainLike:
  /**
    * Smaller than smallest data point (like -∞)
    */
  case Bottom

  /**
    * A single data point in the finite range of this domain
    */
  case Point[P: DiscreteValue](p: P) extends DiscreteDomain1D[P]

  /**
    * Larger than largest data point (like +∞)
    */
  case Top

  override def toString: String = this match
    case Bottom   => "-∞"
    case Point(t) => t.toString
    case Top      => "+∞"

  override def toCodeLikeString: String = this match
    case Bottom   => "Bottom"
    case Point(t) => s"Point($t)"
    case Top      => "Top"

import DiscreteDomain1D.{Bottom, Point, Top}

/**
  * Extends [[DiscreteDomain1D]] with methods on a domain of a discrete value. Using an extension rather than defining
  * these methods on the enum itself resolves the issue of not being able to express the type class context bound
  * directly in the enum definition (because, there, `T` must be covariant to accommodate `Bottom` and `Top`).
  */
extension [T: DiscreteValue](domain1d: DiscreteDomain1D[T])

  /**
    * Successor of this, where `Bottom` and `Top` are their own successors, and the successor of `maxValue` is `Top`.
    *
    * @return
    *   successor of this
    */
  def successor: DiscreteDomain1D[T] = domain1d match
    case Point(p)    => p.successorValue.map(Point(_)).getOrElse(Top)
    case topOrBottom => topOrBottom

  /**
    * Predecessor of this, where `Bottom` and `Top` are their own predecessors, and the predecessor of `minValue` is
    * `Bottom`.
    *
    * @return
    *   successor of this
    */
  def predecessor: DiscreteDomain1D[T] = domain1d match
    case Point(p)    => p.predecessorValue.map(Point(_)).getOrElse(Bottom)
    case topOrBottom => topOrBottom

  /**
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo(interval: DiscreteInterval1D[T]): Boolean = interval contains domain1d

  /**
    * Cross this domain element with that domain element to arrive at a new two-dimensional domain element.
    *
    * @param that
    *   a one-dimensional domain element to be used in the vertical dimension.
    * @tparam T2
    *   discrete value type for that domain.
    * @return
    *   a new two-dimensional domain element with this as the horizontal component and that as the vertical component.
    */
  infix def x[T2: DiscreteValue](that: DiscreteDomain1D[T2]): DiscreteDomain2D[T, T2] =
    DiscreteDomain2D(domain1d, that)

  // equivalent symbolic method names

  /**
    * Same as [[belongsTo]]
    *
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  def ∈(interval: DiscreteInterval1D[T]): Boolean = domain1d belongsTo interval

object DiscreteDomain1D:
  /**
    * This ordering sorts Bottoms and Tops correctly and leverages the discrete value ordering for the data points in
    * between.
    *
    * Note that, because `T` is covariant in the enum definition, this ordering will not get summoned automatically for
    * specific instance values of the enum. For example, `Point(3).predecessor equiv Point(2)` will summon the ordering
    * since `Point(3).predecessor` has the type `DiscreteDomain1D[Int]`, but `Point(2) equiv Point(3).predecessor` will
    * not summon the ordering since `Point(2)` returns the type `DiscreteDomain1D.Point[Int]`.
    *
    * One workaround is to safe cast as supertype, e.g., `(Point(2): DiscreteDomain1D[Int]) equiv Point(3).predecessor`
    */
  given [T](using discreteValue: DiscreteValue[T]): Ordering[DiscreteDomain1D[T]] =
    case (Bottom, Bottom)     => 0
    case (Bottom, _)          => -1
    case (_, Bottom)          => 1
    case (Point(x), Point(y)) => discreteValue.compare(x, y)
    case (Top, Top)           => 0
    case (_, Top)             => -1
    case (Top, _)             => 1

  /**
    * This allows a client to use discrete values in methods requiring a discrete domain element by implicitly
    * converting them to a `Point`. For example, a client can write `dataIn1D.getAt(Point(1))` or `dataIn1D.getAt(1)`.
    * It is nice to not have to wrap all discrete values as `Point`s all the time, and cleaner to have one implicit
    * conversion rather than a multitude of overloaded methods (which are especially problematic when combined with
    * default parameters).
    */
  given [T](using DiscreteValue[T]): Conversion[T, DiscreteDomain1D[T]] = Point(_)
