package intervalidus

import intervalidus.collection.Coordinate

import java.time.LocalDate
import scala.language.implicitConversions

/**
  * Domain used in defining and operating on a discrete interval. It describes specific discrete data points as well as
  * the special "`Bottom`" and "`Top`" cases which conceptually lie below and above the finite range of data points
  * (logically below and above `minValue` and `maxValue` respectively). This also gives us a way to accommodate having a
  * predecessor or successor on a boundary, i.e., `maxValue.successor == Top` and `minValue.predecessor == Bottom`. This
  * allows predecessor and successor to be closed, where `Top.successor == Top.predecessor == Top`, and
  * `Bottom.predecessor == Bottom.successor == Bottom`.
  *
  * @tparam T
  *   expected to be a discrete value (i.e., `DiscreteValue[T]` should be given).
  */
enum DiscreteDomain1D[+T]:
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

import DiscreteDomain1D.{Bottom, Point, Top}

/**
  * Companion for the one-dimensional domain used in defining and operating on discrete intervals.
  */
object DiscreteDomain1D:

  /**
    * Type class instance for one-dimensional discrete domains.
    */
  given [T: DiscreteValue]: DiscreteDomainLike[DiscreteDomain1D[T]] with
    extension (domain: DiscreteDomain1D[T])
      override def isUnbounded: Boolean = domain match
        case Point(_) => false
        case _        => true

      override def toCodeLikeString: String =
        def codeFor(value: T): String = value match
          case d: LocalDate => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
          case _            => value.toString

        domain match
          case Bottom   => "Bottom"
          case Point(t) => s"Point(${codeFor(t)})"
          case Top      => "Top"

      override def asCoordinate: Coordinate =
        Coordinate(domain.orderedHash)

      override def successor: DiscreteDomain1D[T] = domain match
        case Point(p)    => p.successorValue.map(Point(_)).getOrElse(Top)
        case topOrBottom => topOrBottom

      override def predecessor: DiscreteDomain1D[T] = domain match
        case Point(p)    => p.predecessorValue.map(Point(_)).getOrElse(Bottom)
        case topOrBottom => topOrBottom

      /**
        * A special totally-ordered hash of a discrete domain used for mapping intervals to box search trees in double
        * space. If `x1 < x2` (i.e., there are some number of `successorOf` functions that can be applied to `x1` to
        * reach `x2`), then `orderedHashOf(x1) ≤ orderedHashOf(x2)`.
        *
        * @note
        *   having equal `orderedHashOf` results for different inputs is allowed, but represents a hash collision. If
        *   the `orderedHashOf` method has too many collisions, the performance of box search trees will suffer.
        */
      def orderedHash: Double =
        val discreteValue = summon[DiscreteValue[T]]
        domain match
          case Point(p) => p.orderedHashValue
          case Top      => discreteValue.maxValue.orderedHashValue
          case Bottom   => discreteValue.minValue.orderedHashValue

      /**
        * Cross this domain element with that domain element to arrive at a new two-dimensional domain element.
        *
        * @param that
        *   a one-dimensional domain element to be used as the vertical dimension.
        * @tparam T2
        *   discrete value type for that domain.
        * @return
        *   a new two-dimensional domain element with this as the horizontal component and that as the vertical
        *   component.
        */
      infix def x[T2: DiscreteValue](that: DiscreteDomain1D[T2]): DiscreteDomain2D[T, T2] =
        DiscreteDomain2D(domain, that)

  /**
    * This ordering sorts Bottoms and Tops correctly and leverages the discrete value ordering for the data points in
    * between.
    *
    * @note
    *   Because `T` is covariant in the enum definition, this ordering will not get summoned automatically for specific
    *   instance values of the enum. For example, `Point(3).predecessor equiv Point(2)` will summon the ordering since
    *   `Point(3).predecessor` has the type `DiscreteDomain1D[Int]`, but `Point(2) equiv Point(3).predecessor` will not
    *   summon the ordering since `Point(2)` returns the type `DiscreteDomain1D.Point[Int]`.
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
