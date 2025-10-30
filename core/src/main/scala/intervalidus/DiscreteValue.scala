package intervalidus

/**
  * Type class for discrete values, which are:
  *   1. bounded, with both a lower bound ([[minValue]]) and an upper bound ([[maxValue]])
  *   1. totally ordered (extends [[Ordering]], requiring a [[compare]] method)
  *   1. mappable to a weakly monotonic double value (requires an [[orderedHashOf]] method that may have "collisions")
  *   1. well ordered, having both a [[predecessorOf]] and [[successorOf]] function (unlike a [[ContinuousValue]]).
  *
  * The predecessor must only be defined for values strictly greater than `minValue` and the successor only for values
  * strictly less than `maxValue`. Having the predecessor and successor definitions limited in this way avoids issues
  * where related methods of the underlying data types behave unexpectedly/inconsistently on the boundaries. For
  * example, `Int.MaxValue + 1` and `Int.MinValue - 1` silently wrap around to each other, whereas both
  * `LocalDate.MAX.plusDays(1)` and `LocalDate.MIN.minusDays(1)` throw a `DateTimeException`.
  *
  * See [[https://en.wikipedia.org/wiki/Bounded_set]], [[https://en.wikipedia.org/wiki/Maximum_and_minimum]],
  * [[https://en.wikipedia.org/wiki/Total_order]], [[https://en.wikipedia.org/wiki/Monotonic_function]], and
  * [[https://en.wikipedia.org/wiki/Well-order]].
  *
  * @tparam T
  *   a type with discrete value behavior (e.g., `Int`)
  */
trait DiscreteValue[T] extends DomainValueLike[T]:
  override def bracePunctuation: String = ".."

  /**
    * Successor of a discrete value, when defined: only `successorOf(maxValue)` is not defined. See
    * [[https://en.wikipedia.org/wiki/Successor_function]].
    */
  def successorOf(x: T): Option[T]

  /**
    * Predecessor of a discrete value, when defined: only `predecessorOf(minValue)` is not defined. See
    * [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]].
    */
  def predecessorOf(x: T): Option[T]

  extension (lhs: T)
    /**
      * Successor of this discrete value, when defined: only `maxValue.successorValue` is not defined. See
      * [[https://en.wikipedia.org/wiki/Successor_function]].
      */
    def successorValue: Option[T] = successorOf(lhs)

    /**
      * Predecessor of this discrete value, when defined: only `minValue.predecessorValue` is not defined. See
      * [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]].
      */
    def predecessorValue: Option[T] = predecessorOf(lhs)

/**
  * Default discrete value type classes for common data types.
  */
object DiscreteValue:

  /**
    * Type class for integers as discrete values.
    */
  given IntDiscreteValue: DiscreteValue[Int] with
    override def compare(lhs: Int, rhs: Int): Int = lhs.compareTo(rhs)

    override def successorOf(x: Int): Option[Int] = if x < maxValue then Some(x + 1) else None

    override def predecessorOf(x: Int): Option[Int] = if x > minValue then Some(x - 1) else None

    override def orderedHashOf(x: Int): Double = x.toDouble

    override val maxValue: Int = Int.MaxValue

    override val minValue: Int = Int.MinValue

  /**
    * Type class for long integers as discrete values.
    */
  given LongDiscreteValue: DiscreteValue[Long] with
    override def compare(lhs: Long, rhs: Long): Int = lhs.compareTo(rhs)

    override def successorOf(x: Long): Option[Long] = if x < maxValue then Some(x + 1) else None

    override def predecessorOf(x: Long): Option[Long] = if x > minValue then Some(x - 1) else None

    override def orderedHashOf(x: Long): Double = x.toDouble

    override val maxValue: Long = Long.MaxValue

    override val minValue: Long = Long.MinValue

  import java.time.LocalDate

  /**
    * Type class for local dates as discrete values.
    */
  given LocalDateDiscreteValue: DiscreteValue[LocalDate] with
    override def compare(lhs: LocalDate, rhs: LocalDate): Int = lhs.compareTo(rhs)

    override def successorOf(x: LocalDate): Option[LocalDate] =
      if x.isEqual(maxValue) then None else Some(x.plusDays(1))

    override def predecessorOf(x: LocalDate): Option[LocalDate] =
      if x.isEqual(minValue) then None else Some(x.minusDays(1))

    override def orderedHashOf(x: LocalDate): Double = x.toEpochDay.toDouble

    override val maxValue: LocalDate = LocalDate.MAX

    override val minValue: LocalDate = LocalDate.MIN

  import java.math.BigInteger

  /**
    * Type class for big integers as discrete values.
    *
    * @note
    *   per [[https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html]] "BigInteger constructors and
    *   operations throw ArithmeticException when the result is out of the supported range of
    *   -2<sup>Integer.MAX_VALUE</sup> (exclusive) to +2<sup>Integer.MAX_VALUE</sup> (exclusive)." However, on the
    *   practical side of things, this number is so large that just rendering it as a string can easily cause OOM in the
    *   string builder! So maybe override this and pick your own "biggest" BigInteger based on how you plan to use it
    *   rather than this theoretically biggest one...
    */
  given BigIntegerDiscreteValue: DiscreteValue[BigInteger] with
    override def compare(lhs: BigInteger, rhs: BigInteger): Int = lhs.compareTo(rhs)

    override def successorOf(x: BigInteger): Option[BigInteger] =
      if x.equals(maxValue) then None else Some(x.add(BigInteger.valueOf(1)))

    override def predecessorOf(x: BigInteger): Option[BigInteger] =
      if x.equals(minValue) then None else Some(x.add(BigInteger.valueOf(-1)))

    override def orderedHashOf(x: BigInteger): Double = x.doubleValue()

    override lazy val maxValue: BigInteger = BigInteger.valueOf(2).pow(Int.MaxValue - 1).add(BigInteger.valueOf(-1))

    override lazy val minValue: BigInteger = maxValue.negate

  /**
    * Constructs a type class from a non-empty, distinct, and indexed sequence of values, for example:
    * {{{
    *   case class Num(i: Int)
    *   object Num:
    *     val values = IndexedSeq(Num(1), Num(2), Num(3), Num(5), Num(8), Num(13))
    *   given DiscreteValue[Num] = DiscreteValue.fromSeq(Num.values)
    * }}}
    * Useful when representing enums as discrete values when auto-derivation is not possible or not desired, for
    * example:
    * {{{
    *   enum Color:
    *     case Red, Yellow, Green, Cyan, Blue, Magenta
    *   given DiscreteValue[Color] = DiscreteValue.fromSeq(Color.values)
    * }}}
    *
    * @param values
    *   a non-empty, distinct, indexed sequence of values
    * @tparam E
    *   type for this discrete value
    * @return
    *   a new discrete value type class for `E`
    */
  def fromSeq[E](
    values: IndexedSeq[E]
  ): DiscreteValue[E] = new DiscreteValue[E]:
    require(values.nonEmpty && values == values.distinct, "values must be non-empty and distinct")

    override val minValue: E = values.head

    override val maxValue: E = values.last

    override def predecessorOf(x: E): Option[E] =
      if x == minValue then None else Some(values(values.indexOf(x) - 1))

    override def successorOf(x: E): Option[E] =
      if x == maxValue then None else Some(values(values.indexOf(x) + 1))

    override def compare(lhs: E, rhs: E): Int = values.indexOf(lhs).compareTo(values.indexOf(rhs))

    override def orderedHashOf(x: E): Double = values.indexOf(x).toDouble

  /**
    * Auto-derives a DiscreteValue type class from an enum type, for example:
    * {{{
    *   enum Color derives DiscreteValue:
    *     case Red, Yellow, Green, Cyan, Blue, Magenta
    * }}}
    *
    * @tparam E
    *   the enum type
    * @return
    *   a discrete value type class based on the enum values
    */
  inline def derived[E <: scala.reflect.Enum]: DiscreteValue[E] =
    fromSeq(EnumMacro.enumValues[E].toIndexedSeq)
