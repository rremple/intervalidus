package intervalidus

/**
  * Template for a type class with operations on a discrete value. A discrete value is
  *   1. finite, with a max and min value (think Int with its MaxValue and MinValue)
  *   1. totally ordered (this type class extends the [[Ordering]] type class, requiring a compare method).
  *   1. have predecessor and successor functions, which are properly defined on x: max < x < min.
  *
  * Having predecessors and successors defined this way avoids issues where the natural successor functions of the
  * underlying types behave unexpectedly/inconsistently on the boundaries, e.g., how Int.MaxValue + 1 and Int.MinValue -
  * 1 silently wrap around to each other, vs. how both LocalDate.MAX.plusDays(1) and LocalDate.MIN.minusDays(1) throw a
  * DateTimeException.
  *
  * @tparam T
  *   a value that has discrete value behavior (e.g., Int)
  */
trait DiscreteValue[T] extends Ordering[T]:
  /**
    * Maximum discrete value
    */
  def maxValue: T

  /**
    * Minimum discrete value
    */
  def minValue: T

  /**
    * Successor of a discrete value, when defined: only successorOf(maxValue) is not defined.
    */
  def successorOf(x: T): Option[T]

  /**
    * Predecessor of a discrete value, when defined: only predecessorOf(minValue) is not defined.
    */
  def predecessorOf(x: T): Option[T]

  extension (lhs: T)
    /**
      * Successor of this discrete value, when defined: only maxValue.successor is not defined.
      */
    def successor: Option[T] = successorOf(lhs)

    /**
      * Predecessor of this discrete value, when defined: only minValue.predecessor is not defined.
      */
    def predecessor: Option[T] = predecessorOf(lhs)

object DiscreteValue:

  /**
    * Type class for integers as discrete values.
    */
  given IntDiscreteValue: DiscreteValue[Int] with
    override def compare(lhs: Int, rhs: Int): Int = lhs.compareTo(rhs)

    override def successorOf(x: Int): Option[Int] = if x < maxValue then Some(x + 1) else None

    override def predecessorOf(x: Int): Option[Int] = if x > minValue then Some(x - 1) else None

    override val maxValue: Int = Int.MaxValue

    override val minValue: Int = Int.MinValue

  /**
    * Type class for long integers as discrete values.
    */
  given LongDiscreteValue: DiscreteValue[Long] with
    override def compare(lhs: Long, rhs: Long): Int = lhs.compareTo(rhs)

    override def successorOf(x: Long): Option[Long] = if x < maxValue then Some(x + 1) else None

    override def predecessorOf(x: Long): Option[Long] = if x > minValue then Some(x - 1) else None

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

    override val maxValue: LocalDate = LocalDate.MAX

    override val minValue: LocalDate = LocalDate.MIN

  import java.math.BigInteger

  /**
    * Type class for big integers as discrete values.
    *
    * @note
    *   per https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html "BigInteger constructors and operations
    *   throw ArithmeticException when the result is out of the supported range of -2^Integer.MAX_VALUE (exclusive) to
    *   +2^Integer.MAX_VALUE (exclusive)."
    */
  given BigIntegerDiscreteValue: DiscreteValue[BigInteger] with
    override def compare(lhs: BigInteger, rhs: BigInteger): Int = lhs.compareTo(rhs)

    override def successorOf(x: BigInteger): Option[BigInteger] =
      if x.equals(maxValue) then None else Some(x.add(BigInteger.valueOf(1)))

    override def predecessorOf(x: BigInteger): Option[BigInteger] =
      if x.equals(minValue) then None else Some(x.add(BigInteger.valueOf(-1)))

    // TODO: how expensive is this?
    override lazy val maxValue: BigInteger = BigInteger.valueOf(2).pow(Int.MaxValue - 1).add(BigInteger.valueOf(-1))

    override lazy val minValue: BigInteger = maxValue.negate
