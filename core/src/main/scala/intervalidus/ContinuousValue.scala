package intervalidus

/**
  * Type class for continuous values, which are:
  *   1. bounded, with both a lower bound ([[minValue]]) and an upper bound ([[maxValue]])
  *   1. totally ordered (extends [[Ordering]], requiring a [[compare]] method)
  *   1. mappable to a weakly monotonic double value (requires an [[orderedHashOf]] method that may have "collisions")
  *
  * Unlike a [[DiscreteValue]], a continuous value is not necessarily well-ordered. That is, there is no requirement for
  * having predecessors and successors defined. Although one can say that every digitized data structure is, by its very
  * nature, discrete, it is often easier to reason about some data types when treated as continuous. For example,
  * integer values and local dates are good candidates for being treated as discrete where double-precision float values
  * and local date-times are often better treated as continuous.
  *
  * See [[https://en.wikipedia.org/wiki/Bounded_set]], [[https://en.wikipedia.org/wiki/Maximum_and_minimum]],
  * [[https://en.wikipedia.org/wiki/Total_order]], [[https://en.wikipedia.org/wiki/Monotonic_function]], and
  * [[https://en.wikipedia.org/wiki/Well-order]].
  *
  * @tparam T
  *   a type with continuous value behavior (e.g., `Double`)
  */
trait ContinuousValue[T] extends DomainValueLike[T]:
  override def bracePunctuation: String = ", "

/**
  * Default continuous value type classes for common data types.
  */
object ContinuousValue:

  import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

  /**
    * Type class for doubles as continuous values.
    */
  given DoubleContinuousValue: ContinuousValue[Double] with
    override def compare(lhs: Double, rhs: Double): Int = lhs.compareTo(rhs)

    override def orderedHashOf(x: Double): Double = x

    override val maxValue: Double = Double.MaxValue

    override val minValue: Double = Double.MinValue

  /**
    * Type class for local date-times as continuous values.
    */
  given LocalDateTimeContinuousValue: ContinuousValue[LocalDateTime] with
    override def compare(lhs: LocalDateTime, rhs: LocalDateTime): Int = lhs.compareTo(rhs)

    // hashing uses millis, so this prevents long overflow when hashing
    // all date-times above and below these values will collide respectively
    private val minMilliInstant = Instant.ofEpochMilli(Long.MinValue)
    private val maxMilliInstant = Instant.ofEpochMilli(Long.MaxValue)

    override def orderedHashOf(x: LocalDateTime): Double = x.toInstant(ZoneOffset.UTC) match
      case i if i.isBefore(minMilliInstant) => Long.MinValue.toDouble
      case i if i.isAfter(maxMilliInstant)  => Long.MaxValue.toDouble
      case i                                => i.toEpochMilli.toDouble

    override val maxValue: LocalDateTime = LocalDateTime.MAX

    override val minValue: LocalDateTime = LocalDateTime.MIN

  /**
    * Type class for integers as continuous values (even though they are discrete).
    */
  given IntContinuousValue: ContinuousValue[Int] with
    override def compare(lhs: Int, rhs: Int): Int = lhs.compareTo(rhs)

    override def orderedHashOf(x: Int): Double = x.toDouble

    override val maxValue: Int = Int.MaxValue

    override val minValue: Int = Int.MinValue

  /**
    * Type class for long integers as continuous values (even though they are discrete).
    */
  given LongContinuousValue: ContinuousValue[Long] with
    override def compare(lhs: Long, rhs: Long): Int = lhs.compareTo(rhs)

    override def orderedHashOf(x: Long): Double = x.toDouble

    override val maxValue: Long = Long.MaxValue

    override val minValue: Long = Long.MinValue

  /**
    * Type class for local dates as continuous values (even though they are discrete).
    */
  given LocalDateContinuousValue: ContinuousValue[LocalDate] with
    override def compare(lhs: LocalDate, rhs: LocalDate): Int = lhs.compareTo(rhs)

    override def orderedHashOf(x: LocalDate): Double = x.toEpochDay.toDouble

    override val maxValue: LocalDate = LocalDate.MAX

    override val minValue: LocalDate = LocalDate.MIN

  /**
    * Constructs a type class from a non-empty, distinct sequence of values. Useful when representing enums as
    * continuous values (even though they are discrete), for example:
    * {{{
    *   enum Color:
    *     case Red, Yellow, Green, Cyan, Blue, Magenta
    *   given ContinuousValue[Color] = ContinuousValue.fromSeq(Color.values)
    * }}}
    *
    * @param values
    *   a non-empty, distinct sequence of values
    * @tparam E
    *   type for this continuous value
    * @return
    *   a new continuous value type class for `E`
    */
  def fromSeq[E](
    values: IndexedSeq[E]
  ): ContinuousValue[E] = new ContinuousValue[E]:
    require(values.nonEmpty && values == values.distinct, "values must be non-empty and distinct")

    override val minValue: E = values.head

    override val maxValue: E = values.last

    override def compare(lhs: E, rhs: E): Int = values.indexOf(lhs).compareTo(values.indexOf(rhs))

    override def orderedHashOf(x: E): Double = values.indexOf(x).toDouble
