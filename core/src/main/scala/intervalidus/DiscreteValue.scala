package intervalidus

/**
  * Type class template for a discrete value.
  *
  * @inheritdoc
  *
  * Unlike continuous values, discrete values also have predecessors and successors, which are properly defined on `x:
  * max < x < min`. Having predecessors and successors defined this way avoids issues where the natural successor
  * functions of the underlying types behave unexpectedly/inconsistently on the boundaries, e.g., how `Int.MaxValue + 1`
  * and `Int.MinValue - 1` silently wrap around to each other, vs. how both `LocalDate.MAX.plusDays(1)` and
  * `LocalDate.MIN.minusDays(1)` throw a `DateTimeException`.
  *
  * @tparam T
  *   a value that has discrete value behavior (e.g., `Int`)
  */
trait DiscreteValue[T] extends DomainValueLike[T]:
  override def bracePunctuation: String = ".."

  /**
    * Successor of a discrete value, when defined: only successorOf(maxValue) is not defined. See
    * [[https://en.wikipedia.org/wiki/Successor_function]].
    */
  def successorOf(x: T): Option[T]

  /**
    * Predecessor of a discrete value, when defined: only predecessorOf(minValue) is not defined. See
    * [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]].
    */
  def predecessorOf(x: T): Option[T]

  extension (lhs: T)
    /**
      * Successor of this discrete value, when defined: only maxValue.successorValue is not defined. See
      * [[https://en.wikipedia.org/wiki/Successor_function]].
      */
    def successorValue: Option[T] = successorOf(lhs)

    /**
      * Predecessor of this discrete value, when defined: only minValue.predecessorValue is not defined. See
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

    /**
      * Based on benchmarks, this seems to be too computationally expensive, where the benefit of reducing levels is
      * negligible.
      *
      * TODO: Find a better way (or just live with the "wasted" levels)? TODO: can we make this configurable?
      *
      * It is important not to over-sample the larger parts of numeric ranges as part of hashing, since our box search
      * trees in double space will be halving ranges. One would have to cut the [Int.MinValue..Int.MaxValue] box in half
      * 13 times to get to a subtree containing a box [0..524,288] and then another 19 levels to get down to 1: 32 total
      * levels (which makes sense for a 32 bit number). So for more practical use cases, there are a lot of levels to
      * descend before we reach "useful", "normal" ranges. In a quadtree, this would result in the creation of 13*3=39
      * leaves and 13 branches that are kind of worthless and just slow things down. Attempting to resolve this here by
      * using a logistic function to hash integers to doubles, which would increase resolution for numbers with smaller
      * magnitudes and decrease resolution for the larger. (See https://en.wikipedia.org/wiki/Sigmoid_function). This
      * one places all integers into a double range -1 to 1 following the logistic function. It is "normalized" in the
      * sense that negatives map to negatives, positives to positives, and zero to zero. The choice of a small k is to
      * keep the curve steep enough that integers with smaller magnitudes will be well represented in the result where
      * integers with very large magnitudes will not be.
      *   - All integers greater than about 3.6 million will result in 1.0, where some integers greater than about 2.4
      *     million will increasingly hash to the same value. Similarly, those less than -3.8 will result in -1.0 (not
      *     sure where the asymmetry comes from), where those less than about -2.5 million will increasing hash to the
      *     same value. (If the integers you use are regularly outside of this range, you may want to give a
      *     DiscreteValue[Int] different from this default.)
      *   - Splitting the double range [0..1] in half as [0..0.5] and [0.5..1], represents integers in the ranges
      *     [0..109,861] and [109,861..Int.MaxValue] (pretty non-linear: less than 0.01% of integers in left side).
      *   - Splitting the double range [0..0.5] in half would represent integer ranges of [0..51,083] and
      *     [51,083..109,861], which is pretty linear: 46.5% of integers in the range split are on the left. Subsequents
      *     splits will be around 25,131, 12,516, 6,252, 3,125, 1,562, 781, 390, 195, 97, 48, 24, 12, 6, and 3. So total
      *     splits from Int.MaxValue to get to 1 is only 15 instead of 32.
      */
//    private def normalizedSigmoid(x: Int): Double = {
//      val L: Double = 2.0 // The maximum value of the logistic function.
//      val k: Double = 0.00001 // The steepness of the curve.
//      val x0: Double = 0.0 // The x-value of the sigmoid's midpoint, where the function has its inflection point.
//      L / (1 + math.exp(-k * (x.toDouble - x0))) - 1.0
//    }

    override def orderedHashOf(x: Int): Double = x.toDouble // normalizedSigmoid(x)

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
    * Constructs a type class from a non-empty, distinct sequence of values. Useful when representing enums as discrete
    * values, for example:
    * {{{
    *   enum Color:
    *     case Red, Yellow, Green, Cyan, Blue, Magenta
    *   given DiscreteValue[Color] = DiscreteValue.fromSeq(Color.values)
    * }}}
    *
    * @param values
    *   a non-empty, distinct sequence of values
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
