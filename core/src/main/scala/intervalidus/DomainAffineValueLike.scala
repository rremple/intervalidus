package intervalidus

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Try

/**
  * A domain value that, in addition to being bounded, totally ordered, and having an ordered hash, has:
  *   - a [[Displacement]] type which represents the signed distance between two domain values (which can be obtained
  *     with the [[displacement]] method). Displacements are ordered, have a zero element, and can be negated.
  *   - a [[Scalar]] type by which a displacement can be scaled linearly. Scalars are ordered, have a zero element, have
  *     an identity element, and can be negated.
  *   - a set of methods for combining scalars, displacements, and domain values. For example, the [[displace]] method
  *     calculates a new domain value based on some domain value and a displacement
  *
  * @tparam T
  *   a type with continuous or discrete affine value behavior (e.g., `Double`)
  */
trait DomainAffineValueLike[T] extends DomainValueLike[T]:
  /**
    * If the result fails, returns None.
    */
  protected def safe[S](result: => S): Option[S] =
    Try(result).toOption // non-fatal exceptions are typically DateTimeException or ArithmeticException

  /**
    * The result of operations on doubles can result in non-numbers and infinities. This method filters those out,
    * returning None for these invalid numbers.
    */
  protected def validateDouble(d: Double): Option[Double] =
    if d.isNaN || d.isInfinite then None else Some(d)

  /**
    * The displacement type should have symmetric valid ranges above and below zero. Displacments form a mathematical
    * group, having an identity (zero), a combine (add), and an inverse (negate) function. However a more generic type
    * class is not used because add must be "safe" in that overflows, or any result falling outside of a specific valid
    * value range, are returned as None
    */
  type Displacement

  /**
    * The scalar type allows the displacement to be scaled (multiplied).
    */
  type Scalar

  /*
   * Related to only the displacement and scalar types
   */

  /**
    * Compare two displacements.
    */
  def compareDisplacements(x: Displacement, y: Displacement): Int

  /**
    * Displacements are ordered using their compare operator
    */
  given displacementOrdering: Ordering[Displacement] = compareDisplacements(_, _)

  /**
    * Displacement zero value.
    */
  def zeroDisplacement: Displacement

  /**
    * Negate a displacement, where offset plus negate(offset) == zero. Should always be defined because we assume valid
    * displacement ranges are symmetric above and below zero.
    */
  def negateDisplacement(offset: Displacement): Displacement

  /**
    * Compare two displacements.
    */
  def compareScalars(x: Scalar, y: Scalar): Int

  /**
    * Scalars are ordered using their compare operator
    */
  given scalarOrdering: Ordering[Scalar] = compareScalars(_, _)

  /**
    * Scalar zero value. Scalars less than zero reflect a displacement.
    */
  def zeroScalar: Scalar

  /**
    * Identity scalar, where a displacement scaled by this amount does not change.
    */
  def identityScalar: Scalar

  /**
    * Negate a scalar, where scalar plus negate(scalar) == zero. Should always be defined because we assume valid scalar
    * ranges are symmetric above and below zero.
    */
  def negateScalar(scalar: Scalar): Scalar

  /**
    * Add two displacements. This can overflow, e.g., both are max displacements. If it overflows, it returns None.
    */
  def add(offset1: Displacement, offset2: Displacement): Option[Displacement]

  /**
    * Scale a displacement. This can overflow, e.g., scale up a max displacements. If it overflows, it returns None.
    * Integer displacement types are rounded when scaled. See [[https://en.wikipedia.org/wiki/Scaling_(geometry)]]
    */
  def scale(offset: Displacement, scaledBy: Scalar): Option[Displacement]

  /*
   * Related to both the value and displacement type
   */

  /**
    * Find the displacement from one value to another. This can overflow, e.g., start is min = -max and end is max, so
    * result would be 2*max > max. If it overflows, it returns None.
    */
  def displacement(start: T, end: T): Option[Displacement]

  /**
    * Displace a value. This can overflow, e.g., value is max and offset is max, so result would be 2*max > max. If it
    * overflows, it returns None.
    */
  def displace(value: T, offset: Displacement): Option[T]

  /*
   * Concrete, based on definitions above
   */

  /**
    * Reflection scalar, where a displacement scaled by this amount is its negation.
    */
  def reflectionScalar: Scalar = negateScalar(identityScalar)

  /**
    * Iterates from start to end (exclusive) in steps <= maxStep (final step may be smaller) returning each displacement
    * and the size of the step. (The sum of the step sizes should equal end minus start).
    *
    * For example, if start = 3.0 and end = 5.5 with a maxStep of 1.0 then the result will be (3.0, 1.0), (4.0, 1.0),
    * (5.0, 0.5), where (5.5 - 3.0) == (1.0 + 1.0 + 0.5) == 2.5. Another example, if start = 5.0 and end = 3.0 with a
    * maxStep of -1.0 then the result will be (5.0, -1.0), (4.0, -1.0), where (3.0 - 5.0) == (-1.0 + -1.0) == -2.0.
    *
    * @param start
    *   the displacement to iterate from
    * @param end
    *   the displacement to iterate to (exclusive)
    * @param fullStep
    *   the step size for all but (possibly) the last step
    * @return
    *   an iterable of each displacement from start to end paired with its step size
    */
  def range(
    start: Displacement,
    end: Displacement,
    fullStep: Displacement
  ): IterableOnce[(Displacement, Displacement)] =
    val backward = fullStep < zeroDisplacement
    val forward = fullStep > zeroDisplacement
    require(
      (start == end) || (forward && start < end) || (backward && start > end),
      "will not terminate"
    )
    Iterator.unfold(start): prior =>
      add(prior, fullStep) match
        case _ if prior == end => // terminate iteration when we hit the end
          None
        case Some(next) if (forward && next <= end) || (backward && next >= end) => // common case, take a full step
          Some(((prior, fullStep), next))
        case Some(_) | None => // overstepped, end minus prior will have a smaller magnitude than maxStep
          add(end, negateDisplacement(prior)).map(finalStep => ((prior, finalStep), end))

  /**
    * Reflect a value around some pivot. This can overflow in two ways:
    *   - the value and pivot are so far apart that their displacement overflows, e.g., value is min and pivot is max
    *   - displacing the value pushes it outside the range, e.g., value is zero and pivot is max.
    *
    * If it overflows for either reason, it returns None.
    */
  def reflect(value: T, pivot: T): Option[T] = displacement(value, pivot).flatMap(displace(pivot, _))

  extension (offset: Displacement)
    /**
      * Negate a displacement. offset plus negate(offset) == zero.
      */
    def negated: Displacement = negateDisplacement(offset)

    /**
      * The magnitude of a displacement, i.e., its absolute value.
      */
    def magnitude: Displacement = if offset < zeroDisplacement then negateDisplacement(offset) else offset

    /**
      * Add another displacement to this one. This can overflow, e.g., both are max displacements. If it overflows, it
      * returns None.
      */
    infix def plus(other: Displacement): Option[Displacement] = add(offset, other)

    /**
      * Subtracts another displacement from this one. This can overflow, e.g., one is max and the other is min. If it
      * overflows, it returns None.
      */
    infix def minus(other: Displacement): Option[Displacement] = add(offset, negateDisplacement(other))

    /**
      * Scales by a factor. This can overflow, e.g., scale up a max displacements. If it overflows, it returns None.
      * Integer displacement types are rounded when scaled. See [[https://en.wikipedia.org/wiki/Scaling_(geometry)]]
      */
    infix def scaledBy(factor: Scalar): Option[Displacement] = scale(offset, factor)

object DomainAffineValueLike:

  /**
    * Right now all type classes use a double for the scalar.
    */
  trait DoubleScalar:
    self: DomainAffineValueLike[?] =>

    override type Scalar = Double
    override def identityScalar: Scalar = 1.0
    override def zeroScalar: Scalar = 0.0
    override def compareScalars(x: Scalar, y: Scalar): Int = x.compareTo(y)
    override def negateScalar(scalar: Scalar): Scalar = -scalar

  /**
    * For type classes using a double displacement.
    */
  trait DoubleDisplacement extends DoubleScalar:
    self: DomainAffineValueLike[?] =>

    override type Displacement = Double
    override def zeroDisplacement: Displacement = 0.0
    override def compareDisplacements(x: Displacement, y: Displacement): Int = x.compareTo(y)
    override def negateDisplacement(offset: Displacement): Displacement = -offset

    override def add(offset1: Displacement, offset2: Displacement): Option[Displacement] =
      validateDouble(offset1 + offset2)
    override def scale(offset: Displacement, scaledBy: Scalar): Option[Displacement] =
      validateDouble(offset * scaledBy)

  /**
    * For type classes using a duration displacement.
    */
  trait DurationDisplacement extends DoubleScalar:
    self: DomainAffineValueLike[?] =>

    import java.time.Duration
    private val nanosPerSecond = 1_000_000_000

    override type Displacement = Duration
    override def zeroDisplacement: Displacement = Duration.ZERO
    override def compareDisplacements(x: Displacement, y: Displacement): Int = x.compareTo(y)
    override def negateDisplacement(offset: Displacement): Displacement = offset.negated()

    override def add(offset1: Displacement, offset2: Displacement): Option[Displacement] =
      safe(offset1.plus(offset2))
    override def scale(offset: Duration, scaledBy: Scalar): Option[Duration] =
      validateDouble(scaledBy).flatMap: validScale =>
        val totalNanos = (BigDecimal(offset.getSeconds) * nanosPerSecond) + offset.getNano
        val (secondsBD, remainderNanosBD) = (totalNanos * validScale) /% nanosPerSecond
        if secondsBD.isValidLong then Some(Duration.ofSeconds(secondsBD.toLong, remainderNanosBD.toLong))
        else None

  /**
    * For type classes using an integer displacement.
    */
  trait IntDisplacement extends DoubleScalar:
    self: DomainAffineValueLike[?] =>

    override type Displacement = Int
    override def zeroDisplacement: Displacement = 0
    override def compareDisplacements(x: Displacement, y: Displacement): Int = x.compareTo(y)
    override def negateDisplacement(offset: Displacement): Displacement = -offset

    override def add(offset1: Displacement, offset2: Displacement): Option[Displacement] =
      safe(Math.addExact(offset1, offset2))
    override def scale(offset: Displacement, scaledBy: Scalar): Option[Displacement] =
      validateDouble(offset.toDouble * scaledBy).flatMap: scaled =>
        if scaled > Int.MaxValue || scaled < Int.MinValue then None
        else Some(Math.round(scaled).toInt)

  /**
    * For type classes using a long integer displacement.
    */
  trait LongDisplacement extends DoubleScalar:
    self: DomainAffineValueLike[?] =>

    // When the double's effective 53 bit mantissa has to represents 63 bit long integers, values wind up 1024 apart.
    // Backing off by 1024 ensures Math.round never overflows the signed 64-bit limit.
    private val MaxRoundableDouble: Double = (Long.MaxValue - 1024L).toDouble
    private val MinRoundableDouble: Double = (Long.MinValue + 1024L).toDouble

    override type Displacement = Long
    override def zeroDisplacement: Displacement = 0
    override def compareDisplacements(x: Displacement, y: Displacement): Int = x.compareTo(y)
    override def negateDisplacement(offset: Displacement): Displacement = -offset

    override def add(offset1: Displacement, offset2: Displacement): Option[Displacement] =
      safe(Math.addExact(offset1, offset2))
    override def scale(offset: Displacement, scaledBy: Scalar): Option[Displacement] =
      validateDouble(offset.toDouble * scaledBy).flatMap: scaled =>
        if scaled > MaxRoundableDouble || scaled < MinRoundableDouble then None
        else Some(Math.round(scaled))

  /**
    * For type classes using a big integer displacement.
    */
  trait BigIntDisplacement extends DoubleScalar:
    self: DomainAffineValueLike[BigInt] =>

    /**
      * If the result is not in the BigInt range, returns None. Implemented in DomainAffineValueLike[BigInt] where the
      * min and max are known.
      */
    protected def inRange(result: BigInt): Option[BigInt]

    override type Displacement = BigInt
    override def zeroDisplacement: Displacement = BigInt(0)
    override def compareDisplacements(x: Displacement, y: Displacement): Int = x.compareTo(y)
    override def negateDisplacement(offset: Displacement): Displacement = -offset

    override def add(offset1: Displacement, offset2: Displacement): Option[Displacement] =
      inRange(offset1 + offset2)
    override def scale(offset: Displacement, scaledBy: Scalar): Option[Displacement] =
      validateDouble(scaledBy).flatMap: validScale =>
        val scaledBD = BigDecimal(offset) * validScale
        val roundedBD = scaledBD.setScale(0, BigDecimal.RoundingMode.HALF_UP)
        inRange(roundedBD.toBigInt)
