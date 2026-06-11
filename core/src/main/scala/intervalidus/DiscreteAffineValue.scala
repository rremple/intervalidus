package intervalidus

import intervalidus.DomainAffineValueLike.*

import java.lang.Math
import java.time.LocalDate
import java.time.temporal.ChronoUnit

/**
  * Type class for discrete affine values
  *
  * @tparam T
  *   a type with discrete affine value behavior (e.g., `Int`)
  */

trait DiscreteAffineValue[T](delegate: DiscreteValue[T]) extends DiscreteValue[T] with DomainAffineValueLike[T]:
  override def compare(lhs: T, rhs: T): Int = delegate.compare(lhs, rhs)
  override def successorOf(x: T): Option[T] = delegate.successorOf(x)
  override def predecessorOf(x: T): Option[T] = delegate.predecessorOf(x)
  override def orderedHashOf(x: T): Double = delegate.orderedHashOf(x)
  override val maxValue: T = delegate.maxValue
  override val minValue: T = delegate.minValue

/**
  * Default discrete affine value type classes for common data types.
  */
object DiscreteAffineValue:
  /**
    * Type class for integers as discrete affine values.
    */
  given IntDiscreteAffineValue: (DiscreteAffineValue[Int] & IntDisplacement) =
    new DiscreteAffineValue[Int](DiscreteValue.IntDiscreteValue) with IntDisplacement:
      override def displacement(start: Int, end: Int): Option[Displacement] =
        safe(Math.subtractExact(end, start))
      override def displace(value: Int, offset: Displacement): Option[Int] =
        safe(Math.addExact(value, offset))

  /**
    * Type class for long integers as discrete affine values.
    */
  given LongDiscreteAffineValue: (DiscreteAffineValue[Long] & LongDisplacement) =
    new DiscreteAffineValue[Long](DiscreteValue.LongDiscreteValue) with LongDisplacement:
      override def displacement(start: Long, end: Long): Option[Displacement] =
        safe(Math.subtractExact(end, start))
      override def displace(value: Long, offset: Displacement): Option[Long] =
        safe(Math.addExact(value, offset))

  /**
    * Type class for local dates as discrete affine values.
    */
  given LocalDateDiscreteAffineValue: (DiscreteAffineValue[LocalDate] & LongDisplacement) =
    new DiscreteAffineValue[LocalDate](DiscreteValue.LocalDateDiscreteValue) with LongDisplacement:
      override def displacement(start: LocalDate, end: LocalDate): Option[Displacement] =
        safe(ChronoUnit.DAYS.between(start, end))
      override def displace(value: LocalDate, offset: Displacement): Option[LocalDate] =
        safe(value.plusDays(offset))

  /**
    * Type class for big integers as discrete affine values.
    */
  given BigIntDiscreteAffineValue: (DiscreteAffineValue[BigInt] & BigIntDisplacement) =
    new DiscreteAffineValue[BigInt](DiscreteValue.BigIntDiscreteValue) with BigIntDisplacement:
      override def inRange(result: BigInt): Option[BigInt] =
        if result < minValue || result > maxValue then None else Some(result)
      override def displacement(start: BigInt, end: BigInt): Option[Displacement] =
        inRange(end - start)
      override def displace(value: BigInt, offset: Displacement): Option[BigInt] =
        inRange(value + offset)
