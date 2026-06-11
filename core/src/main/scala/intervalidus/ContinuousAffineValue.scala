package intervalidus

import intervalidus.DomainAffineValueLike.*

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant, LocalDate, LocalDateTime}

/**
  * Type class for continuous affine values
  *
  * @tparam T
  *   a type with continuous affine value behavior (e.g., `Double`)
  */
trait ContinuousAffineValue[T](delegate: ContinuousValue[T]) extends ContinuousValue[T] with DomainAffineValueLike[T]:
  override def compare(lhs: T, rhs: T): Int = delegate.compare(lhs, rhs)
  override def orderedHashOf(x: T): Double = delegate.orderedHashOf(x)
  override val maxValue: T = delegate.maxValue
  override val minValue: T = delegate.minValue

/**
  * Default continuous affine value type classes for common data types.
  */
object ContinuousAffineValue:
  /**
    * Type class for doubles as continuous affine values.
    */
  given DoubleContinuousAffineValue: (ContinuousAffineValue[Double] & DoubleDisplacement) =
    new ContinuousAffineValue[Double](ContinuousValue.DoubleContinuousValue) with DoubleDisplacement:
      override def displacement(start: Double, end: Double): Option[Displacement] =
        validateDouble(end - start)
      override def displace(value: Double, offset: Displacement): Option[Displacement] =
        validateDouble(value + offset)

  /**
    * Type class for local date-times as continuous affine values.
    */
  given LocalDateTimeContinuousAffineValue: (ContinuousAffineValue[LocalDateTime] & DurationDisplacement) =
    new ContinuousAffineValue[LocalDateTime](ContinuousValue.LocalDateTimeContinuousValue) with DurationDisplacement:
      override def displacement(start: LocalDateTime, end: LocalDateTime): Option[Displacement] =
        safe(Duration.between(start, end))
      override def displace(value: LocalDateTime, offset: Displacement): Option[LocalDateTime] =
        safe(value.plus(offset))

  /**
    * Type class for instants as continuous affine values.
    */
  given InstantContinuousAffineValue: (ContinuousAffineValue[Instant] & DurationDisplacement) =
    new ContinuousAffineValue[Instant](ContinuousValue.InstantContinuousValue) with DurationDisplacement:
      override def displacement(start: Instant, end: Instant): Option[Displacement] =
        safe(Duration.between(start, end))
      override def displace(value: Instant, offset: Displacement): Option[Instant] =
        safe(value.plus(offset))

  /**
    * Type class for integers as continuous affine values (even though they are discrete).
    */
  given IntContinuousAffineValue: (ContinuousAffineValue[Int] & IntDisplacement) =
    new ContinuousAffineValue[Int](ContinuousValue.IntContinuousValue) with IntDisplacement:
      override def displacement(start: Int, end: Int): Option[Displacement] =
        safe(Math.subtractExact(end, start))
      override def displace(value: Int, offset: Displacement): Option[Int] =
        safe(Math.addExact(value, offset))

  /**
    * Type class for long integers as continuous affine values (even though they are discrete).
    */
  given LongContinuousAffineValue: (ContinuousAffineValue[Long] & LongDisplacement) =
    new ContinuousAffineValue[Long](ContinuousValue.LongContinuousValue) with LongDisplacement:
      override def displacement(start: Long, end: Long): Option[Displacement] =
        safe(Math.subtractExact(end, start))
      override def displace(value: Long, offset: Displacement): Option[Long] =
        safe(Math.addExact(value, offset))

  /**
    * Type class for local dates as continuous affine values (even though they are discrete).
    */
  given LocalDateContinuousAffineValue: (ContinuousAffineValue[LocalDate] & LongDisplacement) =
    new ContinuousAffineValue[LocalDate](ContinuousValue.LocalDateContinuousValue) with LongDisplacement:
      override def displacement(start: LocalDate, end: LocalDate): Option[Displacement] =
        safe(ChronoUnit.DAYS.between(start, end))
      override def displace(value: LocalDate, offset: Displacement): Option[LocalDate] =
        safe(value.plusDays(offset))
