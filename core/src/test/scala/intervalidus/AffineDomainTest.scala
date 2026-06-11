package intervalidus

import intervalidus.Domain.{HasDisplacementType, HasScalarType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{Duration, Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}

class AffineDomainTest extends AnyFunSuite with ProofMatchers with Matchers:

  test("Proves discrete domain has correct displacement and scalar types"):
    import DiscreteAffineValue.given

    type DiscreteDomain = Domain.In4D[Int, Long, LocalDate, BigInt]
    assertProven[DiscreteDomain HasDisplacementType (Int, Long, Long, BigInt)](
      "discrete domain has correct displacement types"
    )
    assertProven[DiscreteDomain HasScalarType (Double, Double, Double, Double)](
      "discrete domain has correct scalar types"
    )

  test("Proves continuous domain has correct displacement and scalar types"):
    import ContinuousAffineValue.given

    type ContinuousDomain = Tuple.Concat[Domain.In3D[Int, Long, LocalDate], Domain.In3D[Double, Instant, LocalDateTime]]
    assertProven[ContinuousDomain HasDisplacementType (Int, Long, Long, Double, Duration, Duration)](
      "continuous domain has correct displacement types"
    )
    assertProven[ContinuousDomain HasScalarType (Double, Double, Double, Double, Double, Double)](
      "continuous domain has correct scalar types"
    )

  test("Discrete Int displacements with Double scalars work (Int)"):
    import DiscreteAffineValue.IntDiscreteAffineValue
    val op = summon[DomainAffineValueLike[Int]]

    op.scalarOrdering.gt(5.0, 2.0) shouldBe true
    op.displacementOrdering.gt(5, 2) shouldBe true
    op.reflectionScalar shouldBe op.negateScalar(op.identityScalar)
    (-5).magnitude shouldBe 5.magnitude
    5.negated shouldBe -5
    5 plus 2 shouldBe Some(7)
    5 minus 2 shouldBe Some(3)
    Int.MaxValue plus 1 shouldBe None
    Int.MinValue minus 1 shouldBe None
    op.displace(1, 1) shouldBe Some(2)
    op.displacement(-2, 2) shouldBe Some(4)
    op.reflect(-2, 2) shouldBe Some(6)
    5 scaledBy op.zeroScalar shouldBe Some(op.zeroDisplacement)
    5 scaledBy op.identityScalar shouldBe Some(5)
    5 scaledBy op.negateScalar(2.0) shouldBe Some(-10)
    5 scaledBy Int.MaxValue.toDouble shouldBe None
    4 scaledBy 2.1 shouldBe Some(8) // 8.4 rounded down
    4 scaledBy 2.2 shouldBe Some(9) // 8.8 rounded up

  test("Discrete Long displacements with Double scalars work (Long)"):
    import DiscreteAffineValue.LongDiscreteAffineValue
    val op = summon[DomainAffineValueLike[Long]]

    op.displacementOrdering.gt(5L, 2L) shouldBe true
    5L.negated shouldBe -5L
    5L plus 2 shouldBe Some(7L)
    5L minus 2 shouldBe Some(3L)
    Long.MaxValue plus 1L shouldBe None
    Long.MinValue minus 1L shouldBe None
    op.displace(1L, 1L) shouldBe Some(2L)
    op.displacement(-2L, 2L) shouldBe Some(4L)
    5L scaledBy op.zeroScalar shouldBe Some(op.zeroDisplacement)
    5L scaledBy op.identityScalar shouldBe Some(5L)
    5L scaledBy op.negateScalar(2.0) shouldBe Some(-10L)
    4L scaledBy 2.1 shouldBe Some(8L) // 8.4 rounded down
    4L scaledBy 2.2 shouldBe Some(9L) // 8.8 rounded up

    // weird rounding boundaries/artifacts where large longs cannot be represented exactly in doubles after scaling
    (Long.MaxValue / 2L) scaledBy 2.0 shouldBe None // too close to the edge
    ((Long.MaxValue - 511L) / 2L) scaledBy 2.0 shouldBe None // still to close to the edge to be safely rounded
    (((Long.MaxValue - 512L) / 2L) scaledBy 2.0).flatMap(Long.MaxValue minus _) shouldBe Some(1023) // at the edge

    (Long.MinValue / 2L) scaledBy 2.0 shouldBe None // too close to the edge
    ((Long.MinValue + 512L) / 2L) scaledBy 2.0 shouldBe None // still to close to the edge to be safely rounded
    (((Long.MinValue + 513L) / 2L) scaledBy 2.0).flatMap(Long.MinValue minus _) shouldBe Some(-1024) // at the edge

  test("Discrete Long displacements with Double scalars work (LocalDate)"):
    import DiscreteAffineValue.LocalDateDiscreteAffineValue
    val op = summon[DomainAffineValueLike[LocalDate]]
    def may(day: Int): LocalDate = LocalDate.of(2026, 5, day)

    op.displace(may(30), 1L) shouldBe Some(may(31))
    op.displacement(may(10), may(20)) shouldBe Some(10L)
    op.displacement(may(20), may(10)) shouldBe Some(-10L)

  test("Discrete BigInt displacements with Double scalars work (BigInt)"):
    import DiscreteAffineValue.BigIntDiscreteAffineValue
    val op = summon[DomainAffineValueLike[BigInt]]

    op.displacementOrdering.gt(BigInt(5), BigInt(2)) shouldBe true
    BigInt(5).negated shouldBe -BigInt(5)
    BigInt(5) plus BigInt(2) shouldBe Some(BigInt(7))
    BigInt(5) minus BigInt(2) shouldBe Some(BigInt(3))
    op.maxValue plus BigInt(1) shouldBe None // in BigInt range, but outside domain range
    op.minValue minus BigInt(2) shouldBe None // in BigInt range, but outside domain range
    op.displace(BigInt(1), BigInt(1)) shouldBe Some(BigInt(2))
    op.displacement(BigInt(-2), BigInt(2)) shouldBe Some(BigInt(4))
    BigInt(5) scaledBy op.zeroScalar shouldBe Some(op.zeroDisplacement)
    BigInt(5) scaledBy op.identityScalar shouldBe Some(BigInt(5))
    BigInt(5) scaledBy op.negateScalar(2.0) shouldBe Some(BigInt(-10))
    BigInt(4) scaledBy 2.1 shouldBe Some(BigInt(8)) // 8.4 rounded down
    BigInt(4) scaledBy 2.2 shouldBe Some(BigInt(9)) // 8.8 rounded up

  test("Continuous Int displacements with Double scalars work (Int)"):
    import ContinuousAffineValue.IntContinuousAffineValue
    val op = summon[DomainAffineValueLike[Int]]

    // only test things not covered in discrete
    op.displace(1, 1) shouldBe Some(2)
    op.displacement(-2, 2) shouldBe Some(4)

  test("Continuous Long displacements with Double scalars work (Long)"):
    import ContinuousAffineValue.LongContinuousAffineValue
    val op = summon[DomainAffineValueLike[Long]]

    // only test things not covered in discrete
    op.displace(1L, 1L) shouldBe Some(2L)
    op.displacement(-2L, 2L) shouldBe Some(4L)

  test("Continuous Long displacements with Double scalars work (LocalDate)"):
    import ContinuousAffineValue.LocalDateContinuousAffineValue
    val op = summon[DomainAffineValueLike[LocalDate]]

    def may(day: Int): LocalDate = LocalDate.of(2026, 5, day)

    // only test things not covered in discrete
    op.displace(may(30), 1L) shouldBe Some(may(31))
    op.displacement(may(10), may(20)) shouldBe Some(10L)
    op.displacement(may(20), may(10)) shouldBe Some(-10L)

  test("Continuous Double displacements with Double scalars work (Double)"):
    import ContinuousAffineValue.DoubleContinuousAffineValue
    val op = summon[DomainAffineValueLike[Double]]

    op.displacementOrdering.gt(5.0, 2.0) shouldBe true
    5.0.negated shouldBe -5.0
    5.0 plus 2.0 shouldBe Some(7.0)
    5.0 minus 2.0 shouldBe Some(3.0)
    op.maxValue plus Double.MaxValue shouldBe None // overflows to positive infinity
    op.minValue minus Double.MaxValue shouldBe None // overflows to negative infinity
    op.displace(1.0, 1.0) shouldBe Some(2.0)
    op.displacement(-2.0, 2.0) shouldBe Some(4.0)
    5.0 scaledBy op.zeroScalar shouldBe Some(op.zeroDisplacement)
    5.0 scaledBy op.identityScalar shouldBe Some(5.0)
    5.0 scaledBy op.negateScalar(2.0) shouldBe Some(-10.0)
    5.0 scaledBy Double.MaxValue shouldBe None
    4.0 scaledBy 2.1 shouldBe Some(8.4) // no rounding
    4.0 scaledBy 2.2 shouldBe Some(8.8) // no rounding

  test("Continuous Duration displacements with Double scalars work (Instant)"):
    import ContinuousAffineValue.InstantContinuousAffineValue
    val op = summon[DomainAffineValueLike[Instant]]

    import Duration.{ofHours as hours}
    def may(day: Int): Instant = LocalDate.of(2026, 5, day).atStartOfDay().toInstant(ZoneOffset.UTC)

    op.displacementOrdering.gt(hours(5), hours(2)) shouldBe true
    op.negateDisplacement(hours(5)) shouldBe hours(-5)
    op.add(hours(5), hours(2)) shouldBe Some(hours(7))
    op.add(hours(5), hours(2).negated) shouldBe Some(hours(3))
    op.add(Duration.ofSeconds(Long.MaxValue), hours(5)) shouldBe None // overflows
    op.add(Duration.ofSeconds(Long.MinValue), hours(-5)) shouldBe None // overflows
    op.displace(may(30), Duration.ofDays(1)) shouldBe Some(may(31))
    op.displacement(may(10), may(20)) shouldBe Some(Duration.ofDays(10))
    op.displacement(may(10), may(10).minusSeconds(10)) shouldBe Some(Duration.ofSeconds(-10))
    hours(5) scaledBy op.zeroScalar shouldBe Some(op.zeroDisplacement)
    hours(5) scaledBy op.identityScalar shouldBe Some(hours(5))
    hours(5) scaledBy op.negateScalar(2.0) shouldBe Some(hours(-10))
    hours(4) scaledBy 2.1 shouldBe Some(hours(8).plusMinutes(24)) // no rounding
    hours(4) scaledBy 2.2 shouldBe Some(hours(8).plusMinutes(48)) // no rounding
    Duration.ofSeconds(Long.MaxValue) scaledBy 2.0 shouldBe None // overflows

    assertThrows[IllegalArgumentException]: // backward && not start > end => will not terminate
      op.range(hours(4), hours(6).plusMinutes(24), hours(-1))

    op.range(hours(4), hours(6).plusMinutes(24), hours(1)).iterator.toList shouldBe List(
      (hours(4), hours(1)),
      (hours(5), hours(1)),
      (hours(6), Duration.ofMinutes(24))
    )

    op.range(hours(6), hours(4).minusMinutes(24), hours(-1)).iterator.toList shouldBe List(
      (hours(6), hours(-1)),
      (hours(5), hours(-1)),
      (hours(4), Duration.ofMinutes(-24))
    )

  test("Continuous Duration displacements with Double scalars (LocalDateTime)"):
    import ContinuousAffineValue.LocalDateTimeContinuousAffineValue
    val op = summon[DomainAffineValueLike[LocalDateTime]]

    import Duration.{ofDays as days}
    def may(day: Int): LocalDateTime = LocalDate.of(2026, 5, day).atTime(LocalTime.MIDNIGHT)

    // only test things not covered by LocalDateTime
    op.displace(may(30), days(1)) shouldBe Some(may(31))
    op.displacement(may(10), may(20)) shouldBe Some(days(10))
    op.displacement(may(10), may(10).minusSeconds(10)) shouldBe Some(Duration.ofSeconds(-10))
