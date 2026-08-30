package intervalidus.math

import intervalidus.*
import intervalidus.Domain1D.{Bottom, Top}
import intervalidus.Interval1D.*
import intervalidus.ContinuousValue.DoubleContinuousValue
import intervalidus.immutable.DataFunction
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class PolynomialTest extends AnyFunSuite with Matchers:

  import Polynomial.*
  import Double.{NegativeInfinity, PositiveInfinity}
  type Dim = Domain.In1D[Double]

  test("Polynomial stuff"):
    (x + 1).toString shouldBe "x + 1"
    assertThrows[IllegalArgumentException]:
      (x ^ -1) + 1

    assertThrows[IllegalArgumentException]:
      Polynomial(-1 -> 1, 0 -> 1)

    val a = -2.875 * (x ^ 3) + 2.375 * x - 8

    a shouldBe Polynomial(3 -> -2.875, 1 -> 2.375, 0 -> -8.0)
    a.toCodeLikeString shouldBe "Polynomial(3 -> -2.875, 1 -> 2.375, 0 -> -8.0)"
    a.toString shouldBe "-2.875x^3 + 2.375x - 8"

    a.degree shouldBe 3
    x.degree shouldBe 1
    constant(1.0).degree shouldBe 0
    zero.degree shouldBe -1

    constant(0.0) shouldBe zero
    (0.0: Polynomial) shouldBe zero
    (0: Polynomial) shouldBe zero

    a + 8 shouldBe -2.875 * (x ^ 3) + 2.375 * x
    a * 8 shouldBe -23 * (x ^ 3) + 19 * x - 64
    a + 8 shouldBe 8 + a
    a * 8 shouldBe 8 * a

    x(Bottom) shouldBe NegativeInfinity
    x(Top) shouldBe PositiveInfinity
    zero(Bottom) shouldBe 0.0
    zero(Top) shouldBe 0.0
    constant(1.0)(Bottom) shouldBe 1.0

    a(Bottom) shouldBe PositiveInfinity
    a(Top) shouldBe NegativeInfinity
    a(0.0) shouldBe -8.0
    a(1.0) shouldBe -8.5
    a(Domain1D.open(0.0)) shouldBe -8.0

    zero.degree shouldBe -1
    zero(1.0) shouldBe 0.0
    zero.toString shouldBe "0"
    zero.toCodeLikeString shouldBe "Polynomial.zero"

    val b = Polynomial(3 -> 0.875, 0 -> 8.0, 4 -> 4.0)
    b.toString shouldBe "4x^4 + 0.875x^3 + 8"
    b.degree shouldBe 4
    b(Bottom) shouldBe PositiveInfinity
    b(Top) shouldBe PositiveInfinity
    b(0.0) shouldBe 8.0

    a + b shouldBe 4 * (x ^ 4) - 2 * (x ^ 3) + 2.375 * x
    a - b shouldBe -4 * (x ^ 4) - 3.75 * (x ^ 3) + 2.375 * x - 16
    a + (-b) shouldBe a - b

    val c = 2 * x + 2
    a * c shouldBe -5.75 * (x ^ 4) - 5.75 * (x ^ 3) + 4.75 * (x ^ 2) - 11.25 * x - 16

    a.andThen(c) shouldBe -5.75 * (x ^ 3) + 4.75 * x - 14
    a.compose(c) shouldBe -23 * (x ^ 3) - 69 * (x ^ 2) - 64.25 * x - 26.25
    c ^ 3 shouldBe 8 * (x ^ 3) + 24 * (x ^ 2) + 24 * x + 8
    c ^ 4 shouldBe (c ^ 3) * c

    a.derivative shouldBe (-2.875 * 3) * (x ^ 2) + 2.375
    b.derivative shouldBe (4.0 * 4.0) * (x ^ 3) + (0.875 * 3) * (x ^ 2)
    c.derivative shouldBe (2.0: Polynomial)

    a.integral shouldBe (-2.875 / 4) * (x ^ 4) + (2.375 / 2) * (x ^ 2) - 8.0 * x
    b.integral shouldBe (4.0 / 5.0) * (x ^ 5) + (0.875 / 4) * (x ^ 4) + 8.0 * x
    c.integral shouldBe (x ^ 2) + 2 * x
    c.integrate(Interval1D.interval(1.0, 10.0)) shouldBe 100.0 + 20.0 - 3.0

  test("Spline stuff"):
    // Because the Spline type is relatively weak, there can be DataFunctions masquerading as Splines that are not based
    // on Polynomials, which causes errors in methods that assume all functions are polynomials.
    val badSpline: Spline = DataFunction.of(
      interval(0.0, 1.0) -> (_ => 0.0) // not a Polynomial
    )
    assertThrows[Exception]:
      val _ = badSpline.derivative // applyToPieces fails on presence of non-polynomial
    assertThrows[Exception]:
      val _ = badSpline + badSpline // zipAndApplyToPieces fails on presence of non-polynomial
    assertThrows[Exception]:
      val _ = badSpline.integral // fails on presence of non-polynomial
    assertThrows[Exception]:
      val _ = badSpline.toCodeLikeString // fails on presence of non-polynomial

    val a = (3 * (x ^ 2))
    val b = (8 * x - 4)

    val splineOne: Spline = a // implicitly converted to a spline with one unbounded piece
    splineOne.derivative.get shouldBe a.derivative
    splineOne.integral.get shouldBe a.integral
    splineOne.integrate(Interval1D.interval(1.0, 3.0)) shouldBe 27.0 - 1.0
    a.integrate(Interval1D.interval(1.0, 3.0)) shouldBe 27.0 - 1.0

    val splineUnbounded: Spline = DataFunction.of(
      intervalTo(2.0) -> a,
      intervalFromAfter(2.0) -> b
    )

    val splineBounded: Spline = DataFunction.of(
      interval(-2.0, 0.0) -> a,
      intervalFromAfter(0.0).to(6.0) -> b
    )

    splineUnbounded.toCodeLikeString shouldBe "DataFunction.of(" +
      "intervalTo(2.0) -> Polynomial(2 -> 3.0), " +
      "intervalFromAfter(2.0) -> Polynomial(1 -> 8.0, 0 -> -4.0))"
    DataFunction.ofValue(zero).toCodeLikeString shouldBe "DataFunction.of(unbounded -> Polynomial.zero)"

    a.integral(2.0) shouldBe 8.0
    b.integral(2.0) shouldBe 8.0

    splineUnbounded.integral shouldBe DataFunction.of(
      intervalTo(2.0) -> a.integral, // C = 0.0
      intervalFromAfter(2.0) -> b.integral // C = 0.0 + (8.0 - 8.0) = 0.0
    )

    zero.integral(-2.0) shouldBe 0.0
    a.integral(-2.0) shouldBe -8.0

    a.integral(0.0) shouldBe 0.0
    b.integral(0.0) shouldBe 0.0

    b.integral(6.0) shouldBe 120.0
    zero.integral(6.0) shouldBe 0.0

    splineBounded.integral shouldBe DataFunction.of(
      intervalToBefore(-2.0) -> zero, // C = 0.0
      interval(-2.0, 0.0) -> (a.integral + 8.0), // C = 0.0 + (0.0 - (-8.0)) = 8.0
      intervalFromAfter(0.0).to(6.0) -> (b.integral + 8.0), // C = 8.0 + (0.0 - 0.0) = 8.0
      intervalFromAfter(6.0) -> constant(128.0) // C = 8.0 + (120.0 - 0.0) = 128.0
    )

    splineBounded.integrate(intervalTo(-1.0)) shouldBe 7.0
    splineBounded.integrate(intervalTo(4.0)) shouldBe 56.0
    splineBounded.integrate(interval(-10.0, 10.0)) shouldBe 128.0

    splineUnbounded.derivative shouldBe DataFunction.of(
      intervalTo(2.0) -> a.derivative,
      intervalFromAfter(2.0) -> b.derivative
    )

    splineBounded.derivative shouldBe DataFunction.of(
      interval(-2.0, 0.0) -> a.derivative,
      intervalFromAfter(0.0).to(6.0) -> b.derivative
    )

    (splineUnbounded + 42) shouldBe DataFunction.of(
      intervalTo(2.0) -> (a + 42.0),
      intervalFromAfter(2.0) -> (b + 42.0)
    )

    (splineUnbounded - 42) shouldBe DataFunction.of(
      intervalTo(2.0) -> (a - 42.0),
      intervalFromAfter(2.0) -> (b - 42.0)
    )

    (splineUnbounded * 42) shouldBe DataFunction.of(
      intervalTo(2.0) -> (a * 42.0),
      intervalFromAfter(2.0) -> (b * 42.0)
    )

    (splineUnbounded ^ 2) shouldBe DataFunction.of(
      intervalTo(2.0) -> (a ^ 2),
      intervalFromAfter(2.0) -> (b ^ 2)
    )

    (splineUnbounded + splineBounded) shouldBe DataFunction.of(
      intervalToBefore(-2.0) -> a,
      interval(-2.0, 0.0) -> 2 * a,
      intervalFromAfter(0.0).to(2.0) -> (a + b),
      intervalFromAfter(2.0).to(6.0) -> 2 * b,
      intervalFromAfter(6.0) -> b
    )

    (splineUnbounded - splineBounded) shouldBe DataFunction.of(
      intervalToBefore(-2.0) -> a,
      interval(-2.0, 0.0) -> zero,
      intervalFromAfter(0.0).to(2.0) -> (a - b),
      intervalFromAfter(2.0).to(6.0) -> zero,
      intervalFromAfter(6.0) -> b
    )

    (splineUnbounded * splineBounded) shouldBe DataFunction.of(
      intervalToBefore(-2.0) -> zero,
      interval(-2.0, 0.0) -> (a ^ 2),
      intervalFromAfter(0.0).to(2.0) -> (a * b),
      intervalFromAfter(2.0).to(6.0) -> (b ^ 2),
      intervalFromAfter(6.0) -> zero
    )

    val points = Seq((0.0, 0.0), (1.0, 4.0), (2.0, 20.0), (3.0, 36.0), (4.0, 40.0)) // A Subtle S-Curve (Sigmoid-like)
    val splineFit = fitCubicSpline(points)
    splineFit shouldBe DataFunction.of(
      interval(0.0, 1.0) -> (3 * (x ^ 3) + x),
      intervalFromAfter(1.0).to(3.0) -> (-3 * (x ^ 3) + 18 * (x ^ 2) - 17 * x + 6),
      intervalFromAfter(3.0).to(4.0) -> (3 * (x ^ 3) - 36 * (x ^ 2) + 145 * x - 156)
    )
    points.foreach: (x, y) =>
      splineFit(x) shouldBe y

    assertThrows[Exception]:
      val _ = fitCubicSpline(Seq((0.0, 0.0), (1.0, 4.0))) // At least 3 points are required for a cubic spline
    assertThrows[Exception]:
      val _ = fitCubicSpline(Seq((4.0, 40.0), (0.0, 0.0), (1.0, 4.0))) // x values must be strictly increasing
