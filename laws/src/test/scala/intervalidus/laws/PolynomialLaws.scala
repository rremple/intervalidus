package intervalidus.laws

import intervalidus.laws.PolynomialGenerator.*
import intervalidus.*
import intervalidus.Domain.In1D
import intervalidus.immutable.DataFunction
import intervalidus.math.Polynomial
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions
import scala.math

class PolynomialLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 2000 /*, workers = 2*/ )
  def laws: String = getClass.getSimpleName

  import Polynomial.*

  // Delta tolerance for "normal" floating-point comparisons
  val absoluteTolerance: Double = 1e-6
  val relativeTolerance: Double = 1e-9

  extension (actual: Double)
    infix def isAbout(expected: Double): Boolean =
      val diff = math.abs(actual - expected)
      (actual == expected) ||
      (diff < absoluteTolerance) ||
      (diff < relativeTolerance * math.max(math.abs(actual), math.abs(expected)))
    infix def shouldBeAbout(expected: Double): Assertion =
      assert(actual isAbout expected, s"\nExpected: \n$expected ± $absoluteTolerance\nActual: \n$actual\n")

  extension (lhs: Polynomial)
    infix def isAbout(rhs: Polynomial): Boolean = lhs == rhs || (
      lhs.coefficients.size == rhs.coefficients.size && lhs.coefficients.keys.forall: exponent =>
        lhs.coefficients(exponent) isAbout rhs.coefficients(exponent)
    )
    infix def ≡≡(rhs: Polynomial): Assertion =
      assert(lhs isAbout rhs, s"\nExpected (rhs): \n${rhs}\nActual (lhs): \n${lhs}\n")

  extension (lhs: Spline)
    infix def isAbout(rhs: Spline)(using DomainValueLike[Double]): Boolean =
      val leftFilled = lhs.fill(Interval1D.unbounded -> zero)
      val rightFilled = rhs.fill(Interval1D.unbounded -> zero)
      leftFilled ≡ rightFilled || {
        val left = leftFilled.compressAll().getAll
        val right = rightFilled.compressAll().getAll
        left.size == right.size && left
          .zip(right)
          .forall:
            case (ValidData(lp: Polynomial, li), ValidData(rp: Polynomial, ri)) =>
              (li == ri) && (lp isAbout rp)
            case _ => false
      }
    infix def ≡≡(rhs: Spline)(using DomainValueLike[Double]): Assertion =
      assert(lhs isAbout rhs, s"\nExpected (rhs): \n${rhs}\nActual (lhs): \n${lhs}\n")

  def findSamples(commonDomain: IntervalShape.In1D[Double])(using DomainValueLike[Double]): Seq[Double] =
    commonDomain.boundingInterval.map(_.headInterval1D) match
      case None           => Seq.empty // should never happen
      case Some(interval) =>
        val start = interval.start match
          case Domain1D.Bottom       => sampleMin
          case Domain1D.Point(s)     => sampleMin.max(s)
          case Domain1D.OpenPoint(s) => sampleMin.max(s)
          case theUnexpected         => fail(s"didn't expect $theUnexpected")

        val end = interval.end match
          case Domain1D.Top          => sampleMax
          case Domain1D.Point(e)     => sampleMax.min(e)
          case Domain1D.OpenPoint(e) => sampleMax.min(e)
          case theUnexpected         => fail(s"didn't expect $theUnexpected")

        (start.toInt to end.toInt by 2).toSeq.map(_.toDouble).filter(commonDomain.contains)

  // everything is continuous and in 1D
  import ContinuousValue.DoubleContinuousValue
  given CoreConfig[In1D[Double]] = intervalidus.laws.DataGenerator.testCoreConfig

  property(s"1D Continuous ring & field axioms for polynomials and splines [$laws]"):
    import ContinuousValue.IntContinuousValue
    forAll(genPolynomial, genPolynomial, genPolynomial, genSpline, genSpline, genSpline): (a, b, c, p, q, r) =>
      val one = constant(1.0)
      val oneSpline = DataFunction.ofValue(one)
      val zeroSpline = DataFunction.ofValue(zero)

      // Commutativity, Associativity, Identity, and Distributivity

      // - Polynomials

      (a + b) ≡≡ (b + a)
      ((a + b) + c) ≡≡ (a + (b + c))
      (a + zero) ≡≡ a
      (a * one) ≡≡ a
      (a * zero) ≡≡ zero
      (a + 0) ≡≡ a
      (a * 1) ≡≡ a
      (a * (b + c)) ≡≡ ((a * b) + (a * c))

      // - Splines

      (p + q) ≡≡ (q + p)
      ((p + q) + r) ≡≡ (p + (q + r))
      (p + zeroSpline) ≡≡ p
      (p * oneSpline) ≡≡ p
      (p * zeroSpline) ≡≡ zeroSpline
      (p + 0) ≡≡ p
      (p * 1) ≡≡ p
      (p * (q + r)) ≡≡ ((p * q) + (p * r))

  property(s"1D Continuous evaluation & composition invariants of polynomials and splines [$laws]"):
    import ContinuousValue.IntContinuousValue
    forAll(genPolynomial, genPolynomial, genSpline, genSpline, genSpline): (a, b, pRaw, qRaw, r) =>
      // Need some guaranteed intersection: p.domain ∩ q.domain is r.domain ∪ (pRaw.domain ∩ qRaw.domain)
      val p = pRaw + r
      val q = qRaw + r
      val samples = findSamples(p.domain ∩ q.domain)

      // Homomorphism and Composition

      val a_plus_b = a + b
      val a_compose_b = a.compose(b)
      val a_andThen_b = a.andThen(b)
      val p_plus_q = p + q

      samples.foreach: x =>
        // - Polynomials

        a_plus_b(x) shouldBeAbout a(x) + b(x)
        a_compose_b(x) shouldBeAbout a(b(x))
        a_andThen_b(x) shouldBeAbout b(a(x))

        // - Splines (no composition)

        p_plus_q(x) shouldBeAbout p(x) + q(x)

  property(s"1D Continuous calculus operations on polynomials and splines [$laws]"):
    import ContinuousValue.IntContinuousValue
    forAll(genPolynomial, genPolynomial, genSpline, genSpline, genCoefficient): (a, b, p, q, c) =>
      // Fundamental Theorem of Calculus, Linearity, and the Product Rule

      // - Polynomials
      a.integral.derivative ≡≡ a
      (a + b).derivative ≡≡ (a.derivative + b.derivative)
      (a + b).integral ≡≡ (a.integral + b.integral)
      (a * c).integral ≡≡ (a.integral * c)
      (a * b).derivative ≡≡ ((a.derivative * b) + (a * b.derivative))

      // - Splines

      p.integral.derivative ≡≡ p
      (p + q).derivative ≡≡ (p.derivative + q.derivative)
      try (p + q).integral ≡≡ (p.integral + q.integral)
      catch
        case e: TestFailedException =>
          println("failure of (p + q).integral ≡≡ (p.integral + q.integral)")
          println(s"p=\n$p\np.integral=\n${p.integral}")
          println(s"q=\n$q\nq.integral=\n${q.integral}")
          println(s"p+q=\n${p + q}\n(p+q).integral=\n${(p + q).integral}")
          println(s"p.integral + q.integral=\n${p.integral + q.integral}")
      (p * c).integral ≡≡ (p.integral * c)
      (p * q).derivative ≡≡ ((p.derivative * q) + (p * q.derivative))

  property(s"1D Continuous operations for cubic spline interpolation [$laws]"):
    forAll(genSplinePoints): pointsToInterpolate =>
      // Interpolation, Continuity, and Natural Boundaries

      val fit = fitCubicSpline(pointsToInterpolate)
      pointsToInterpolate.foreach: (x, y) =>
        fit(x) shouldBeAbout y

      val data = fit.getAll.map(d => (d.value, d.interval.headInterval1D))
      val adjacentData = data.zip(data.drop(1))
      adjacentData.foreach:
        case ((left: Polynomial, Interval1D(_, leftEnd)), (right: Polynomial, Interval1D(rightStart, _))) =>
          left(leftEnd) shouldBeAbout right(rightStart)
          left.derivative(leftEnd) shouldBeAbout right.derivative(rightStart)
          left.derivative.derivative(leftEnd) shouldBeAbout right.derivative.derivative(rightStart)
        case theUnexpected => fail(s"didn't expect $theUnexpected")

      data.headOption.foreach:
        case (head: Polynomial, Interval1D(start, _)) => head.derivative.derivative(start) shouldBeAbout 0.0
        case theUnexpected                            => fail(s"didn't expect $theUnexpected")

      data.lastOption.foreach:
        case (last: Polynomial, Interval1D(_, end)) => last.derivative.derivative(end) shouldBeAbout 0.0
        case theUnexpected                          => fail(s"didn't expect $theUnexpected")
