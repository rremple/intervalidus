package intervalidus.laws

import intervalidus.*
import intervalidus.Domain.In1D
import intervalidus.laws.IntervalGenerator.genNonIntersecting
import intervalidus.math.Polynomial
import intervalidus.immutable.DataFunction
import org.scalacheck.Gen

import scala.language.implicitConversions

object PolynomialGenerator:

  val genCoefficient: Gen[Double] =
    Gen.frequency(
      90 -> Gen.chooseNum(-64, 64).map(_.toDouble / 8), // so -8.0 to 8.0 in 1/8 increments
      10 -> Gen.const(0.0)
    )

  val genPolynomial: Gen[Polynomial] = for
    degree <- Gen.choose(-1, 4)
    coefficients <- Gen.listOfN(degree + 1, genCoefficient)
  yield Polynomial(coefficients.zipWithIndex.map(_.swap)*)

  val sampleMin = -100.0
  val sampleMax = 100.0

  def genSpline(using
    config: CoreConfig[In1D[Double]]
  )(using DomainValueLike[Double], DomainValueLike[Int]): Gen[Polynomial.Spline] =
    val sampleRange = Interval1D.interval(sampleMin, sampleMax)
    for
      intIntervals <- genNonIntersecting[In1D[Int]]
      rawIntervals1d = intIntervals.map(_.headInterval1D[Int].map(_.toDouble))
      intervals1d = rawIntervals1d.flatMap(_.intersectionWith(sampleRange)).take(10)
      polynomials <- Gen.listOfN(intervals1d.size, genPolynomial)
    yield DataFunction(intervals1d.zip(polynomials).map(_ -> _)).compressAll()

  val genSplinePoints: Gen[List[(Double, Double)]] = for
    n <- Gen.choose(3, 8)
    x0 <- Gen.choose(sampleMin, 0.0)
    dxs <- Gen.listOfN(n - 1, Gen.choose(4.0, 20.0))
    xs = dxs.scanLeft(x0)(_ + _)
    ys <- Gen.listOfN(n, Gen.choose(sampleMin, sampleMax))
  yield xs.zip(ys)
