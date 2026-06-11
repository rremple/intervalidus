package intervalidus

import intervalidus.ContinuousAffineValue.given
import intervalidus.DomainAffineLike.*
import intervalidus.Domain1D.{domain, open}
import intervalidus.Interval1D.*
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class ContinuousAffineIntervalTest
  extends AnyFunSuite
  with Matchers
  with IntervalCommonBehaviors
  with AffineIntervalCommonBehaviors:

  testsFor(commonBehaviors("Continuous affine"))

  extension (lhs: Domain1D[Int])
    override infix def displacementToInt(rhs: Domain1D[Int]): Option[Int] = lhs.displacementTo(rhs)
    override infix def displacedByInt(offset: Int): Domain1D[Int] = lhs.displacedBy(offset)
    override infix def reflectedAboutInt(pivot: Domain1D[Int]): Domain1D[Int] = lhs.reflectedAbout(pivot)
    override infix def scaledAboutInt(center: Domain1D[Int], scaledBy: Double): Domain1D[Int] =
      lhs.scaledAbout(center, scaledBy)

  extension (lhs: Interval1D[Int])
    override infix def measureInt: Option[Int] = lhs.measure
    override infix def displacedByInt(offset: Int): Option[Interval1D[Int]] = lhs.displacedBy(offset)
    override infix def reflectedAboutInt(pivot: Domain1D[Int]): Option[Interval1D[Int]] = lhs.reflectedAbout(pivot)
    override infix def scaledAboutInt(center: Domain1D[Int], scaledBy: Double): Option[Interval1D[Int]] =
      lhs.scaledAbout(center, scaledBy)

  extension (lhs: Interval[Int2d])
    override infix def measureInt: Option[(Int, Int)] = lhs.measure
    override infix def displacedByInt(offset: (Int, Int)): Option[Interval[Int2d]] = lhs.displacedBy(offset)
    override infix def reflectedAboutInt(pivot: Int2d): Option[Interval[Int2d]] = lhs.reflectedAbout(pivot)
    override infix def scaledAboutInt(center: Int2d, scaledBy: (Double, Double)): Option[Interval[Int2d]] =
      lhs.scaledAbout(center, scaledBy)

  testsFor(commonAffineBehaviors("Continuous"))

  test("Continuous: Int affine 1d open domain behaviors"):
    domain(2) displacementToInt open(2) shouldBe Some(0)
    open(2) displacementToInt top shouldBe None
    open(2) reflectedAboutInt 3 shouldBe open(4)

  test("Continuous: Double affine 2d interval shape behaviors"):

    val i1 = (intervalFrom(1.0).toBefore(3.0) x intervalFromAfter(3.0).to(5.0))
    i1.measure shouldBe Some((2.0, 2.0))
    val i2 = (intervalFromAfter(2.0).to(5.0) x intervalFrom(6.0).toBefore(10.0))
    i2.measure shouldBe Some((3.0, 4.0))
    val i3 = (intervalFrom(4.0) x interval(4.0, 5.0))
    i3.measure shouldBe None

    val shape = IntervalShape(Seq(i1, i2, i3))

    val shapeReflected = shape.reflectedAbout(Domain.in2D(3.0, 3.0))
    shapeReflected.allIntervals.toList shouldBe List(
      intervalTo(2.0) x interval(1.0, 2.0),
      intervalFrom(1.0).toBefore(4.0) x intervalFromAfter(-4.0).to(0.0),
      intervalFromAfter(3.0).to(5.0) x intervalFrom(1.0).toBefore(3.0)
    )

    val shapeDisplaced = shape.displacedBy((3.0, 2.0))
    shapeDisplaced.allIntervals.toList shouldBe List(
      intervalFrom(4.0).toBefore(6.0) x intervalFromAfter(5.0).to(7.0),
      intervalFromAfter(5.0).to(8.0) x intervalFrom(8.0).toBefore(12.0),
      intervalFrom(7.0) x interval(6.0, 7.0)
    )

    val shapeScaled = shape.scaledAbout(Domain.in2D(0.0, 0.0), scaledBy = (3.0, 2.0))
    shapeScaled.allIntervals.toList shouldBe List(
      intervalFrom(3.0).toBefore(9.0) x intervalFromAfter(6.0).to(10.0),
      intervalFromAfter(6.0).to(15.0) x intervalFrom(12.0).toBefore(20.0),
      intervalFrom(12.0) x interval(8.0, 10.0)
    )

    type S = (Double, Double) // displacement vector

    i1.mapMeasure[S, Double](volume) shouldBe Some(4.0)
    i2.mapMeasure[S, Double](volume) shouldBe Some(12.0)
    i2.mapMeasure[S, Double](diagonal) shouldBe Some(5.0)

    shape.mapReduceMeasure[S, Double](volume, _ + _) shouldBe Some(16.0)
    shapeReflected.mapReduceMeasure[S, Double](volume, _ + _) shouldBe Some(16.0)
    shapeDisplaced.mapReduceMeasure[S, Double](volume, _ + _) shouldBe Some(16.0)
    shapeScaled.mapReduceMeasure[S, Double](volume, _ + _) shouldBe Some(16.0 * 3.0 * 2.0)

    shape.mapReduceMeasure[S, Double](diagonal, math.max) shouldBe Some(5.0) // math.sqrt(3.0 ** 2 + 4.0 ** 2)
    shapeReflected.mapReduceMeasure[S, Double](diagonal, math.max) shouldBe Some(5.0)
    shapeDisplaced.mapReduceMeasure[S, Double](diagonal, math.max) shouldBe Some(5.0)
    shapeScaled.mapReduceMeasure[S, Double](diagonal, math.max) shouldBe Some(math.sqrt(9.0 ** 2 + 8.0 ** 2))
