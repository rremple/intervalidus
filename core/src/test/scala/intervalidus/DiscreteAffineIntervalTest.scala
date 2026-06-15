package intervalidus

import intervalidus.DiscreteAffineValue.given
import intervalidus.DomainAffineLike.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DiscreteAffineIntervalTest
  extends AnyFunSuite
  with Matchers
  with IntervalCommonBehaviors
  with AffineIntervalCommonBehaviors:

  testsFor(commonBehaviors("Discrete affine"))

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

  extension (lhs: IntervalShape[Int2d])
    def boundingShapeInt(thickness: (Int, Int)): IntervalShape[Int2d] = lhs.boundingShape(thickness)

  testsFor(commonAffineBehaviors("Discrete"))
