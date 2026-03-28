package intervalidus

import intervalidus.ContinuousValue.given
import intervalidus.IntervalShape.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class ContinuousIntervalShapeTest extends AnyFunSuite with Matchers with IntervalShapeCommonBehaviors:
  testsFor(commonBehaviors("Continuous"))

  test(s"Continuous-specific Int IntervalShape tests"):
    val stringTest = Seq(
      toBeforeOrigin x fromOrigin, // II
      toBeforeOrigin x toBeforeOrigin // III
    ).toShape
    stringTest.toString shouldBe "IntervalShape({(-∞, 0), (-∞, +∞)})"
    stringTest.toCodeLikeString shouldBe "IntervalShape(intervalToBefore(0) x unbounded)"
