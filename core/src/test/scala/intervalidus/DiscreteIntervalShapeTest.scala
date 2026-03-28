package intervalidus

import intervalidus.DiscreteValue.given
import intervalidus.IntervalShape.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DiscreteIntervalShapeTest extends AnyFunSuite with Matchers with IntervalShapeCommonBehaviors:
  testsFor(commonBehaviors("Discrete"))

  test(s"Discrete-specific Int IntervalShape tests"):
    val stringTest = Seq(
      toBeforeOrigin x fromOrigin, // II
      toBeforeOrigin x toBeforeOrigin // III
    ).toShape
    stringTest.toString shouldBe "IntervalShape({(-∞..-1], (-∞..+∞)})"
    stringTest.toCodeLikeString shouldBe "IntervalShape(intervalTo(-1) x unbounded)"
