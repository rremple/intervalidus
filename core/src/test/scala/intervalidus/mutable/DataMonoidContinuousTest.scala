package intervalidus.mutable

import intervalidus.ContinuousValue.IntContinuousValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataMonoidContinuousTest extends AnyFunSuite with Matchers with MutableMonoidBaseBehaviors:
  testsFor(commonBehaviors("Continuous"))
