package intervalidus.mutable

import intervalidus.DiscreteValue.IntDiscreteValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataMonoidDiscreteTest extends AnyFunSuite with Matchers with MutableMonoidBaseBehaviors:
  testsFor(commonBehaviors("Discrete"))
