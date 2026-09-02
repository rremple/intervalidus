package intervalidus.immutable

import intervalidus.DiscreteValue.IntDiscreteValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataMonoidDiscreteTest extends AnyFunSuite with Matchers with ImmutableMonoidBaseBehaviors:
  testsFor(commonBehaviors("Discrete"))
