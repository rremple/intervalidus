package intervalidus.immutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataMonoidDiscreteTest
  extends AnyFunSuite
  with Matchers
  with ImmutableMonoidBaseBehaviors(using DiscreteValue.IntDiscreteValue):
  testsFor(commonBehaviors("Discrete"))
