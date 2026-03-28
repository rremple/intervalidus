package intervalidus.immutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataMonoidContinuousTest
  extends AnyFunSuite
  with Matchers
  with ImmutableMonoidBaseBehaviors(using ContinuousValue.IntContinuousValue):
  testsFor(commonBehaviors("Continuous"))
