package intervalidus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class VariableBaseTest extends AnyFunSuite with Matchers:
  test("VariableBase functions"):
    val fixture = VariableBase.InstantDiscreteValue

    def dateFromMillis(m: Long): Instant = Instant.ofEpochMilli(m)

    val minHashable = dateFromMillis(Long.MinValue)
    val maxHashable = dateFromMillis(Long.MaxValue)
    fixture.orderedHashOf(minHashable.minusSeconds(1)) shouldBe Long.MinValue
    assert(fixture.orderedHashOf(minHashable.plusSeconds(1)) > Long.MinValue)
    assert(fixture.orderedHashOf(maxHashable.minusSeconds(1)) < Long.MaxValue)
    fixture.orderedHashOf(maxHashable.plusSeconds(1)) shouldBe Long.MaxValue

    fixture.predecessorOf(Instant.MIN) shouldBe None
    assert(fixture.predecessorOf(Instant.MIN.plusSeconds(1)).isDefined)
    assert(fixture.successorOf(Instant.MAX.minusSeconds(1)).isDefined)
    fixture.successorOf(Instant.MAX) shouldBe None
