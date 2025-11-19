package intervalidus.mutable

import intervalidus.CurrentInstant
import intervalidus.VariableBase.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate.of as date
import java.time.{Instant, ZoneOffset}
import scala.language.implicitConversions

class VariableTest extends AnyFunSuite with Matchers:
  test("Mutable: Variable functions"):
    val time0 = date(2025, 11, 18).atTime(12, 15).toInstant(ZoneOffset.UTC)
    val time1 = time0.plusSeconds(10)
    val time2 = time1.plusNanos(10)
    def slightlyBefore(d: Instant): Instant = d.minusNanos(5)

    val fixture = Variable("Hello")
    assert(fixture.lastChange.isEmpty)
    fixture.get shouldBe "Hello"
    fixture.toString shouldBe "Hello"
    fixture(time1) shouldBe "Hello"
    fixture(time2) shouldBe "Hello"
    fixture.getPrior shouldBe None

    given CurrentInstant = CurrentInstant.simulated(time1)
    fixture.set("World")
    fixture.lastChange shouldBe Some(time1)
    fixture.get shouldBe "World"
    fixture.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture(time1) shouldBe "World"
    fixture.getPrior shouldBe Some("Hello")

    fixture.set("Hello World!")(using CurrentInstant.simulated(time1)) // conflict
    fixture.lastChange shouldBe Some(time2) // bumped to resolve conflict
    fixture.get shouldBe "Hello World!"
    fixture.getAt(slightlyBefore(time2)) shouldBe "World"
    fixture.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture.getPrior shouldBe Some("World")
    fixture.history.values should contain theSameElementsAs Seq("Hello", "World", "Hello World!")

    fixture.unset()
    fixture.lastChange shouldBe Some(time1)
    fixture.get shouldBe "World"
    fixture.getAt(slightlyBefore(time2)) shouldBe "World"
    fixture.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture.getPrior shouldBe Some("Hello")

    // out-of-order update
    fixture.set("!")(using CurrentInstant.simulated(slightlyBefore(time1)))
    fixture.lastChange shouldBe Some(time2)

    fixture.reset()
    assert(fixture.lastChange.isEmpty)
    fixture.get shouldBe "!"
    fixture.getAt(slightlyBefore(time2)) shouldBe "!"
    fixture.getAt(slightlyBefore(time1)) shouldBe "!"
    fixture.getAt(slightlyBefore(time0)) shouldBe "!"
    fixture.getPrior shouldBe None

    val historyBefore = fixture.history
    fixture.unset()
    fixture.history shouldBe historyBefore // nothing to unset
