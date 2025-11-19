package intervalidus.immutable

import intervalidus.CurrentInstant
import intervalidus.VariableBase.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate.of as date
import java.time.{Instant, ZoneOffset}
import scala.language.implicitConversions

class VariableTest extends AnyFunSuite with Matchers:
  test("Immutable: Variable functions"):
    val time0 = date(2025, 11, 18).atTime(12, 15).toInstant(ZoneOffset.UTC)
    val time1 = time0.plusSeconds(10)
    val time2 = time1.plusNanos(10)
    def slightlyBefore(d: Instant): Instant = d.minusNanos(5)

    val fixture0 = Variable("Hello")
    assert(fixture0.lastChange.isEmpty)
    fixture0.get shouldBe "Hello"
    fixture0.toString shouldBe "Hello"
    fixture0(time1) shouldBe "Hello"
    fixture0(time2) shouldBe "Hello"
    fixture0.getPrior shouldBe None

    given CurrentInstant = CurrentInstant.simulated(time1)
    val fixture1 = fixture0.set("World")
    fixture1.lastChange shouldBe Some(time1)
    fixture1.get shouldBe "World"
    fixture1.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture1(time1) shouldBe "World"
    fixture1.getPrior shouldBe Some("Hello")

    val fixture2 = fixture1.set("Hello World!")(using CurrentInstant.simulated(time1)) // conflict
    fixture2.lastChange shouldBe Some(time2) // bumped to resolve conflict
    fixture2.get shouldBe "Hello World!"
    fixture2.getAt(slightlyBefore(time2)) shouldBe "World"
    fixture2.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture2.getPrior shouldBe Some("World")
    fixture2.history.values should contain theSameElementsAs Seq("Hello", "World", "Hello World!")

    val fixture3 = fixture2.unset() match
      case Some(newFixture) => newFixture
      case None             => fail("expected unset to reverse a change")
    fixture3.lastChange shouldBe Some(time1)
    fixture3.get shouldBe "World"
    fixture3.getAt(slightlyBefore(time2)) shouldBe "World"
    fixture3.getAt(slightlyBefore(time1)) shouldBe "Hello"
    fixture3.getPrior shouldBe Some("Hello")

    // out-of-order update
    val fixture4 = fixture3.set("!")(using CurrentInstant.simulated(slightlyBefore(time1)))
    fixture4.lastChange shouldBe Some(time2)

    val fixture5 = fixture4.reset()
    assert(fixture5.lastChange.isEmpty)
    fixture5.get shouldBe "!"
    fixture5.getAt(slightlyBefore(time2)) shouldBe "!"
    fixture5.getAt(slightlyBefore(time1)) shouldBe "!"
    fixture5.getAt(slightlyBefore(time0)) shouldBe "!"
    fixture5.getPrior shouldBe None

    fixture5.unset() match
      case Some(_) => fail("expected unset not to reverse a change")
      case None    => succeed // nothing to unset
