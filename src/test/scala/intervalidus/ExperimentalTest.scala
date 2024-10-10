package intervalidus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentalTest extends AnyFunSuite with Matchers:

  test("Experimental functions"):
    val fixtureAll = Experimental.all
    assert(fixtureAll.enabled("anyFeature"))
    fixtureAll.control("anyFeature")("nonExperimental", "experimental") shouldBe "experimental"
    fixtureAll.parallelCheck("anyFeature")("one", "one")() shouldBe "one"
    fixtureAll.parallelCheck("anyFeature")("one", "not one")((_, _) => "failed") shouldBe "failed"
    assertThrows[Exception]:
      val _ = fixtureAll.parallelCheck("anyFeature")("one", "not one")()

    val fixtureAllExcept = Experimental.allExcept("thatFeature")
    assert(fixtureAllExcept.enabled("thisFeature"))
    assert(!fixtureAllExcept.enabled("thatFeature"))
    fixtureAllExcept.control("thisFeature")("nonExperimental", "experimental") shouldBe "experimental"
    fixtureAllExcept.control("thatFeature")("nonExperimental", "experimental") shouldBe "nonExperimental"
    fixtureAllExcept.parallelCheck("thatFeature")("one", "two")() shouldBe "one"

    val fixtureNone = Experimental.none
    assert(!fixtureNone.enabled("anyFeature"))
    fixtureNone.control("anyFeature")("nonExperimental", "experimental") shouldBe "nonExperimental"
    fixtureNone.parallelCheck("anyFeature")("one", "two")() shouldBe "one"

    val fixtureSome = Experimental("thisFeature")
    assert(fixtureSome.enabled("thisFeature"))
    assert(!fixtureSome.enabled("thatFeature"))
    fixtureSome.control("thisFeature")("nonExperimental", "experimental") shouldBe "experimental"
    fixtureSome.control("thatFeature")("nonExperimental", "experimental") shouldBe "nonExperimental"
    fixtureSome.parallelCheck("thisFeature")("one", "one")() shouldBe "one"
    fixtureSome.parallelCheck("thisFeature")("one", "not one")((_, _) => "failed") shouldBe "failed"
    fixtureSome.parallelCheck("thatFeature")("one", "not one")() shouldBe "one"

  test("Experimental implicits"):
    def select(first: => String, second: => String)(using experiment: Experimental): String =
      experiment.control("pickSecond")(first, second)

    def passThru(first: => String, second: => String)(using Experimental): String =
      select(first, second)

    val default = summon[Experimental]
    default.control("anyFeature")("nonExperimental", "experimental") shouldBe "nonExperimental"
    select("a", "b") shouldBe "a"
    select("a", "b")(using Experimental("pickSecond")) shouldBe "b"

    passThru("a", "b") shouldBe "a"
    passThru("a", "b")(using Experimental("pickSecond")) shouldBe "b"
