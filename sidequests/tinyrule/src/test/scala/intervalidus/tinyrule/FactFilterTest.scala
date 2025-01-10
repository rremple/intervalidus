package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FactFilterTest extends AnyFunSuite with Matchers:

  import FactFilter.*

  private val fact1 = Fact("fact1", Attribute("myInt", 0), Attribute("myDouble", 1.1))
  private val fact2 = Fact("fact2", Attribute("myInt", 0), Attribute("myDouble", 1.5))
  private val fact3 = Fact("fact3", Attribute("myInt", 1), Attribute("myDouble", 1.1))
  private val fact4 = Fact("fact4", Attribute("myInt", 1), Attribute("myDouble", 1.5))
  private val facts = Set(fact1, fact2, fact3, fact4)
  private val filter1 = filter("myInt" attributeEquals 0) // fact 1 and 2
  private val filter2 = filter("myDouble" attributeGreaterThan 1.3) // fact 2 and 4

  test("Simple filters"):
    filter1(facts) shouldBe Set(fact1, fact2)
    filter2(facts) shouldBe Set(fact2, fact4)
    val excludingFilter = filter1.excluding("fact1")
    excludingFilter(facts) shouldBe Set(fact2)

  test("Augmenting and redacting filters"):
    val greeting = Attribute("greeting", "hello world")
    val augmentFilter = ("myInt" attributeEquals 0).augment(Fact("augments", greeting))
    val augmentFilterAlt = AugmentFilter("myInt" attributeEquals 0, Fact("augments", greeting))
    augmentFilter shouldBe augmentFilterAlt
    val newFacts = augmentFilter(facts)
    newFacts shouldBe Set(fact1 + greeting, fact2 + greeting, fact3, fact4)
    val redactFilter = ("myDouble" attributeGreaterThan 1.4).redact("myInt")
    val expected = Set(fact1 + greeting, fact2 + greeting - "myInt", fact3, fact4 - "myInt")
    redactFilter(newFacts) shouldBe expected
    (augmentFilter and redactFilter)(facts) shouldBe expected
    val myInt10 = Attribute("myInt", 10)
    val augmentInt = Rule.always.augmentWhenAbsent(Fact("add myInt back", myInt10))
    val expectedInt = Set(fact1 + greeting, fact2 + greeting - "myInt" + myInt10, fact3, fact4 - "myInt" + myInt10)
    augmentInt(expected)

  test("Combining filters"):
    (filter1 and filter2)(facts) shouldBe Set(fact2)

    val filter1Redact = ("myInt" attributeEquals 0).redact("myDouble")
    val myBiggerDouble = Attribute("myDouble", 1.7)
    val filter2Aug = ("myDouble" attributeGreaterThan 1.3).augmentAsReplacement(Fact("augments", myBiggerDouble))

    val fact1Mod = fact1 - "myDouble"
    val fact2Mod = fact2 - "myDouble" + myBiggerDouble
    val fact4Mod = fact4 - "myDouble" + myBiggerDouble

    filter1Redact(facts) shouldBe Set(
      fact1Mod,
      fact2 - "myDouble",
      fact3,
      fact4
    )
    filter2Aug(facts) shouldBe Set(
      fact1,
      fact2Mod,
      fact3,
      fact4Mod
    )

    val orFilter = (filter1Redact or filter2Aug)
    val orFilterAlt = OrFactFilter(List(filter1Redact, filter2Aug))
    orFilter shouldBe orFilterAlt

    orFilter(facts) shouldBe
      Set(fact1, fact2Mod, fact3, fact4 + myBiggerDouble) // fact 4 keeps both values of myDouble
    (filter1Redact orWhenAbsent filter2Aug)(facts) shouldBe
      Set(fact1, fact2Mod, fact3, fact4) // fact 4 keeps the myDouble from filter 1
    (filter1Redact orAsReplacement filter2Aug)(facts) shouldBe
      Set(fact1, fact2Mod, fact3, fact4Mod) // fact 4 keeps the myDouble from filter 2
