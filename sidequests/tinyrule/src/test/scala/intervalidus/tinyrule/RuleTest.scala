package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class RuleTest extends AnyFunSuite with Matchers:

  import MatchType.*

  private val fact1b = Fact("fact1b", Attribute("myBoolean", false))
  private val fact2b = Fact("fact2b", Attribute("myBoolean", true))
  private val fact1i = Fact("fact1i", Attribute("myInt", 0))
  private val fact2i = Fact("fact2i", Attribute("myInt", 1))
  private val fact1d = Fact("fact1d", Attribute("myDouble", 0.0))
  private val fact2d = Fact("fact2d", Attribute("myDouble", 1.0))

  test("Constructing and applying BooleanRule"):
    val booleanRule1 = Equals("myBoolean", false)
    assert(booleanRule1.isInstanceOf[BooleanRule])
    assert(booleanRule1(fact1b))
    assert(!booleanRule1(fact2b))

    assert(!GreaterThan("myBoolean", false)(fact2b)) // not supported
    assert(!LessThan("myBoolean", true)(fact1b)) // not supported
    assert(!Contains("myBoolean", true)(fact2b)) // not supported

  test("Constructing and applying IntRule"):
    val intRule1 = Equals("myInt", 0)
    assert(intRule1.isInstanceOf[IntRule])
    assert(intRule1(fact1i))
    assert(!intRule1(fact2i))
    assert(!intRule1(fact1d)) // names and types don't match

    val intRule2: Rule = AttributeRule(GreaterThan, Attribute("myInt", 0))
    assert(!intRule2(fact1i))
    assert(intRule2(fact2i))
    assert(!intRule2(fact1d))

    val intRule3 = LessThan("myInt", 1)
    assert(intRule3(fact1i))
    assert(!intRule3(fact2i))
    assert(!intRule3(fact1d))

    val intRule4 = Contains("myInt", 1)
    assert(!intRule4(fact2i)) // IntRule does not support Contains

  test("Constructing and applying DoubleRule"):
    val doubleRule1 = Equals("myDouble", 0.0)
    assert(doubleRule1.isInstanceOf[DoubleRule])
    assert(doubleRule1(fact1d))
    assert(!doubleRule1(fact2d))
    assert(!doubleRule1(fact1i)) // names and types don't match

    val doubleRule2: Rule = AttributeRule(GreaterThan, Attribute("myDouble", 0.0))
    assert(!doubleRule2(fact1d))
    assert(doubleRule2(fact2d))
    assert(!doubleRule2(fact1i))

    val doubleRule3 = LessThan("myDouble", 1.0)
    assert(doubleRule3(fact1d))
    assert(!doubleRule3(fact2d))
    assert(!doubleRule3(fact1i))

    val doubleRule4 = Contains("myDouble", 1.0)
    assert(!doubleRule4(fact2d)) // DoubleRule does not support Contains

  test("Constructing and applying StringRule"):
    val stringRule1 = "myString" attributeEquals "eat"
    assert(stringRule1.isInstanceOf[StringRule])
    assert(!stringRule1(Fact("create", Attribute("myString", "create"))))

    val stringRule2 = "myString" attributeGreaterThan "eat"
    assert(!stringRule2(Fact("create", Attribute("myString", "create"))))

    val stringRule3 = "myString" attributeLessThan "eat"
    assert(stringRule3(Fact("create", Attribute("myString", "create"))))

    val stringRule4 = "myString" attributeContains "eat"
    assert(stringRule4(Fact("create", Attribute("myString", "create"))))

  test("Constructing and applying DateRule"):
    val today = LocalDate.now()
    val tomorrow = today.plusDays(1)
    val yesterday = today.minusDays(1)
    val dateRule1 = "myDate" attributeGreaterThan today
    assert(dateRule1.isInstanceOf[DateRule])
    assert(dateRule1(Fact("tomorrow", Attribute("myDate", tomorrow))))
    assert(!dateRule1(Fact("yesterday", Attribute("myDate", yesterday))))

    val dateRule2 = "myDate" attributeLessThan today
    assert(!dateRule2(Fact("tomorrow", Attribute("myDate", tomorrow))))
    assert(dateRule2(Fact("yesterday", Attribute("myDate", yesterday))))

    val dateRule3 = "myDate" attributeEquals today
    assert(!dateRule3(Fact("tomorrow", Attribute("myDate", tomorrow))))
    assert(!dateRule3(Fact("yesterday", Attribute("myDate", yesterday))))
    assert(dateRule3(Fact("yesterday", Attribute("myDate", today))))

    val dateRule4 = "myDate" attributeContains today // not supported
    assert(!dateRule4(Fact("tomorrow", Attribute("myDate", today))))

  test("Combining and applying"):
    val intRule1 = Equals("myInt", 0)
    val intRule2: Rule = AttributeRule(GreaterThan, Attribute("myInt", 0))
    val orRule: OrRule = intRule1 or intRule2

    assert(orRule(fact1i))
    assert(orRule(fact2i))
    assert(!orRule(fact1d))

    val andRule1: AndRule = intRule1 and intRule2
    assert(!andRule1(fact1i))
    assert(!andRule1(fact2i))
    assert(!andRule1(fact1d))

    val andRule2: AndRule = intRule1 and Rule.not(intRule2) and Rule.always
    assert(andRule2(fact1i))
    assert(!andRule2(fact2i))
    assert(!andRule2(fact1d))
