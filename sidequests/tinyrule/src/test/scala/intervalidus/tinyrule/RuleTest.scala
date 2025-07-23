package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class RuleTest extends AnyFunSuite with Matchers:

  import Attribute.*
  import MatchType.*

  private val fact1b = Fact("fact1b", "myBoolean" is false)
  private val fact2b = Fact("fact2b", "myBoolean" is true)
  private val fact1i = Fact("fact1i", "myInt" is 0)
  private val fact2i = Fact("fact2i", "myInt" is 1)
  private val fact1d = Fact("fact1d", "myDouble" is 0.0)
  private val fact2d = Fact("fact2d", "myDouble" is 1.0)

  test("Constructing and applying BooleanRule"):
    val booleanRule1 = Equals("myBoolean" is false)
    assert(booleanRule1.isInstanceOf[AttributeRule[Boolean]])
    assert(booleanRule1(fact1b))
    assert(!booleanRule1(fact2b))

    assert(!GreaterThan("myBoolean", false).apply(fact2b)) // not supported
    assert(!LessThan("myBoolean", true).apply(fact1b)) // not supported
    assert(!Contains("myBoolean", true).apply(fact2b)) // not supported

  test("Constructing and applying IntRule"):
    val intRule1 = Equals("myInt" is 0)
    assert(intRule1.isInstanceOf[AttributeRule[Int]])
    assert(intRule1(fact1i))
    assert(!intRule1(fact2i))
    assert(!intRule1(fact1d)) // names and types don't match

    val intRule2: Rule = AttributeRule(GreaterThan, "myInt" is 0)
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
    val doubleRule1 = Equals("myDouble" is 0.0)
    assert(doubleRule1.isInstanceOf[AttributeRule[Double]])
    assert(doubleRule1(fact1d))
    assert(!doubleRule1(fact2d))
    assert(!doubleRule1(fact1i)) // names and types don't match

    val doubleRule2: Rule = AttributeRule(GreaterThan, "myDouble" is 0.0)
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
    assert(stringRule1.isInstanceOf[AttributeRule[String]])
    assert(!stringRule1(Fact("create", "myString" is "create")))

    val stringRule2 = "myString" attributeGreaterThan "eat"
    assert(!stringRule2(Fact("create", "myString" is "create")))

    val stringRule3 = "myString" attributeLessThan "eat"
    assert(stringRule3(Fact("create", "myString" is "create")))

    val stringRule4 = "myString" attributeContains "eat"
    assert(stringRule4(Fact("create", "myString" is "create")))

  test("Constructing and applying DateRule"):
    val today = LocalDate.now()
    val tomorrow = today.plusDays(1)
    val yesterday = today.minusDays(1)
    val dateRule1 = "myDate" attributeGreaterThan today
    assert(dateRule1.isInstanceOf[AttributeRule[LocalDate]])
    assert(dateRule1(Fact("tomorrow", "myDate" is tomorrow)))
    assert(!dateRule1(Fact("yesterday", "myDate" is yesterday)))

    val dateRule2 = "myDate" attributeLessThan today
    assert(!dateRule2(Fact("tomorrow", "myDate" is tomorrow)))
    assert(dateRule2(Fact("yesterday", "myDate" is yesterday)))

    val dateRule3 = "myDate" attributeEquals today
    assert(!dateRule3(Fact("tomorrow", "myDate" is tomorrow)))
    assert(!dateRule3(Fact("yesterday", "myDate" is yesterday)))
    assert(dateRule3(Fact("yesterday", "myDate" is today)))

    val dateRule4 = "myDate" attributeContains today // not supported
    assert(!dateRule4(Fact("tomorrow", "myDate" is today)))

  test("Combining and applying"):
    val intRule1 = Equals("myInt", 0)
    val intRule2: Rule = AttributeRule(GreaterThan, "myInt" is 0)
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
