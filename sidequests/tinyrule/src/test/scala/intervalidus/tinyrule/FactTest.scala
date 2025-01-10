package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class FactTest extends AnyFunSuite with Matchers:

  // all supported types and sets of types
  case class BigStuff(
    myBoolean: Boolean,
    myInt: Int,
    myString: String,
    myDouble: Double,
    myDate: LocalDate,
    maybeBoolean: Option[Boolean],
    maybeInt: Option[Int],
    maybeString: Option[String],
    maybeDouble: Option[Double],
    maybeDate: Option[LocalDate],
    myBooleans: Set[Boolean],
    myInts: Set[Int],
    myStrings: Set[String],
    myDoubles: Set[Double],
    myDates: Set[LocalDate]
  )

  test("To and from Tuples"):
    val today = LocalDate.now()
    val tomorrow = today.plusDays(1)

    val test1: BigStuff =
      BigStuff(
        true,
        1,
        "hello",
        1.5,
        today,
        Some(true),
        Some(1),
        Some("hello"),
        Some(1.5),
        Some(today),
        Set(true, false),
        Set(1, 2, 3),
        Set("hello", "world"),
        Set(1.5, 1.6),
        Set(today, tomorrow)
      )
    val expectedAttributes1 = Set(
      Attribute("myBoolean", true),
      Attribute("myInt", 1),
      Attribute("myString", "hello"),
      Attribute("myDouble", 1.5),
      Attribute("myDate", today),
      Attribute("maybeBoolean", true),
      Attribute("maybeInt", 1),
      Attribute("maybeString", "hello"),
      Attribute("maybeDouble", 1.5),
      Attribute("maybeDate", today),
      Attribute("myBooleans", true),
      Attribute("myBooleans", false),
      Attribute("myBooleans", true),
      Attribute("myBooleans", false),
      Attribute("myInts", 1),
      Attribute("myInts", 2),
      Attribute("myInts", 3),
      Attribute("myStrings", "hello"),
      Attribute("myStrings", "world"),
      Attribute("myDoubles", 1.5),
      Attribute("myDoubles", 1.6),
      Attribute("myDates", today),
      Attribute("myDates", tomorrow)
    )
    val fact1 = Fact.from(test1)
    fact1.attributes shouldBe expectedAttributes1
    assert(fact1.id.startsWith("BigStuff"))
    val back1 = fact1.to[BigStuff]
    test1 shouldBe back1

    val test2: BigStuff =
      BigStuff(
        true,
        1,
        "hello",
        1.5,
        LocalDate.now(),
        None,
        None,
        None,
        None,
        None,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty
      )
    val expectedAttributes2 = Set(
      Attribute("myBoolean", true),
      Attribute("myInt", 1),
      Attribute("myString", "hello"),
      Attribute("myDouble", 1.5),
      Attribute("myDate", today)
    )
    val fact2 = Fact.from(test2)
    fact2.attributes shouldBe expectedAttributes2
    val back2 = fact2.to[BigStuff]
    test2 shouldBe back2

    val fact3 = fact2.copy(attributes = fact2.attributes.filterNot(_.name == "myBoolean"))
    assertThrows[Exception](fact3.to[BigStuff]) // missing attribute

  test("Basic operations"):
    val today = LocalDate.now()
    val fact1 = Fact("fact1", Attribute("greeting", "hello world"), Attribute("formal", false))
    fact1.id shouldBe "fact1"
    fact1.withId shouldBe ("fact1", fact1)
    fact1.attributes shouldBe Set(
      StringAttribute("greeting", "hello world"),
      BooleanAttribute("formal", false)
    )
    (fact1 - "greeting").attributes shouldBe Set(BooleanAttribute("formal", false))
    (fact1 + Attribute("informal", true)).attributes shouldBe Set(
      StringAttribute("greeting", "hello world"),
      BooleanAttribute("formal", false),
      BooleanAttribute("informal", true)
    )
    fact1.toString shouldBe
      """Fact fact1, attributes:
        | - greeting -> hello world,
        | - formal -> false
        |""".stripMargin.replaceAll("\r", "")
    val fact2 = Fact("fact2", Attribute("greeting", "Hola Mundo"), Attribute("updated", today))
    fact1.merge(fact2).attributes shouldBe Set(
      StringAttribute("greeting", "hello world"),
      StringAttribute("greeting", "Hola Mundo"),
      BooleanAttribute("formal", false),
      DateAttribute("updated", today)
    )

    fact1.merge(fact2, FactMergeStyle.WhenAbsent).attributes shouldBe Set(
      StringAttribute("greeting", "hello world"),
      BooleanAttribute("formal", false),
      DateAttribute("updated", today)
    )
    fact1.merge(fact2, FactMergeStyle.AsReplacement).attributes shouldBe Set(
      StringAttribute("greeting", "Hola Mundo"),
      BooleanAttribute("formal", false),
      DateAttribute("updated", today)
    )

    val fact2rhs = Fact("fact2", Attribute("greeting", "HELLO AGAIN"))
    val fact3rhs = Fact("fact3", Attribute("sad", true))

    val mergedFacts = Fact.mergeAll(Set(fact1, fact2), Set(fact2rhs, fact3rhs))
    mergedFacts.filter(_.id == "fact1") shouldBe Set(fact1) // lhs only
    mergedFacts.filter(_.id == "fact3") shouldBe Set(fact3rhs) // rhs only
    mergedFacts.filter(_.id == "fact2").flatMap(_.attributes) shouldBe Set( // actually merged
      StringAttribute("greeting", "Hola Mundo"),
      StringAttribute("greeting", "HELLO AGAIN"),
      DateAttribute("updated", today)
    )
