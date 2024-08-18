package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteDomain1D.Point
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DTest extends AnyFunSuite with Matchers with intervalidus.DataIn1DBaseBehaviors:

  import DataIn1DBase.ValidData1D
  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests(DataIn1D(_), DataIn1D.of(_)))
  testsFor(doubleUseCaseTests(DataIn1D(_)))

  test("Practical use case: tax brackets using zip"):
    val brackets = DataIn1D(taxBrackets)

    def taxUsingZip(income: Int): Double =
      val incomeInterval: DataIn1D[Unit, Int] = DataIn1D(Seq(ValidData1D((), interval(1, income))))
      val taxesByBracket = incomeInterval
        .zip(brackets)
        .getAll
        .map:
          case ValidData1D((_, rate), DiscreteInterval1D(Point(bottomInBracket), Point(topInBracket))) =>
            rate * (topInBracket - bottomInBracket + 1)
          case unexpected => fail(s"invalid bracket: $unexpected")
      taxesByBracket.sum

    val expectedTax = 0.1 * (23200 - 0) +
      0.12 * (94300 - 23200) +
      0.22 * (201050 - 94300) +
      0.24 * (250000 - 201050)

    taxUsingZip(250000) shouldBe expectedTax // 2320 + 8532 + 23485 + 11747

  test("Adding and removing data in intervals"):
    val empty: DataIn1D[String, Int] = DataIn1D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = testData("Hello" -> interval(0, 9), "World" -> intervalFrom(10))
    val f0 = mutable.DataIn1D(allData).toImmutable

    val f1 = f0.set("to", interval(5, 15))
    val expectedData1 = testData("Hello" -> interval(0, 4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    f1.getAll.toList shouldBe expectedData1

    val f2 = f1.set("!", interval(20, 25)) // split
    val expectedData2 = testData(
      "Hello" -> interval(0, 4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!"
        -> interval(20, 25),
      "World" -> intervalFrom(26)
    )
    f2.getAll.toList shouldBe expectedData2

    f2.toString shouldBe
      """|| 0 .. 4   | 5 .. 15  | 16 .. 19 | 20 .. 25 | 26 .. +∞ |
         || Hello    |
         |           | to       |
         |                      | World    |
         |                                 | !        |
         |                                            | World    |
         |""".stripMargin.replaceAll("\r", "")

    f2.setIfNoConflict("Hey", intervalTo(4)) shouldBe None
    val f3 = f2
      .setIfNoConflict("Hey", intervalTo(-1))
      .getOrElse(fail("Expected Some, got None"))
      .set("Hey", intervalTo(4))
      .remove(intervalFrom(21))
    val expectedData3 = testData(
      "Hey" -> intervalTo(4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!" ->
        intervalAt(20)
    )
    f3.getAll.toList shouldBe expectedData3

    val f4 = f3.set("World", intervalFrom(20))
    val expectedData4 = testData("Hey" -> intervalTo(4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    f4.getAll.toList shouldBe expectedData4

    val f5 = f4.remove(interval(5, 15))
    val expectedData5 = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))
    f5.getAll.toList shouldBe expectedData5

    val f6 = f5
      .set("remove me", intervalFrom(1))
      .remove(intervalFrom(1))
    val expectedData6 = testData("Hey" -> intervalTo(0))
    f6.getAll.toList shouldBe expectedData6

    import DataIn1DBase.DiffAction1D.*

    val actionsFrom2To4 = f4.diffActionsFrom(f2)
    actionsFrom2To4.toList shouldBe List(
      Create(ValidData1D("Hey", intervalTo(4))),
      Delete(0),
      Update(ValidData1D("World", intervalFrom(16))),
      Delete(20),
      Delete(26)
    )
    val actionsFrom4To6 = f6.diffActionsFrom(f4)
    actionsFrom4To6.toList shouldBe List(
      Update(ValidData1D("Hey", intervalTo(0))),
      Delete(5),
      Delete(16)
    )
    val f4sync = f2.applyDiffActions(actionsFrom2To4)
    f4sync.getAll.toList shouldBe expectedData4

    val f6sync = f2.syncWith(f6)
    f6sync.getAll.toList shouldBe expectedData6

  test("Mapping, flatmapping, etc."):
    val allData = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))

    val fixture1 = DataIn1D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 = fixture1.map(d => ValidData1D(d.value + "!", d.interval.endingWith(d.interval.end.successor)))
    val expectedData2 = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(16))
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3.flatMap(d => DataIn1D.of[String, Int](d.value).map(x => ValidData1D(x.value, d.interval)))
    val expectedData4 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.interval ⊆ intervalTo(10))
    val expectedData5 = testData("Hey!!!" -> intervalTo(5))
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataIn1D.of[String, Int](d.value))
    val expectedData6 = testData("Hey!!!" -> unbounded)
    fixture6.getAll.toList shouldBe expectedData6
    fixture6.get shouldBe "Hey!!!"

    val fixture7 = fixture6.filter(_.value == "Planet")
    assert(fixture7.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture7.get

  test("Compressing data in intervals"):
    val allData = testData(
      "Hello" -> intervalTo(4),
      "World" -> intervalAt(5),
      "World" -> intervalAt(6),
      "Hello" -> intervalAt(7),
      "Hello" -> interval(8, 9),
      "Hello" -> intervalFrom(10)
    )

    val fixture1 = DataIn1D(allData)
    fixture1.domain.toList shouldBe List(unbounded[Int])
    val compressed1 = fixture1.compress("Hello")
    val expectedData1 = testData(
      "Hello" -> intervalTo(4),
      "World" -> intervalAt(5),
      "World" -> intervalAt(6),
      "Hello" -> intervalFrom(7)
    )
    compressed1.getAll.toList shouldBe expectedData1
    compressed1.domain.toList shouldBe List(unbounded[Int])

    val fixture2 = DataIn1D(allData)
    val compressed2 = fixture2.compressAll()
    val expectedData2 = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    compressed2.getAll.toList shouldBe expectedData2
    compressed2.domain.toList shouldBe List(unbounded[Int])

  test("Updating data in intervals"):
    val one = DataIn1D.of("value").remove(intervalAt(0)) // split
    val expectedData0 = testData("value" -> intervalTo(-1), "value" -> intervalFrom(1))
    one.getAll.toList shouldBe expectedData0
    one.getAt(0) shouldBe None
    one.getAt(1) shouldBe Some("value")

    val allData = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    val f0 = DataIn1D(allData)

    val f1 = f0.update(ValidData1D("World!", interval(5, 7)))
    val expectedData1 = testData("Hello" -> intervalTo(4), "World!" -> interval(5, 7), "Hello" -> intervalFrom(8))
    f1.getAll.toList shouldBe expectedData1

    val f2 = f1
      .remove(interval(3, 5))
      .update("to", interval(2, 9))
    val expectedData2 = testData(
      "Hello" -> intervalTo(1),
      "to" -> intervalAt(2),
      "to" -> interval(6, 9),
      "Hello" -> intervalFrom(10)
    )
    f2.getAll.toList shouldBe expectedData2
    f2.domain.toList shouldBe List(intervalTo(2), intervalFrom(6))

    val f3 = f2
      .remove(interval(2, 9))
      .update("World!", interval(-5, -2))
    val expectedData3 = testData(
      "Hello" -> intervalTo(-6),
      "World!" -> interval(-5, -2),
      "Hello" -> interval(-1, 1),
      "Hello" -> intervalFrom(10)
    )
    f3.getAll.toList shouldBe expectedData3
    f3.domain.toList shouldBe List(intervalTo(1), intervalFrom(10))
