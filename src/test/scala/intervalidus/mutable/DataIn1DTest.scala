package intervalidus.mutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DTest extends AnyFunSuite with Matchers with DataIn1DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn1D(_), DataIn1D.of(_)))
  testsFor(doubleUseCaseTests("Mutable", DataIn1D(_)))

  test("Mutable: Adding and removing data in intervals"):
    val empty: DataIn1D[String, Int] = DataIn1D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = testData("Hello" -> interval(0, 9), "World" -> intervalFrom(10))
    val fixture = immutable.DataIn1D(allData).toImmutable.toMutable

    fixture.set(interval(5, 15) -> "to")
    val expectedData1 = testData("Hello" -> interval(0, 4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData1

    fixture.set(interval(20, 25) -> "!") // split
    val expectedData2 = testData(
      "Hello" -> interval(0, 4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!" -> interval(20, 25),
      "World" -> intervalFrom(26)
    )
    fixture.getAll.toList shouldBe expectedData2

    fixture.toString shouldBe
      """|| 0 .. 4   | 5 .. 15  | 16 .. 19 | 20 .. 25 | 26 .. +∞ |
         || Hello    |
         |           | to       |
         |                      | World    |
         |                                 | !        |
         |                                            | World    |
         |""".stripMargin.replaceAll("\r", "")

    val copyFixture2 = fixture.copy

    assert(!fixture.setIfNoConflict(intervalTo(4) -> "Hey"))
    assert(fixture.setIfNoConflict(intervalTo(-1) -> "Hey"))

    fixture.set(intervalTo(4) -> "Hey")
    fixture.remove(intervalFrom(21))
    val expectedData3 = testData(
      "Hey" -> intervalTo(4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!" -> intervalAt(20)
    )
    fixture.getAll.toList shouldBe expectedData3

    val f3a = fixture.copy
    f3a.replaceByKey(intervalTo(4).key, intervalTo(3) -> "Hello")
    f3a.replace(interval(16, 19) -> "World", interval(15, 20) -> "World!")
    val expectedData3a = testData(
      "Hello" -> intervalTo(3),
      "to" -> interval(5, 14),
      "World!" -> interval(15, 20)
    )
    f3a.getAll.toList shouldBe expectedData3a

    fixture.set(intervalFrom(20) -> "World")
    val expectedData4 = testData("Hey" -> intervalTo(4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData4

    val copyFixture4 = fixture.copy

    fixture.remove(interval(5, 15))
    val expectedData5 = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData5

    fixture.set(intervalFrom(1) -> "remove me")
    fixture.remove(intervalFrom(1))
    val expectedData6 = testData("Hey" -> intervalTo(0))
    fixture.getAll.toList shouldBe expectedData6

    val copyFixture6 = fixture.copy

    import DiffAction1D.*

    val actionsFrom2To4 = copyFixture4.diffActionsFrom(copyFixture2)
    actionsFrom2To4.toList shouldBe List(
      Create(intervalTo(4) -> "Hey"),
      Delete(0),
      Update(intervalFrom(16) -> "World"),
      Delete(20),
      Delete(26)
    )
    val actionsFrom4To6 = copyFixture6.diffActionsFrom(copyFixture4)
    actionsFrom4To6.toList shouldBe List(
      Update(intervalTo(0) -> "Hey"),
      Delete(5),
      Delete(16)
    )
    copyFixture2.applyDiffActions(actionsFrom2To4)
    copyFixture2.getAll.toList shouldBe expectedData4

    copyFixture2.syncWith(copyFixture6)
    copyFixture2.getAll.toList shouldBe expectedData6

  test("Mutable: Mapping, flatmapping, etc."):
    val allData = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))

    val fixture1 = DataIn1D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture1.map(d => d.interval.endingWith(d.interval.end.successor) -> (d.value + "!"))
    val expectedData2a = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(16))
    fixture1.getAll.toList shouldBe expectedData2a

    fixture1.map(d => d.interval.startingWith(d.interval.start.predecessor) -> d.value)
    val expectedData2b = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(15))
    fixture1.getAll.toList shouldBe expectedData2b

    fixture1.mapValues(_ + "!!")
    val expectedData3 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(15))
    fixture1.getAll.toList shouldBe expectedData3

    fixture1.flatMap(d => DataIn1D(Seq(d)))
    val expectedData4 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(15))
    fixture1.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture1.get

    fixture1.filter(_.interval ⊆ intervalTo(10))
    val expectedData5 = testData("Hey!!!" -> intervalTo(5))
    fixture1.getAll.toList shouldBe expectedData5
    assert(!fixture1.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture1.get

    fixture1.flatMap(d => DataIn1D.of[String, Int](d.value))
    val expectedData6 = testData("Hey!!!" -> unbounded)
    fixture1.getAll.toList shouldBe expectedData6
    fixture1.get shouldBe "Hey!!!"

    fixture1.filter(_.value == "Planet")
    assert(fixture1.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture1.get

  test("Mutable: Compressing data in intervals"):
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
    fixture1.compress("Hello")
    val expectedData1 = testData(
      "Hello" -> intervalTo(4),
      "World" -> intervalAt(5),
      "World" -> intervalAt(6),
      "Hello" -> intervalFrom(7)
    )
    fixture1.getAll.toList shouldBe expectedData1
    fixture1.domain.toList shouldBe List(unbounded[Int])

    val fixture2 = DataIn1D(allData)
    fixture2.compressAll()
    val expectedData2 = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(unbounded[Int])

  test("Mutable: Updating data in intervals"):
    val one: DataIn1D[String, Int] = DataIn1D.of("value")

    one.remove(intervalAt(0)) // split
    val expectedData0 = testData("value" -> intervalTo(-1), "value" -> intervalFrom(1))
    one.getAll.toList shouldBe expectedData0
    one.getAt(0) shouldBe None
    one.getAt(1) shouldBe Some("value")

    val allData = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    val fixture = DataIn1D(allData)

    fixture.update(interval(5, 7) -> "World!")
    val expectedData1 = testData("Hello" -> intervalTo(4), "World!" -> interval(5, 7), "Hello" -> intervalFrom(8))
    fixture.getAll.toList shouldBe expectedData1

    fixture.remove(interval(3, 5))
    fixture.update(interval(2, 9) -> "to")
    val expectedData2 = testData(
      "Hello" -> intervalTo(1),
      "to" -> intervalAt(2),
      "to" -> interval(6, 9),
      "Hello" -> intervalFrom(10)
    )
    fixture.getAll.toList shouldBe expectedData2
    fixture.domain.toList shouldBe List(intervalTo(2), intervalFrom(6))

    fixture.remove(interval(2, 9))
    fixture.update(interval(-5, -2) -> "World!")
    val expectedData3 = testData(
      "Hello" -> intervalTo(-6),
      "World!" -> interval(-5, -2),
      "Hello" -> interval(-1, 1),
      "Hello" -> intervalFrom(10)
    )
    fixture.getAll.toList shouldBe expectedData3
    fixture.domain.toList shouldBe List(intervalTo(1), intervalFrom(10))
