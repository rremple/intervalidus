package intervalidus.mutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DVersionedTest extends AnyFunSuite with Matchers with DataIn1DVersionedBaseBehaviors:

  import DataIn1DBase.ValidData1D
  import DataIn1DVersionedBase.VersionSelection
  import DataIn2DBase.ValidData2D as ValidDataIn2D
  import DiscreteInterval1D.*

  // increment current version with each data element
  def newDataIn1DVersioned(allData: Iterable[ValidData1D[String, Int]]): DataIn1DVersioned[String, Int] =
    val dataIn1DVersioned = DataIn1DVersioned.from[String, Int]()
    allData.foreach: validData =>
      dataIn1DVersioned.set(validData)
      dataIn1DVersioned.incrementCurrentVersion()
    dataIn1DVersioned

  // shared
  testsFor(stringLookupTests(newDataIn1DVersioned, DataIn1DVersioned(_), DataIn1DVersioned.of(_)))

  test("Adding and removing data in intervals"):
    val empty: DataIn1DVersioned[String, Int] = immutable.DataIn1DVersioned().toMutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(DiscreteDomain1D.Top)

    assertThrows[Exception]: // version too small
      empty.setCurrentVersion(DiscreteDomain1D.Bottom)

    empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      empty.incrementCurrentVersion()

    val allData = testData("Hello" -> interval(0, 9), "World" -> intervalFrom(10))
    val fixture = newDataIn1DVersioned(allData)

    fixture.set("to", interval(5, 15))
    fixture.incrementCurrentVersion()
    val expectedData1 = testData("Hello" -> interval(0, 4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData1

    fixture.set("!", interval(20, 25)) // split
    fixture.incrementCurrentVersion()
    val expectedData2 = testData(
      "Hello" -> interval(0, 4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!" -> interval(20, 25),
      "World" -> intervalFrom(26)
    )
    fixture.getAll.toList shouldBe expectedData2

    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 4
        || 0 .. 4        | 5 .. 9        | 10 .. 15      | 16 .. 19      | 20 .. 25      | 26 .. +∞      |
        || Hello [0..1]  |
        |                                | World [1..1]  |
        |                                                                | World [2..2]  |
        || Hello [2..+∞) |
        |                | to [2..+∞)    |
        |                                                | World [2..+∞) |
        |                                                                | ! [3..+∞)     |
        |                                                                                | World [2..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    val copyFixture2 = fixture.copy

    assert(!fixture.setIfNoConflict("Hey", intervalTo(4)))
    assert(fixture.setIfNoConflict("Hey", intervalTo(-1)))
    fixture.incrementCurrentVersion()

    fixture.set("Hey", intervalTo(4))
    fixture.remove(intervalFrom(21))
    fixture.incrementCurrentVersion()
    val expectedData3 =
      testData("Hey" -> intervalTo(4), "to" -> interval(5, 15), "World" -> interval(16, 19), "!" -> intervalAt(20))
    fixture.getAll.toList shouldBe expectedData3

    fixture.set("World", intervalFrom(20))
    fixture.incrementCurrentVersion()
    val expectedData4 = testData("Hey" -> intervalTo(4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData4

    val copyFixture4 = fixture.copy

    fixture.remove(interval(5, 15))
    fixture.incrementCurrentVersion()
    val expectedData5 = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData5

    fixture.set("remove me", intervalFrom(1))
    fixture.remove(intervalFrom(1))
    val expectedData6 = testData("Hey" -> intervalTo(0))
    fixture.getAll.toList shouldBe expectedData6

    val copyFixture6 = fixture.copy

    import DataIn2DBase.DiffAction2D.*

    val actionsFrom2To4 = copyFixture4.diffActionsFrom(copyFixture2)
    actionsFrom2To4.toList shouldBe List(
      Create(ValidDataIn2D("Hey", DiscreteInterval2D(intervalTo(-1), intervalAt(4)))),
      Create(ValidDataIn2D("Hey", DiscreteInterval2D(intervalTo(4), intervalFrom(5)))),
      Update(ValidDataIn2D("Hello", DiscreteInterval2D(interval(0, 4), interval(2, 4)))),
      Update(ValidDataIn2D("!", DiscreteInterval2D(interval(20, 25), interval(3, 4)))),
      Create(ValidDataIn2D("!", DiscreteInterval2D(intervalAt(20), intervalAt(5)))),
      Create(ValidDataIn2D("World", DiscreteInterval2D(intervalFrom(20), intervalFrom(6)))),
      Update(ValidDataIn2D("World", DiscreteInterval2D(intervalFrom(26), interval(2, 4))))
    )
    val actionsFrom4To6 = copyFixture6.diffActionsFrom(copyFixture4)
    actionsFrom4To6.toList shouldBe List(
      Update(ValidDataIn2D("Hey", DiscreteInterval2D(intervalTo(4), interval(5, 7)))),
      Create(ValidDataIn2D("Hey", DiscreteInterval2D(intervalTo(0), intervalFrom(8)))),
      Update(ValidDataIn2D("to", DiscreteInterval2D(interval(5, 15), interval(2, 6)))),
      Update(ValidDataIn2D("World", DiscreteInterval2D(interval(16, 19), interval(2, 7)))),
      Update(ValidDataIn2D("World", DiscreteInterval2D(intervalFrom(20), interval(6, 7))))
    )
    copyFixture2.applyDiffActions(actionsFrom2To4)
    copyFixture2.getAll(using VersionSelection(copyFixture4.getCurrentVersion)).toList shouldBe expectedData4

    copyFixture2.syncWith(copyFixture6)
    copyFixture2.getAll(using VersionSelection(copyFixture6.getCurrentVersion)).toList shouldBe expectedData6

  test("Mapping, flat mapping, etc."):
    val allData = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))

    val fixture = newDataIn1DVersioned(allData)
    fixture.copy.getAll.toList shouldBe fixture.getAll.toList

    val concat = fixture.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.horizontal.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture.map(d =>
      ValidDataIn2D(
        d.value + "!",
        d.interval.withHorizontalUpdate(_.endingWith(d.interval.horizontal.end.successor))
      )
    )
    val expectedData2 = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap(d => DataIn1DVersioned(Seq(d)))
    val expectedData4 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.filter(_.interval.horizontal ⊆ intervalTo(10))
    val expectedData5 = testData("Hey!!!" -> intervalTo(5))
    fixture.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataIn1DVersioned.of[String, Int](d.value))
    val expectedData6 = testData("Hey!!!" -> unbounded)
    fixture.getAll.toList shouldBe expectedData6
    fixture.get shouldBe "Hey!!!"

    fixture.filter(_.value == "Planet")
    assert(fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

  test("Compressing data in intervals"):
    val allData = testData(
      "Hello" -> intervalTo(4),
      "World" -> intervalAt(5),
      "World" -> intervalAt(6),
      "Hello" -> intervalAt(7),
      "Hello" -> interval(8, 9),
      "Hello" -> intervalFrom(10)
    )

    val fixture1 = DataIn1DVersioned.from(allData)
    fixture1.compress("Hello")
    val expectedData1 =
      testData("Hello" -> intervalTo(4), "World" -> intervalAt(5), "World" -> intervalAt(6), "Hello" -> intervalFrom(7))
    fixture1.getDataIn1D.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn1DVersioned.from(allData)
    fixture2.compressAll()
    val expectedData2 = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    fixture2.getDataIn1D.getAll.toList shouldBe expectedData2

  test("Updating data in intervals"):
    val one: DataIn1DVersioned[String, Int] = DataIn1DVersioned.of("value")
    one.incrementCurrentVersion()

    one.remove(intervalAt(0)) // split
    val expectedData0 = testData("value" -> intervalTo(-1), "value" -> intervalFrom(1))
    one.getAll.toList shouldBe expectedData0
    one.getAt(0) shouldBe None
    one.getAt(1) shouldBe Some("value")

    val allData = testData("Hello" -> intervalTo(4), "World" -> interval(5, 6), "Hello" -> intervalFrom(7))
    val fixture = newDataIn1DVersioned(allData)

    fixture.update("World!", interval(5, 7))
    fixture.incrementCurrentVersion()
    val expectedData1 = testData("Hello" -> intervalTo(4), "World!" -> interval(5, 7), "Hello" -> intervalFrom(8))
    fixture.getAll.toList shouldBe expectedData1

    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 4
        || -∞ .. 4        | 5 .. 6         | 7 .. 7         | 8 .. +∞        |
        |                 | World [1..2]   |
        |                                  | Hello [2..2]   |
        || Hello [0..+∞)  |
        |                 | World! [3..+∞) |
        |                                                   | Hello [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")

    val fixtureToReset = fixture.copy
    fixtureToReset.remove(intervalTo(4))(using VersionSelection(1))
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 4
        || -∞ .. 4        | 5 .. 6         | 7 .. 7         | 8 .. +∞        |
        || Hello [0..0]   |
        |                 | World [1..2]   |
        |                                  | Hello [2..2]   |
        |                 | World! [3..+∞) |
        |                                                   | Hello [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")
    fixtureToReset.resetToVersion(2)
    // println(fixtureReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 2
        || -∞ .. 4       | 5 .. 6        | 7 .. +∞       |
        || Hello [0..0]  |
        |                | World [1..+∞) |
        |                                | Hello [2..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getDataIn1D(VersionSelection(2)).toString)
    fixture.getDataIn1D(using VersionSelection(2)).toString shouldBe
      """|| -∞ .. 4 | 5 .. 6  | 7 .. +∞ |
         || Hello   |
         |          | World   |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getDataIn1D.toString)
    fixture.getDataIn1D.toString shouldBe
      """|| -∞ .. 4 | 5 .. 7  | 8 .. +∞ |
         || Hello   |
         |          | World!  |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    fixture.update("Hello", interval(5, 6))
    fixture.incrementCurrentVersion()
    val expectedData2 = testData("Hello" -> intervalTo(6), "World!" -> intervalAt(7), "Hello" -> intervalFrom(8))
    fixture.getAll.toList shouldBe expectedData2

    fixture.update("World!", intervalFrom(6))
    fixture.incrementCurrentVersion()
    val expectedData3 = testData("Hello" -> intervalTo(5), "World!" -> intervalFrom(6))
    fixture.getAll.toList shouldBe expectedData3

    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 6
        || -∞ .. 4        | 5 .. 5         | 6 .. 6         | 7 .. 7         | 8 .. +∞        |
        |                 | World [1..2]   |
        |                                                   | Hello [2..2]   |
        |                 | World! [3..3]  |
        |                 | Hello [4..4]   |
        |                                                   | World! [4..4]  |
        |                                                                    | Hello [3..4]   |
        || Hello [0..+∞)  |
        |                 | Hello [5..+∞)  |
        |                                  | World! [5..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    fixture.collapseVersionHistory
    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 0
        || -∞ .. 5        | 6 .. +∞        |
        || Hello [0..+∞)  |
        |                 | World! [0..+∞) |
        |""".stripMargin.replaceAll("\r", "")
