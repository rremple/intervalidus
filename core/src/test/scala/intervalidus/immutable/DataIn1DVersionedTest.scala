package intervalidus.immutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn1DVersionedTest extends AnyFunSuite with Matchers with DataIn1DVersionedBaseBehaviors:

  import DimensionalVersionedBase.VersionSelection
  import DiscreteInterval1D.*

  // increment current version with each data element
  def newDataIn1DVersioned(allData: Iterable[ValidData1D[String, Int]]): DataIn1DVersioned[String, Int] =
    allData.foldLeft(DataIn1DVersioned[String, Int]()): (dataIn1DVersioned, validData) =>
      dataIn1DVersioned
        .set(validData)
        .incrementCurrentVersion()

  // shared
  testsFor(stringLookupTests("Immutable", newDataIn1DVersioned, DataIn1DVersioned(_), DataIn1DVersioned.of(_)))
  {
    given Experimental = Experimental("noSearchTree")
    testsFor(
      stringLookupTests(
        "Immutable [experimental noSearchTree]",
        newDataIn1DVersioned,
        DataIn1DVersioned(_),
        DataIn1DVersioned.of(_)
      )
    )
  }

  test("Immutable: Adding and removing data in intervals"):
    val empty: DataIn1DVersioned[String, Int] = mutable.DataIn1DVersioned().toMutable.toImmutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(DiscreteDomain1D.Top)

    assertThrows[Exception]: // version too small
      empty.setCurrentVersion(DiscreteDomain1D.Bottom)

    val emptyAtMaxVersion = empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      emptyAtMaxVersion.incrementCurrentVersion()

    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val fixture1 = newDataIn1DVersioned(allData)
      .set(interval(5, 15) -> "to")
      .incrementCurrentVersion()
    val expectedData1 = List(interval(0, 4) -> "Hello", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .set(interval(20, 25) -> "!") // split
      .incrementCurrentVersion()
      .recompressAll()
    val expectedData2 = List(
      interval(0, 4) -> "Hello",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      interval(20, 25) -> "!",
      intervalFrom(26) -> "World"
    )
    fixture2.getAll.toList shouldBe expectedData2

    // println(fixture2.toString)
    fixture2.toString shouldBe
      """current version = 4
        || 0 .. 4        | 5 .. 9        | 10 .. 15      | 16 .. 19      | 20 .. 25      | 26 .. +∞      |
        || Hello [0..1]                  |
        |                                | World [1..1]                                                  |
        |                                                | World [2..2]                                  |
        || Hello [2..+∞) |
        |                | to [2..+∞)                    |
        |                                                | World [3..+∞) |
        |                                                                | ! [3..+∞)     |
        |                                                                                | World [3..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    fixture2.setIfNoConflict(intervalTo(4) -> "Hey") shouldBe None
    val fixture3 = fixture2.setIfNoConflict(intervalTo(-1) -> "Hey") match
      case Some(f) =>
        f.incrementCurrentVersion()
          .set(intervalTo(4) -> "Hey")
          .remove(intervalFrom(21))
          .incrementCurrentVersion()
      case None => fail("unexpected conflict")

    val expectedData3 = List(
      intervalTo(4) -> "Hey",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      intervalAt(20) -> "!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .set(intervalFrom(20) -> "World")
      .incrementCurrentVersion()
      .recompressAll()
    val expectedData4 = List(intervalTo(4) -> "Hey", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    fixture4.getAll.toList shouldBe expectedData4

    val fixture5 = fixture4
      .remove(interval(5, 15))
      .incrementCurrentVersion()
    val expectedData5 = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")
    fixture5.getAll.toList shouldBe expectedData5

    val fixture6 = fixture5
      .set(intervalFrom(1) -> "remove me")
      .remove(intervalFrom(1))
    // needed? .recompressAll()
    val expectedData6 = List(intervalTo(0) -> "Hey")
    fixture6.getAll.toList shouldBe expectedData6

    val fixture7 = fixture6.fill(DiscreteInterval1D.unbounded -> "Filled")
    val expectedFilled = List(intervalTo(0) -> "Hey", intervalFrom(1) -> "Filled")
    fixture7.getAll.toList shouldBe expectedFilled

    import DiffAction2D.*

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2)
    actionsFrom2To4.toList shouldBe List(
      Create((intervalTo(-1) x intervalFrom(4)) -> "Hey"),
      Update((interval(0, 4) x interval(2, 4)) -> "Hello"),
      Create((interval(0, 4) x intervalFrom(5)) -> "Hey"),
      Update((interval(20, 25) x interval(3, 4)) -> "!"),
      Create((intervalAt(20) x intervalAt(5)) -> "!"),
      Create((intervalFrom(20) x intervalFrom(6)) -> "World"),
      Update((intervalFrom(26) x interval(3, 4)) -> "World")
    )
    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6.toList shouldBe List(
      Update((interval(0, 4) x interval(5, 7)) -> "Hey"),
      Create((intervalAt(0) x intervalFrom(8)) -> "Hey"),
      Update((interval(5, 15) x interval(2, 6)) -> "to"),
      Update((interval(16, 19) x interval(3, 7)) -> "World"),
      Update((intervalFrom(20) x interval(6, 7)) -> "World")
    )
    val fixture2to4 = fixture2.applyDiffActions(actionsFrom2To4)
    fixture2to4.getAll(using VersionSelection(fixture4.getCurrentVersion)).toList shouldBe expectedData4

    val fixture2to6 = fixture2.syncWith(fixture6)
    fixture2to6.getAll(using VersionSelection(fixture6.getCurrentVersion)).toList shouldBe expectedData6

  test("Immutable: Mapping, flat mapping, etc."):
    val allData = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")

    val fixture1 = newDataIn1DVersioned(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.horizontal.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 = fixture1.map(d =>
      d.interval.withHorizontalUpdate(_.endingWith(d.interval.horizontal.end.successor)) -> (d.value + "!")
    )
    val expectedData2 = List(intervalTo(5) -> "Hey!", intervalFrom(16) -> "World!")
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .flatMap(d => DataIn1DVersioned.of[String, Int](d.value).map(x => d.interval -> x.value))
    val expectedData4 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.interval.horizontal ⊆ intervalTo(10))
    val expectedData5 = List(intervalTo(5) -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataIn1DVersioned.of[String, Int](d.value))
    val expectedData6 = List(unbounded[Int] -> "Hey!!!")
    fixture6.getAll.toList shouldBe expectedData6
    fixture6.get shouldBe "Hey!!!"

    val fixture7 = fixture6.filter(_.value == "Planet")
    assert(fixture7.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture7.get

  test("Immutable: Compressing data in intervals"):
    val allData = List(
      intervalTo(4) -> "Hello",
      intervalAt(5) -> "World",
      intervalAt(6) -> "World",
      intervalAt(7) -> "Hello",
      interval(8, 9) -> "Hello",
      intervalFrom(10) -> "Hello"
    )

    val fixture1 = DataIn1DVersioned
      .from(allData)
      .compress("Hello")
    val expectedData1 = List(
      intervalTo(4) -> "Hello",
      intervalAt(5) -> "World",
      intervalAt(6) -> "World",
      intervalFrom(7) -> "Hello"
    )
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn1DVersioned
      .from(allData)
      .compressAll()
    val expectedData2 = List(intervalTo(4) -> "Hello", interval(5, 6) -> "World", intervalFrom(7) -> "Hello")
    fixture2.getSelectedDataMutable.getAll.toList shouldBe expectedData2

  test("Immutable: Updating data in intervals"):
    val one: DataIn1DVersioned[String, Int] = DataIn1DVersioned
      .of("value")
      .incrementCurrentVersion()

    val oneSplit = one.remove(intervalAt(0)) // split
    val expectedDataSplit = List(intervalTo(-1) -> "value", intervalFrom(1) -> "value")
    oneSplit.getAll.toList shouldBe expectedDataSplit
    oneSplit.getAt(0) shouldBe None
    oneSplit.getAt(1) shouldBe Some("value")

    val allData = List(intervalTo(4) -> "Hello", interval(5, 6) -> "World", intervalFrom(7) -> "Hello")
    val fixture1 = newDataIn1DVersioned(allData)
      .update(interval(5, 7) -> "World!")
      .incrementCurrentVersion()
    // needed? .recompressAll()
    val expectedData1 = List(intervalTo(4) -> "Hello", interval(5, 7) -> "World!", intervalFrom(8) -> "Hello")
    fixture1.getAll.toList shouldBe expectedData1

    // println(fixture1.toString)
    fixture1.toString shouldBe
      """current version = 4
        || -∞ .. 4        | 5 .. 6         | 7 .. 7         | 8 .. +∞        |
        |                 | World [1..2]   |
        |                                  | Hello [2..2]                    |
        || Hello [0..+∞)  |
        |                 | World! [3..+∞)                  |
        |                                                   | Hello [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")

    val fixtureToReset = fixture1
      .remove(intervalTo(4))(using VersionSelection(1))
    // needed? .recompressAll()
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 4
        || -∞ .. 4        | 5 .. 6         | 7 .. 7         | 8 .. +∞        |
        || Hello [0..0]   |
        |                 | World [1..2]   |
        |                                  | Hello [2..2]                    |
        |                 | World! [3..+∞)                  |
        |                                                   | Hello [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")
    val fixtureReset = fixtureToReset.resetToVersion(2)
    // println(fixtureReset.toString)
    fixtureReset.toString shouldBe
      """current version = 2
        || -∞ .. 4       | 5 .. 6        | 7 .. +∞       |
        || Hello [0..0]  |
        |                | World [1..+∞) |
        |                                | Hello [2..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    // println(fixture1.getDataIn1D(using VersionSelection(2)).toString)
    fixture1.getSelectedData(using VersionSelection(2)).toString shouldBe
      """|| -∞ .. 4 | 5 .. 6  | 7 .. +∞ |
         || Hello   |
         |          | World   |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    // println(fixture1.getDataIn1D.toString)
    fixture1.getSelectedData.toString shouldBe
      """|| -∞ .. 4 | 5 .. 7  | 8 .. +∞ |
         || Hello   |
         |          | World!  |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    val fixture2 = fixture1
      .update(interval(5, 6) -> "Hello")
      .incrementCurrentVersion()
    val expectedData2 = List(intervalTo(6) -> "Hello", intervalAt(7) -> "World!", intervalFrom(8) -> "Hello")
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2
      .update(intervalFrom(6) -> "World!")
      .incrementCurrentVersion()
    // needed? .recompressAll()
    val expectedData3 = List(intervalTo(5) -> "Hello", intervalFrom(6) -> "World!")
    fixture3.getAll.toList shouldBe expectedData3

    // println(fixture3.toString)
    fixture3.toString shouldBe
      """current version = 6
        || -∞ .. 4        | 5 .. 5         | 6 .. 6         | 7 .. 7         | 8 .. +∞        |
        |                 | World [1..2]                    |
        |                                                   | Hello [2..2]                    |
        |                 | World! [3..3]                                    |
        |                 | Hello [4..4]                    |
        |                                                   | World! [4..4]  |
        |                                                                    | Hello [3..4]   |
        || Hello [0..+∞)  |
        |                 | Hello [5..+∞)  |
        |                                  | World! [5..+∞)                                   |
        |""".stripMargin.replaceAll("\r", "")

    val fixture4 = fixture3.collapseVersionHistory
    // println(fixture4.toString)
    fixture4.toString shouldBe
      """current version = 0
        || -∞ .. 5        | 6 .. +∞        |
        || Hello [0..+∞)  |
        |                 | World! [0..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Immutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData1D[String, LocalDate]]
    ): DataIn1DVersioned[String, LocalDate] =
      allData.foldLeft(DataIn1DVersioned[String, LocalDate]()): (data, validData) =>
        data
          .set(validData)
          .incrementCurrentVersion()

    val fixture0: DataIn1DVersioned[String, LocalDate] = DataIn1DVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      unbounded[LocalDate] -> "Testing",
      interval(day(1), day(15)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
    val fixture1 = timeboundVersionedString(allData)
    // Visualize(fixture1.getDataIn2D, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)))
    val fixture2 = fixture1.set(zoinks)(using VersionSelection.Unapproved)

    // Visualize(fixture2.getDataIn2D, 5000, "after Zoinks")

    fixture2.getAt(day(5)) shouldBe Some("Hello")
    fixture2.getAt(day(15)) shouldBe Some("World")
    fixture2.getAt(day(0)) shouldBe Some("Testing")
    fixture2.getAt(day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize(fixture.underlying2D, 15000, "before zoinks is approved")

    val fixture3 = fixture2
      .incrementCurrentVersion()
      .approve(zoinks) match // approves zoinks
      case Some(f) =>
        f.approve(zoinks) shouldBe None // already approved
        f
      case None => fail("unexpected failure to approve")

    val fixture4 = fixture3.remove(interval(day(-5), day(5)))(using VersionSelection.Unapproved)
    // Visualize(fixture4.underlying2D, 15000, "after zoinks approved, before remove is approved")

    fixture4.getAt(day(0)) shouldBe Some(zoinks.value)
    fixture4.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(1), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )

    val fixture5 = fixture4
      .incrementCurrentVersion()
      .approveAll(unbounded) // approves the unapproved remove
    // Visualize(fixture5.underlying2D, 15000, "after remove is approved")

    fixture5.getAt(day(0)) shouldBe None
    fixture5.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(6), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
