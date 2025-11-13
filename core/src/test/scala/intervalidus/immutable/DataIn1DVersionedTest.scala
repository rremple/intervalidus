package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}
import scala.language.implicitConversions

class DataIn1DVersionedTest extends AnyFunSuite with Matchers with DataIn1DVersionedBaseBehaviors:

  import DimensionalVersionedBase.*
  import Interval1D.*

  // increment current version with each data element
  def newDataIn1DVersioned(
    allData: Iterable[ValidData[String, Dim[Int]]]
  )(using CurrentDateTime): DataVersioned[String, Dim[Int]] =
    allData.foldLeft(DataVersioned[String, Dim[Int]]()): (dataIn1DVersioned, validData) =>
      dataIn1DVersioned
        .set(validData)
        .incrementCurrentVersion()

  def usingBuilder(data: Iterable[ValidData[String, Dim[Int]]]): DataVersioned[String, Dim[Int]] =
    val builder = DataVersioned.newBuilder[String, Dim[Int]](0)
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  def usingSetMany(data: Iterable[ValidData[String, Dim[Int]]]): DataVersioned[String, Dim[Int]] =
    DataVersioned[String, Dim[Int]]() ++ data

  // shared
  testsFor(stringLookupTests("Immutable", newDataIn1DVersioned, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Immutable (builder)", usingBuilder, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Immutable (setMany)", usingSetMany, DataVersioned(_), DataVersioned.of(_)))

  test("Immutable: Adding and removing data in intervals"):
    val dayZero = LocalDateTime.of(2025, 8, 1, 8, 0)
    given CurrentDateTime = CurrentDateTime.simulated(dayZero)

    val empty: DataVersioned[String, Dim[Int]] = mutable.DataVersioned[String, Dim[Int]]().toMutable.toImmutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    val emptyAtMaxVersion = empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      emptyAtMaxVersion.incrementCurrentVersion()

    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val fixture0 = newDataIn1DVersioned(allData)(using CurrentDateTime.simulated(dayZero))

    // Appropriately fails in one dimension because Tuple.Tail[Domain.In1D[Int]] is empty.
    """fixture0.getByHeadDimension(0)""" shouldNot typeCheck

    val fixture1 = fixture0
      .set(interval(5, 15) -> "to")
      .incrementCurrentVersion()(using CurrentDateTime.simulated(dayZero.plusDays(1)))
    val expectedData1 = List(interval(0, 4) -> "Hello", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .set(interval(20, 25) -> "!") // split
      .incrementCurrentVersion()(using CurrentDateTime.simulated(dayZero.plusDays(1).plusHours(1)))
      .recompressAll() // not needed, but addresses coverage gap
    val expectedData2 = List(
      interval(0, 4) -> "Hello",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      interval(20, 25) -> "!",
      intervalFrom(26) -> "World"
    )
    fixture2.getAll.toList shouldBe expectedData2

    fixture2.getVersionTimestamps should contain theSameElementsAs Map(
      0 -> (dayZero, "init"),
      1 -> (dayZero, "incremented"),
      2 -> (dayZero, "incremented"),
      3 -> (dayZero.plusDays(1), "incremented"),
      4 -> (dayZero.plusDays(1).plusHours(1), "incremented")
    )
    // println(fixture2.toString)
    fixture2.toString shouldBe
      """current version = 4
        || 0 .. 0         | 1 .. 1         | 2 .. 2         | 3 .. +∞        |
        || Hello [0..4]                                                      |
        || Hello [5..9]                    |
        |                 | World [10..+∞) |
        |                                  | to [5..15]                      |
        |                                  | World [16..19]                  |
        |                                  | World [20..+∞) |
        |                                                   | ! [20..25]     |
        |                                                   | World [26..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    fixture2.setIfNoConflict(intervalTo(4) -> "Hey") shouldBe None
    val fixture3 = fixture2.setIfNoConflict(intervalTo(-1) -> "Hey") match
      case Some(f) =>
        f.incrementCurrentVersion()
          .set(intervalTo(4) -> "Hey")
          .removeMany(Seq(intervalFrom(21)))
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
    // if needed: .recompressAll()
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
    // if needed: .recompressAll()
    val expectedData6 = List(intervalTo(0) -> "Hey")
    fixture6.getAll.toList shouldBe expectedData6

    val fixture7 = fixture6.fill(Interval1D.unbounded -> "Filled")
    val expectedFilled = List(intervalTo(0) -> "Hey", intervalFrom(1) -> "Filled")
    fixture7.getAll.toList shouldBe expectedFilled
    fixture7.intervals("Filled").map(_.headInterval1D[Int]) should contain theSameElementsAs List(intervalFrom(1))
    fixture7.removeValue("Filled").getAll.toList shouldBe expectedData6

    val dataL = DataVersioned
      .of(intervalFrom(1).to(3) -> "A", 1, "L init v1") // version 1 at day zero
      .incrementCurrentVersion("L to v2")(using CurrentDateTime.simulated(dayZero.plusDays(3))) // version 2 at day 3
      .set(intervalFrom(5) -> "B")
      .incrementCurrentVersion("L to v3")(using CurrentDateTime.simulated(dayZero.plusDays(4))) // version 3 at day 4
    dataL.getVersionTimestamps should contain theSameElementsAs Map(
      1 -> (dayZero, "L init v1"),
      2 -> (dayZero.plusDays(3), "L to v2"),
      3 -> (dayZero.plusDays(4), "L to v3")
    )
    val dataR = DataVersioned
      .of(intervalFrom(2).to(4) -> "C", 0, "R init v0") // version 0 at day zero
      .incrementCurrentVersion("R to v1")(using CurrentDateTime.simulated(dayZero.plusDays(1))) // version 1 at day 1
      .set(intervalFrom(6) -> "D")
      .incrementCurrentVersion("R to v2")(using CurrentDateTime.simulated(dayZero.plusDays(2))) // version 2 at day 2
    dataR.getVersionTimestamps should contain theSameElementsAs Map(
      0 -> (dayZero, "R init v0"),
      1 -> (dayZero.plusDays(1), "R to v1"),
      2 -> (dayZero.plusDays(2), "R to v2")
    )

    // Default merge operation will "prioritize left"
    val defaultMerge = dataL.merge(dataR)
    defaultMerge.getAll.toList shouldBe List(
      intervalFrom(1).to(3) -> "A",
      intervalFromAfter(3).to(4) -> "C",
      intervalFrom(5) -> "B"
    )
    defaultMerge.getVersionTimestamps should contain theSameElementsAs Map(
      //                                                    *  L (left)   R (right)
      0 -> (dayZero, "R init v0"), //                       *  0 day 0              => (None, Some(R))
      1 -> (dayZero.plusDays(1), "L init v1 & R to v1"), // *  1 day 0    1 day 1   => (Some(L), Some(R)), L < R
      2 -> (dayZero.plusDays(3), "L to v2 & R to v2"), //   *  2 day 3    2 day 2   => (Some(L), Some(R)), L > R
      3 -> (dayZero.plusDays(4), "L to v3") //              *  3 day 4              => (Some(L), None)
    )

    // Custom merge operation will combine overlapping elements
    val customMerge = dataL.merge(dataR, _ + _)
    customMerge.getAll.toList shouldBe List(
      intervalFrom(1).toBefore(2) -> "A",
      intervalFrom(2).to(3) -> "AC",
      intervalFromAfter(3).to(4) -> "C",
      intervalFrom(5).toBefore(6) -> "B",
      intervalFrom(6) -> "BD"
    )
    defaultMerge.getVersionTimestamps should contain theSameElementsAs customMerge.getVersionTimestamps

    val selfMerge = dataL.merge(dataL)
    selfMerge.getAll should contain theSameElementsAs dataL.getAll
    selfMerge.getVersionTimestamps should contain theSameElementsAs dataL.getVersionTimestamps

    import DiffAction.*

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2)
    actionsFrom2To4 shouldBe Iterable(
      Update((interval(0, 4) x interval(0, 4)) -> "Hello"),
      Update((interval(3, 5) x intervalAt(20)) -> "!"),
      Create((interval(3, 4) x interval(21, 25)) -> "!"),
      Update((interval(3, 4) x intervalFrom(26)) -> "World"),
      Create((intervalAt(4) x intervalTo(-1)) -> "Hey"),
      Create((intervalFrom(5) x intervalTo(4)) -> "Hey"),
      Create((intervalFrom(6) x intervalFrom(20)) -> "World")
    )
    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6 shouldBe Iterable(
      Update((interval(2, 6) x interval(5, 15)) -> "to"),
      Update((interval(2, 7) x interval(16, 19)) -> "World"),
      Update((intervalFrom(5) x intervalTo(0)) -> "Hey"),
      Create((interval(5, 7) x interval(1, 4)) -> "Hey"),
      Update((interval(6, 7) x intervalFrom(20)) -> "World")
    )
    val fixture2to4 = fixture2.applyDiffActions(actionsFrom2To4)
    fixture2to4.getAll(using VersionSelection(fixture4.getCurrentVersion)).toList shouldBe expectedData4

    val fixture2to6 = fixture2.syncWith(fixture6)
    fixture2to6.getAll(using VersionSelection(fixture6.getCurrentVersion)).toList shouldBe expectedData6

    val actionsFromVersion7 = fixture6.diffActionsBetween(VersionSelection(7), VersionSelection.Current)
    // actionsFromVersion7.foreach(a => println(a.toCodeLikeString))
    actionsFromVersion7 shouldBe Iterable(
      Update((interval(2, 7) x interval(16, 19)) -> "World"),
      Update((intervalFrom(5) x intervalTo(0)) -> "Hey"),
      Create((interval(5, 7) x interval(1, 4)) -> "Hey"),
      Update((interval(6, 7) x intervalFrom(20)) -> "World")
    )
    fixture6
      .resetToVersion(7)
      .applyDiffActions(actionsFromVersion7)
      .getVersionedData
      .getAll should contain theSameElementsAs fixture6.getVersionedData.getAll

    val fixture8 = fixture6
      .set(intervalFrom(30) -> "World?")(using VersionSelection.Unapproved)
      .remove(intervalFrom(40))(using VersionSelection.Unapproved)

    fixture6.getAll should contain theSameElementsAs fixture8.getAll
    val unapprovedActions = fixture8.diffActionsBetween(VersionSelection.Current, VersionSelection.Unapproved)
    unapprovedActions shouldBe Iterable(
      Create((intervalFrom(unapprovedStartVersion) x interval(30, 39)) -> "World?")
    )

    fixture8
      .resetToVersion(fixture8.getCurrentVersion) // removes unapproved updates
      .applyDiffActions(unapprovedActions)
      .getVersionedData
      .getAll should contain theSameElementsAs fixture8.getVersionedData.getAll

  test("Immutable: Mapping, flat mapping, etc."):
    val allData = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")

    val fixture1 = newDataIn1DVersioned(allData)
    fixture1.copy shouldBe fixture1

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(horizontal(d.interval).toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 = fixture1.map: d =>
      d.interval.to(d.interval.end.rightAdjacent) -> (d.value + "!")
    val expectedData2 = List(intervalTo(5) -> "Hey!", intervalFrom(16) -> "World!")
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .flatMap(d => DataVersioned.of[String, Dim[Int]](d.value).map(x => d.interval -> x.value))
    val expectedData4 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(v => horizontal(v.interval) ⊆ intervalTo(10))
    val expectedData5 = List(intervalTo(5) -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataVersioned.of[String, Dim[Int]](d.value))
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

    val fixture1 = DataVersioned
      .from(allData)
      .compress("Hello")
    val expectedData1 = List(
      intervalTo(4) -> "Hello",
      interval(5, 6) -> "World",
      intervalFrom(7) -> "Hello"
    )
    // getSelectedDataMutable compresses
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    def versioned(i: Interval1D[Int]): Interval.In2D[Int, Int] = intervalFrom(0) x i
    val fixture2 = DataVersioned
      .from(allData)
      .compressAll()
    val expectedData2 = List(
      versioned(intervalTo(4)) -> "Hello",
      versioned(interval(5, 6)) -> "World",
      versioned(intervalFrom(7)) -> "Hello"
    )
    fixture2.getVersionedData.getAll.toList shouldBe expectedData2

  test("Immutable: Updating data in intervals"):
    val one: DataVersioned[String, Dim[Int]] = DataVersioned
      .of[String, Dim[Int]]("value")
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
    // if needed: .recompressAll()
    val expectedData1 = List(intervalTo(4) -> "Hello", interval(5, 7) -> "World!", intervalFrom(8) -> "Hello")
    fixture1.getAll.toList shouldBe expectedData1

    // println(fixture1.toString)
    fixture1.toString shouldBe
      """current version = 4
        || 0 .. 0        | 1 .. 1        | 2 .. 2        | 3 .. +∞       |
        || Hello (-∞..4]                                                 |
        |                | World [5..6]                  |
        |                                | Hello [7..+∞) |
        |                                                | World! [5..7] |
        |                                                | Hello [8..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    val fixtureToReset = fixture1
      .remove(intervalTo(4))(using VersionSelection(1))
    // if needed: .recompressAll()
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 4
        || 0 .. 0        | 1 .. 1        | 2 .. 2        | 3 .. +∞       |
        || Hello (-∞..4] |
        |                | World [5..6]                  |
        |                                | Hello [7..+∞) |
        |                                                | World! [5..7] |
        |                                                | Hello [8..+∞) |
        |""".stripMargin.replaceAll("\r", "")
    val fixtureReset = fixtureToReset.resetToVersion(2)
    // println(fixtureReset.toString)
    fixtureReset.toString shouldBe
      """current version = 2
        || 0 .. 0        | 1 .. 1        | 2 .. +∞       |
        || Hello (-∞..4] |
        |                | World [5..6]                  |
        |                                | Hello [7..+∞) |
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
    // if needed: .recompressAll()
    val expectedData3 = List(intervalTo(5) -> "Hello", intervalFrom(6) -> "World!")
    fixture3.getAll.toList shouldBe expectedData3

    // println(fixture3.toString)
    fixture3.toString shouldBe
      """current version = 6
        || 0 .. 0         | 1 .. 1         | 2 .. 2         | 3 .. 3         | 4 .. 4         | 5 .. +∞        |
        || Hello (-∞..4]                                                                                       |
        |                 | World [5..6]                    |
        |                                  | Hello [7..+∞)  |
        |                                                   | World! [5..7]  |
        |                                                   | Hello [8..+∞)                   |
        |                                                                    | Hello [5..5]                    |
        |                                                                    | Hello [6..6]   |
        |                                                                    | World! [7..7]                   |
        |                                                                                     | World! [6..6]  |
        |                                                                                     | World! [8..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    val fixture4 = fixture3.collapseVersionHistory
    // println(fixture4.toString)
    fixture4.toString shouldBe
      """current version = 0
        || 0 .. +∞        |
        || Hello (-∞..5]  |
        || World! [6..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Immutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData[String, Dim[LocalDate]]]
    ): DataVersioned[String, Dim[LocalDate]] =
      allData.foldLeft(DataVersioned[String, Dim[LocalDate]]()): (data, validData) =>
        data
          .set(validData)
          .incrementCurrentVersion()

    val fixture0: DataVersioned[String, Dim[LocalDate]] = DataVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      unbounded[LocalDate] -> "Testing",
      interval(day(1), day(15)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
    val fixture1 = timeboundVersionedString(allData)
    // Visualize2D(fixture1.getDataIn2D, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)))
    val fixture2 = fixture1.set(zoinks)(using VersionSelection.Unapproved)

    // Visualize2D(fixture2.getDataIn2D, 5000, "after Zoinks")

    fixture2.getAt(day(5)) shouldBe Some("Hello")
    fixture2.getAt(day(15)) shouldBe Some("World")
    fixture2.getAt(day(0)) shouldBe Some("Testing")
    fixture2.getAt(day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize2D(fixture2.getDataIn2D, 15000, "before zoinks is approved")

    val fixture3 = fixture2
      .incrementCurrentVersion()
      .approve(zoinks) match // approves zoinks
      case Some(f) =>
        f.approve(zoinks) shouldBe None // already approved
        f
      case None => fail("unexpected failure to approve")

    val fixture4 = fixture3.remove(interval(day(-5), day(5)))(using VersionSelection.Unapproved)
    // Visualize2D(fixture4.getDataIn2D, 15000, "after zoinks approved, before remove is approved")

    fixture4.getAt(day(0)) shouldBe Some(zoinks.value)
    fixture4.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(1), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )

    val fixture5 = fixture4
      .incrementCurrentVersion()
      .approveAll(unbounded[LocalDate]) // approves the unapproved remove
    // Visualize2D(fixture5.getDataIn2D, 15000, "after remove is approved")

    fixture5.getAt(day(0)) shouldBe None
    fixture5.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(6), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
