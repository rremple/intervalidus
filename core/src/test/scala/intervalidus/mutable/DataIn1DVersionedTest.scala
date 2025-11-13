package intervalidus.mutable

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
    val dataIn1DVersioned = DataVersioned[String, Dim[Int]]()
    allData.foreach: validData =>
      dataIn1DVersioned.set(validData)
      dataIn1DVersioned.incrementCurrentVersion()
    dataIn1DVersioned

  def usingBuilder(data: Iterable[ValidData[String, Dim[Int]]]): DataVersioned[String, Dim[Int]] =
    val builder = DataVersioned.newBuilder[String, Dim[Int]](0)
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  def usingSetMany(data: Iterable[ValidData[String, Dim[Int]]]): DataVersioned[String, Dim[Int]] =
    val newStructure = DataVersioned[String, Dim[Int]]()
    newStructure ++ data
    newStructure

  // shared
  testsFor(stringLookupTests("Mutable", newDataIn1DVersioned, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Mutable (builder)", usingBuilder, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Mutable (setMany)", usingSetMany, DataVersioned(_), DataVersioned.of(_)))

  test("Mutable: Adding and removing data in intervals"):
    val dayZero = LocalDateTime.of(2025, 8, 1, 8, 0)
    given CurrentDateTime = CurrentDateTime.simulated(dayZero)

    val empty: DataVersioned[String, Dim[Int]] = immutable.DataVersioned[String, Dim[Int]]().toImmutable.toMutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      empty.incrementCurrentVersion()

    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val fixture = newDataIn1DVersioned(allData)(using CurrentDateTime.simulated(dayZero))

    // Appropriately fails in one dimension because Tuple.Tail[Domain.In1D[Int]] is empty.
    """fixture.getByHeadDimension(0)""" shouldNot typeCheck

    fixture.set(interval(5, 15) -> "to")
    fixture.incrementCurrentVersion()(using CurrentDateTime.simulated(dayZero.plusDays(1)))
    val expectedData1 = List(interval(0, 4) -> "Hello", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    fixture.getAll.toList shouldBe expectedData1

    fixture.set(interval(20, 25) -> "!") // split
    fixture.incrementCurrentVersion()(using CurrentDateTime.simulated(dayZero.plusDays(1).plusHours(1)))
    fixture.recompressAll() // not needed, but addresses coverage gap
    val expectedData2 = List(
      interval(0, 4) -> "Hello",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      interval(20, 25) -> "!",
      intervalFrom(26) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData2

    fixture.getVersionTimestamps should contain theSameElementsAs Map(
      0 -> (dayZero, "init"),
      1 -> (dayZero, "incremented"),
      2 -> (dayZero, "incremented"),
      3 -> (dayZero.plusDays(1), "incremented"),
      4 -> (dayZero.plusDays(1).plusHours(1), "incremented")
    )
    // println(fixture.toString)
    fixture.toString shouldBe
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

    val fixture2 = fixture.copy

    assert(!fixture.setIfNoConflict(intervalTo(4) -> "Hey"))
    assert(fixture.setIfNoConflict(intervalTo(-1) -> "Hey"))
    fixture.incrementCurrentVersion()

    fixture.set(intervalTo(4) -> "Hey")
    fixture -- Seq(intervalFrom(21))
    fixture.incrementCurrentVersion()
    val expectedData3 = List(
      intervalTo(4) -> "Hey",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      intervalAt(20) -> "!"
    )
    fixture.getAll.toList shouldBe expectedData3

    fixture.set(intervalFrom(20) -> "World")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData4 = List(intervalTo(4) -> "Hey", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    fixture.getAll.toList shouldBe expectedData4

    val fixture4 = fixture.copy

    fixture.remove(interval(5, 15))
    fixture.incrementCurrentVersion()
    val expectedData5 = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")
    fixture.getAll.toList shouldBe expectedData5

    fixture.set(intervalFrom(1) -> "remove me")
    fixture.remove(intervalFrom(1))
    // if needed: fixture.recompressAll()
    val expectedData6 = List(intervalTo(0) -> "Hey")
    fixture.getAll.toList shouldBe expectedData6

    val fixture6 = fixture.copy

    val fixture7 = fixture.copy
    fixture7.fill(Interval1D.unbounded -> "Filled")
    val expectedFilled = List(intervalTo(0) -> "Hey", intervalFrom(1) -> "Filled")
    fixture7.getAll.toList shouldBe expectedFilled
    fixture7.intervals("Filled").map(_.headInterval1D[Int]) should contain theSameElementsAs List(intervalFrom(1))
    fixture7.removeValue("Filled")
    fixture7.getAll.toList shouldBe expectedData6

    val dataL = DataVersioned.of(intervalFrom(1).to(3) -> "A", 1, "L init v1") // version 1 at day zero
    dataL.incrementCurrentVersion("L to v2")(using CurrentDateTime.simulated(dayZero.plusDays(3))) // version 2 at day 3
    dataL.set(intervalFrom(5) -> "B")
    dataL.incrementCurrentVersion("L to v3")(using CurrentDateTime.simulated(dayZero.plusDays(4))) // version 3 at day 4
    dataL.getVersionTimestamps should contain theSameElementsAs Map(
      1 -> (dayZero, "L init v1"),
      2 -> (dayZero.plusDays(3), "L to v2"),
      3 -> (dayZero.plusDays(4), "L to v3")
    )
    val dataR = DataVersioned.of(intervalFrom(2).to(4) -> "C", 0, "R init v0") // version 0 at day zero
    dataR.incrementCurrentVersion("R to v1")(using CurrentDateTime.simulated(dayZero.plusDays(1))) // version 1 at day 1
    dataR.set(intervalFrom(6) -> "D")
    dataR.incrementCurrentVersion("R to v2")(using CurrentDateTime.simulated(dayZero.plusDays(2))) // version 2 at day 2
    dataR.getVersionTimestamps should contain theSameElementsAs Map(
      0 -> (dayZero, "R init v0"),
      1 -> (dayZero.plusDays(1), "R to v1"),
      2 -> (dayZero.plusDays(2), "R to v2")
    )

    // Default merge operation will "prioritize left"
    val defaultMerge = dataL.copy
    defaultMerge.merge(dataR)
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
    val customMerge = dataL.copy
    customMerge.merge(dataR, _ + _)
    customMerge.getAll.toList shouldBe List(
      intervalFrom(1).toBefore(2) -> "A",
      intervalFrom(2).to(3) -> "AC",
      intervalFromAfter(3).to(4) -> "C",
      intervalFrom(5).toBefore(6) -> "B",
      intervalFrom(6) -> "BD"
    )
    defaultMerge.getVersionTimestamps should contain theSameElementsAs customMerge.getVersionTimestamps

    val selfMerge = dataL.copy
    selfMerge.merge(dataL)
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
    fixture2.applyDiffActions(actionsFrom2To4)
    fixture2.getAll(using VersionSelection(fixture4.getCurrentVersion)).toList shouldBe expectedData4

    fixture2.syncWith(fixture6)
    fixture2.getAll(using VersionSelection(fixture6.getCurrentVersion)).toList shouldBe expectedData6

    val actionsFromVersion7 = fixture6.diffActionsBetween(VersionSelection(7), VersionSelection.Current)
    // actionsFromVersion7.foreach(a => println(a.toCodeLikeString))
    actionsFromVersion7 shouldBe Iterable(
      Update((interval(2, 7) x interval(16, 19)) -> "World"),
      Update((intervalFrom(5) x intervalTo(0)) -> "Hey"),
      Create((interval(5, 7) x interval(1, 4)) -> "Hey"),
      Update((interval(6, 7) x intervalFrom(20)) -> "World")
    )
    val fixture6Sync = fixture6.copy
    fixture6Sync.resetToVersion(7)
    fixture6Sync.applyDiffActions(actionsFromVersion7)
    fixture6Sync.getVersionedData.getAll should contain theSameElementsAs fixture6.getVersionedData.getAll

    val fixture8 = fixture6.copy
    fixture8.set(intervalFrom(30) -> "World?")(using VersionSelection.Unapproved)
    fixture8.remove(intervalFrom(40))(using VersionSelection.Unapproved)

    fixture6.getAll should contain theSameElementsAs fixture8.getAll
    val unapprovedActions = fixture8.diffActionsBetween(VersionSelection.Current, VersionSelection.Unapproved)
    unapprovedActions shouldBe Iterable(
      Create((intervalFrom(unapprovedStartVersion) x interval(30, 39)) -> "World?")
    )

    val fixture9 = fixture8.copy
    fixture9.resetToVersion(fixture8.getCurrentVersion) // removes unapproved updates
    fixture9.applyDiffActions(unapprovedActions)
    fixture8.getVersionedData.getAll should contain theSameElementsAs fixture9.getVersionedData.getAll

  test("Mutable: Mapping, flat mapping, etc."):
    val allData = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")

    val fixture = newDataIn1DVersioned(allData)
    fixture.copy shouldBe fixture

    val concat = fixture.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(horizontal(d.interval).toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture.map: d =>
      d.interval.to(d.interval.end.rightAdjacent) -> (d.value + "!")
    val expectedData2 = List(intervalTo(5) -> "Hey!", intervalFrom(16) -> "World!")
    fixture.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap(d => DataVersioned(Seq(d)))
    val expectedData4 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.filter(v => horizontal(v.interval) ⊆ intervalTo(10))
    val expectedData5 = List(intervalTo(5) -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataVersioned.of[String, Dim[Int]](d.value))
    val expectedData6 = List(unbounded[Int] -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData6
    fixture.get shouldBe "Hey!!!"

    fixture.filter(_.value == "Planet")
    assert(fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

  test("Mutable: Compressing data in intervals"):
    val allData = List(
      intervalTo(4) -> "Hello",
      intervalAt(5) -> "World",
      intervalAt(6) -> "World",
      intervalAt(7) -> "Hello",
      interval(8, 9) -> "Hello",
      intervalFrom(10) -> "Hello"
    )

    val fixture1 = DataVersioned.from(allData)
    fixture1.compress("Hello")
    val expectedData1 = List(
      intervalTo(4) -> "Hello",
      interval(5, 6) -> "World",
      intervalFrom(7) -> "Hello"
    )
    // getSelectedDataMutable compresses
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    val fixture2 = DataVersioned.from(allData)
    fixture2.compressAll()
    def versioned(i: Interval1D[Int]): Interval.In2D[Int, Int] = intervalFrom(0) x i
    val expectedData2 = List(
      versioned(intervalTo(4)) -> "Hello",
      versioned(interval(5, 6)) -> "World",
      versioned(intervalFrom(7)) -> "Hello"
    )
    fixture2.getVersionedData.getAll.toList shouldBe expectedData2

  test("Mutable: Updating data in intervals"):
    val one: DataVersioned[String, Dim[Int]] = DataVersioned.of("value")
    one.incrementCurrentVersion()

    one.remove(intervalAt(0)) // split
    val expectedData0 = List(intervalTo(-1) -> "value", intervalFrom(1) -> "value")
    one.getAll.toList shouldBe expectedData0
    one.getAt(0) shouldBe None
    one.getAt(1) shouldBe Some("value")

    val allData = List(intervalTo(4) -> "Hello", interval(5, 6) -> "World", intervalFrom(7) -> "Hello")
    val fixture = newDataIn1DVersioned(allData)

    fixture.update(interval(5, 7) -> "World!")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData1 = List(intervalTo(4) -> "Hello", interval(5, 7) -> "World!", intervalFrom(8) -> "Hello")
    fixture.getAll.toList shouldBe expectedData1

    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 4
        || 0 .. 0        | 1 .. 1        | 2 .. 2        | 3 .. +∞       |
        || Hello (-∞..4]                                                 |
        |                | World [5..6]                  |
        |                                | Hello [7..+∞) |
        |                                                | World! [5..7] |
        |                                                | Hello [8..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    val fixtureToReset = fixture.copy
    fixtureToReset.remove(intervalTo(4))(using VersionSelection(1))
    // if needed: fixtureToReset.recompressAll()
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
    fixtureToReset.resetToVersion(2)
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 2
        || 0 .. 0        | 1 .. 1        | 2 .. +∞       |
        || Hello (-∞..4] |
        |                | World [5..6]                  |
        |                                | Hello [7..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getSelectedData(using VersionSelection(2)).toString)
    fixture.getSelectedData(using VersionSelection(2)).toString shouldBe
      """|| -∞ .. 4 | 5 .. 6  | 7 .. +∞ |
         || Hello   |
         |          | World   |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getSelectedData.toString)
    fixture.getSelectedData.toString shouldBe
      """|| -∞ .. 4 | 5 .. 7  | 8 .. +∞ |
         || Hello   |
         |          | World!  |
         |                    | Hello   |
         |""".stripMargin.replaceAll("\r", "")

    fixture.update(interval(5, 6) -> "Hello")
    fixture.incrementCurrentVersion()
    val expectedData2 = List(intervalTo(6) -> "Hello", intervalAt(7) -> "World!", intervalFrom(8) -> "Hello")
    fixture.getAll.toList shouldBe expectedData2

    fixture.update(intervalFrom(6) -> "World!")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData3 = List(intervalTo(5) -> "Hello", intervalFrom(6) -> "World!")
    fixture.getAll.toList shouldBe expectedData3

    // println(fixture.toString)
    fixture.toString shouldBe
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

    fixture.collapseVersionHistory
    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 0
        || 0 .. +∞        |
        || Hello (-∞..5]  |
        || World! [6..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Mutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData[String, Dim[LocalDate]]]
    ): DataVersioned[String, Dim[LocalDate]] =
      val data = DataVersioned[String, Dim[LocalDate]]()
      allData.foreach: validData =>
        data.set(validData)(using VersionSelection.Current)
        data.incrementCurrentVersion()
      data

    val fixture0: DataVersioned[String, Dim[LocalDate]] = DataVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      unbounded[LocalDate] -> "Testing",
      interval(day(1), day(15)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
    val fixture = timeboundVersionedString(allData)
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)))
    fixture.set(zoinks)(using VersionSelection.Unapproved)
    // Visualize2D(fixture.getDataIn2D, 5000, "gets")

    fixture.getAt(day(5)) shouldBe Some("Hello")
    fixture.getAt(day(15)) shouldBe Some("World")
    fixture.getAt(day(0)) shouldBe Some("Testing")

    // Visualize2D(fixture.getDataIn2D, 15000, "before zoinks is approved")

    fixture.incrementCurrentVersion()
    // fixture.approveAll(unbounded) // approves zoinks
    assert(fixture.approve(zoinks)) // approves zoinks
    assert(!fixture.approve(zoinks)) // already approved

    fixture.remove(interval(day(-5), day(5)))(using VersionSelection.Unapproved)
    // Visualize2D(fixture.getDataIn2D, 15000, "after zoinks approved, before remove is approved")

    fixture.getAt(day(0)) shouldBe Some(zoinks.value)
    fixture.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(1), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )

    fixture.incrementCurrentVersion()
    fixture.approveAll(unbounded[LocalDate]) // approves the unapproved remove
    // Visualize2D(fixture.getDataIn2D, 15000, "after remove is approved")

    fixture.getAt(day(0)) shouldBe None
    fixture.getIntersecting(interval(day(5), day(15))) should contain theSameElementsAs List(
      interval(day(6), day(9)) -> "Hello",
      intervalFrom(day(10)) -> "World"
    )
