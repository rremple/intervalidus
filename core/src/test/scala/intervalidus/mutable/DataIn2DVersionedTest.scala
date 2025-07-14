package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In2D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DVersionedTest extends AnyFunSuite with Matchers with DataIn2DVersionedBaseBehaviors:

  import DimensionalVersionedBase.VersionSelection
  import Interval1D.*

  // increment current version with each data element
  def newDataIn2DVersioned(allData: Iterable[ValidData[String, Dim[Int, Int]]]): DataVersioned[String, Dim[Int, Int]] =
    val dataIn2DVersioned = DataVersioned[String, Dim[Int, Int]]()
    allData.foreach: validData =>
      dataIn2DVersioned.set(validData)
      dataIn2DVersioned.incrementCurrentVersion()
    dataIn2DVersioned

  // shared
  testsFor(stringLookupTests("Mutable", newDataIn2DVersioned, DataVersioned(_), DataVersioned.of(_)))

  test("Mutable: Adding and removing data in intervals"):
    val empty: DataVersioned[String, Dim[Int, Int]] =
      immutable.DataVersioned[String, Dim[Int, Int]]().toImmutable.toMutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Domain1D.Top)

    assertThrows[Exception]: // version too small
      empty.setCurrentVersion(Domain1D.Bottom)

    empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      empty.incrementCurrentVersion()

    val allData = List(
      (interval(0, 9) x intervalFrom(0)) -> "Hello",
      (intervalFrom(10) x intervalTo(0)) -> "World"
    )
    val fixture = newDataIn2DVersioned(allData)
    fixture.getByHeadIndex(0).getAt(0) shouldBe Some("Hello")

    fixture.set((interval(5, 15) x unbounded[Int]) -> "to")
    fixture.incrementCurrentVersion()
    val expectedData1 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData1

    fixture.set((interval(20, 25) x unbounded[Int]) -> "!") // split
    fixture.incrementCurrentVersion()
    fixture.recompressAll() // not needed, but addresses coverage gap
    val expectedData2 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (interval(16, 19) x intervalTo(0)) -> "World",
      (interval(20, 25) x unbounded[Int]) -> "!",
      (intervalFrom(26) x intervalTo(0)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData2

    // println(fixture.toString)
    // format: off
    fixture.toString shouldBe
      """current version = 4
        || 0 .. 0                   | 1 .. 1                   | 2 .. 2                   | 3 .. +∞                  |
        || Hello [0..4] x [0..+∞)                                                                                    |
        || Hello [5..9] x [0..+∞)                              |
        |                           | World [10..+∞) x (-∞..0] |
        |                                                      | to [5..15] x (-∞..+∞)                               |
        |                                                      | World [16..19] x (-∞..0]                            |
        |                                                      | World [20..+∞) x (-∞..0] |
        |                                                                                 | ! [20..25] x (-∞..+∞)    |
        |                                                                                 | World [26..+∞) x (-∞..0] |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    val fixture2 = fixture.copy

    fixture.setIfNoConflict((intervalTo(4) x unbounded[Int]) -> "Hey") shouldBe false
    fixture.setIfNoConflict((intervalTo(-1) x unbounded[Int]) -> "Hey") shouldBe true
    fixture.incrementCurrentVersion()
    fixture.set((intervalTo(4) x unbounded[Int]) -> "Hey")
    fixture.remove(intervalFrom(21) x unbounded[Int])
    fixture.incrementCurrentVersion()

    val expectedData3 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (interval(16, 19) x intervalTo(0)) -> "World",
      (intervalAt(20) x unbounded[Int]) -> "!"
    )
    fixture.getAll.toList shouldBe expectedData3

    fixture.set((intervalFrom(20) x intervalTo(0)) -> "World")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData4 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World",
      (intervalAt(20) x intervalFrom(1)) -> "!"
    )
    fixture.getAll.toList shouldBe expectedData4

    val fixture4 = fixture.copy

    fixture.remove(interval(5, 15) x unbounded[Int])
    fixture.remove(intervalAt(20) x intervalFrom(1))
    fixture.incrementCurrentVersion()
    val expectedData5 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData5

    fixture.set((intervalFrom(1) x unbounded[Int]) -> "remove me")
    fixture.remove(intervalFrom(1) x unbounded[Int])
    // if needed: fixture.recompressAll()
    val expectedData6 = List((intervalTo(0) x unbounded[Int]) -> "Hey")
    fixture.getAll.toList shouldBe expectedData6

    val fixture6 = fixture.copy

    val fixture7 = fixture.copy
    fixture7.fill(Interval.unbounded[Dim[Int, Int]] -> "Filled")
    val expectedFilled = List((intervalTo(0) x unbounded[Int]) -> "Hey", (intervalFrom(1) x unbounded[Int]) -> "Filled")
    fixture7.getAll.toList shouldBe expectedFilled

    val data1 = DataVersioned.from(
      Seq((intervalFrom(1).to(3) x unbounded[Int]) -> "A", (intervalFrom(5) x unbounded[Int]) -> "B")
    )
    val data2 = DataVersioned.from(
      Seq((intervalFrom(2).to(4) x unbounded[Int]) -> "C", (intervalFrom(6) x unbounded[Int]) -> "D")
    )
    // Default merge operation will "prioritize left"
    val defaultMerge = data1.copy
    defaultMerge.merge(data2)
    defaultMerge.getAll.toList shouldBe List(
      (intervalFrom(1).to(3) x unbounded[Int]) -> "A",
      (intervalFromAfter(3).to(4) x unbounded[Int]) -> "C",
      (intervalFrom(5) x unbounded[Int]) -> "B"
    )
    // Custom merge operation will combine overlapping elements
    val customMerge = data1.copy
    customMerge.merge(data2, _ + _)
    customMerge.getAll.toList shouldBe List(
      (intervalFrom(1).toBefore(2) x unbounded[Int]) -> "A",
      (intervalFrom(2).to(3) x unbounded[Int]) -> "AC",
      (intervalFromAfter(3).to(4) x unbounded[Int]) -> "C",
      (intervalFrom(5).toBefore(6) x unbounded[Int]) -> "B",
      (intervalFrom(6) x unbounded[Int]) -> "BD"
    )

    import DiffAction.*

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2)
    actionsFrom2To4.toList shouldBe List(
      Update((interval(0, 4) x interval(0, 4) x intervalFrom(0)) -> "Hello"),
      Update((interval(3, 5) x intervalAt(20) x unbounded[Int]) -> "!"),
      Create((interval(3, 4) x interval(21, 25) x unbounded[Int]) -> "!"),
      Update((interval(3, 4) x intervalFrom(26) x intervalTo(0)) -> "World"),
      Create((intervalAt(4) x intervalTo(-1) x unbounded[Int]) -> "Hey"),
      Create((intervalFrom(5) x intervalTo(4) x unbounded[Int]) -> "Hey"),
      Create((intervalFrom(6) x intervalFrom(20) x intervalTo(0)) -> "World"),
      Create((intervalFrom(6) x intervalAt(20) x intervalFrom(1)) -> "!")
    )

    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6.toList shouldBe List(
      Update((interval(2, 6) x interval(5, 15) x unbounded[Int]) -> "to"),
      Update((interval(2, 7) x interval(16, 19) x intervalTo(0)) -> "World"),
      Update((intervalFrom(5) x intervalTo(0) x unbounded[Int]) -> "Hey"),
      Create((interval(5, 7) x interval(1, 4) x unbounded[Int]) -> "Hey"),
      Update((interval(6, 7) x intervalFrom(20) x intervalTo(0)) -> "World"),
      Update((intervalAt(6) x intervalAt(20) x intervalFrom(1)) -> "!")
    )

    fixture2.applyDiffActions(actionsFrom2To4)
    fixture2.getAll(using VersionSelection(fixture4.getCurrentVersion)).toList shouldBe expectedData4

    fixture2.syncWith(fixture6)
    fixture2.getAll(using VersionSelection(fixture6.getCurrentVersion)).toList shouldBe expectedData6

  test("Mutable: Mapping, flat mapping, etc."):
    val allData = List(
      (intervalTo(4) x intervalFrom(0)) -> "Hey",
      (intervalFrom(16) x intervalFrom(0)) -> "World"
    )

    val fixture = newDataIn2DVersioned(allData)
    fixture.copy.getAll.toList shouldBe fixture.getAll.toList

    val concat = fixture.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(horizontal(d.interval).toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixtureAlt = fixture.copy
    fixtureAlt.mapIntervals(i => i.to(i.end.rightAdjacent))
    fixtureAlt.mapValues(_ + "!")

    fixture.map: d =>
      d.interval.to(d.interval.end.rightAdjacent) -> (d.value + "!")
    val expectedData2 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!"
    )

    fixture.getAll.toList shouldBe expectedData2
    fixtureAlt.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!!!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!!!"
    )
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap(d => DataVersioned(Seq(d)))
    val expectedData4 = expectedData3
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    val fixtureCollect = fixture.copy
    fixtureCollect.collect:
      case v if horizontal(v.interval) ⊆ intervalTo(10) => v
    fixture.filter(v => horizontal(v.interval) ⊆ intervalTo(10))
    val expectedData5 = List((intervalTo(5) x intervalFrom(0)) -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData5
    fixtureCollect.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataVersioned.of[String, Dim[Int, Int]](d.value))
    val expectedData6 = List(Interval.unbounded[Dim[Int, Int]] -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData6
    fixture.get shouldBe "Hey!!!"

    fixture.filter(_.value == "Planet")
    assert(fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

  test("Mutable: Compressing data in intervals"):
    val allData = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (intervalAt(5) x unbounded[Int]) -> "World",
      (intervalAt(6) x unbounded[Int]) -> "World",
      (intervalAt(7) x unbounded[Int]) -> "Hello",
      (interval(8, 9) x unbounded[Int]) -> "Hello",
      (intervalFrom(10) x unbounded[Int]) -> "Hello"
    )

    val fixture1 = DataVersioned.from(allData)
    fixture1.compress("Hello")
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (intervalAt(5) x unbounded[Int]) -> "World",
      (intervalAt(6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    val fixture2 = DataVersioned.from(allData)
    fixture2.compressAll()
    val expectedData2 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture2.getSelectedDataMutable.getAll.toList shouldBe expectedData2

  test("Mutable: Updating data in intervals"):
    val one: DataVersioned[String, Dim[Int, Int]] = DataVersioned.of("value")
    one.incrementCurrentVersion()

    one.remove(intervalAt(0) x unbounded[Int]) // split
    val expectedData0 = List(
      (intervalTo(-1) x unbounded[Int]) -> "value",
      (intervalFrom(1) x unbounded[Int]) -> "value"
    )
    one.getAll.toList shouldBe expectedData0
    one.getAt(0, 0) shouldBe None
    one.getAt(1, 1) shouldBe Some("value")

    val allData = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    val fixture = newDataIn2DVersioned(allData)

    fixture.update((interval(5, 7) x unbounded[Int]) -> "World!")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 7) x unbounded[Int]) -> "World!",
      (intervalFrom(8) x unbounded[Int]) -> "Hello"
    )
    fixture.getAll.toList shouldBe expectedData1

    // println(fixture.toString)
    // format: off
    fixture.toString shouldBe
      """current version = 4
        || 0 .. 0                   | 1 .. 1                   | 2 .. 2                   | 3 .. +∞                  |
        || Hello (-∞..4] x (-∞..+∞)                                                                                  |
        |                           | World [5..6] x (-∞..+∞)                             |
        |                                                      | Hello [7..+∞) x (-∞..+∞) |
        |                                                                                 | World! [5..7] x (-∞..+∞) |
        |                                                                                 | Hello [8..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    val fixtureToReset = fixture.copy
    fixtureToReset.remove(intervalTo(4) x unbounded[Int])(using VersionSelection(1))
    // if needed: fixtureToReset.recompressAll()
    // println(fixtureToReset.toString)
    // format: off
    fixtureToReset.toString shouldBe
      """current version = 4
        || 0 .. 0                   | 1 .. 1                   | 2 .. 2                   | 3 .. +∞                  |
        || Hello (-∞..4] x (-∞..+∞) |
        |                           | World [5..6] x (-∞..+∞)                             |
        |                                                      | Hello [7..+∞) x (-∞..+∞) |
        |                                                                                 | World! [5..7] x (-∞..+∞) |
        |                                                                                 | Hello [8..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")
    // format: on
    fixtureToReset.resetToVersion(2)
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 2
        || 0 .. 0                   | 1 .. 1                   | 2 .. +∞                  |
        || Hello (-∞..4] x (-∞..+∞) |
        |                           | World [5..6] x (-∞..+∞)                             |
        |                                                      | Hello [7..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getSelectedData(using VersionSelection(2)).toString)
    fixture.getSelectedData(using VersionSelection(2)).toString shouldBe
      """|| -∞ .. 4        | 5 .. 6         | 7 .. +∞        |
         || Hello (-∞..+∞) |
         |                 | World (-∞..+∞) |
         |                                  | Hello (-∞..+∞) |
         |""".stripMargin.replaceAll("\r", "")

    // println(fixture.getSelectedData.toString)
    fixture.getSelectedData.toString shouldBe
      """|| -∞ .. 4         | 5 .. 7          | 8 .. +∞         |
         || Hello (-∞..+∞)  |
         |                  | World! (-∞..+∞) |
         |                                    | Hello (-∞..+∞)  |
         |""".stripMargin.replaceAll("\r", "")

    fixture.update((interval(5, 6) x unbounded[Int]) -> "Hello")
    fixture.incrementCurrentVersion()
    val expectedData2 = List(
      (intervalTo(6) x unbounded[Int]) -> "Hello",
      (intervalAt(7) x unbounded[Int]) -> "World!",
      (intervalFrom(8) x unbounded[Int]) -> "Hello"
    )
    fixture.getAll.toList shouldBe expectedData2

    fixture.update((intervalFrom(6) x unbounded[Int]) -> "World!")
    fixture.incrementCurrentVersion()
    // if needed: fixture.recompressAll()
    val expectedData3 = List(
      (intervalTo(5) x unbounded[Int]) -> "Hello",
      (intervalFrom(6) x unbounded[Int]) -> "World!"
    )
    fixture.getAll.toList shouldBe expectedData3

    // println(fixture.toString)
    // format: off
    fixture.toString shouldBe
      """current version = 6
        || 0 .. 0                    | 1 .. 1                    | 2 .. 2                    | 3 .. 3                    | 4 .. 4                    | 5 .. +∞                   |
        || Hello (-∞..4] x (-∞..+∞)                                                                                                                                              |
        |                            | World [5..6] x (-∞..+∞)                               |
        |                                                        | Hello [7..+∞) x (-∞..+∞)  |
        |                                                                                    | World! [5..7] x (-∞..+∞)  |
        |                                                                                    | Hello [8..+∞) x (-∞..+∞)                              |
        |                                                                                                                | Hello [5..5] x (-∞..+∞)                               |
        |                                                                                                                | Hello [6..6] x (-∞..+∞)   |
        |                                                                                                                | World! [7..7] x (-∞..+∞)                              |
        |                                                                                                                                            | World! [6..6] x (-∞..+∞)  |
        |                                                                                                                                            | World! [8..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    fixture.collapseVersionHistory
    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 0
        || 0 .. +∞                   |
        || Hello (-∞..5] x (-∞..+∞)  |
        || World! [6..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Mutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData[String, Dim[LocalDate, LocalDate]]]
    ): DataVersioned[String, Dim[LocalDate, LocalDate]] =
      val data = DataVersioned[String, Dim[LocalDate, LocalDate]]()
      allData.foreach: validData =>
        data.set(validData)(using VersionSelection.Current)
        data.incrementCurrentVersion()
      data

    val fixture0: DataVersioned[String, Dim[LocalDate, LocalDate]] = DataVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      (unbounded[LocalDate] x unbounded[LocalDate]) -> "Testing",
      (interval(day(1), day(15)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
    val fixture = timeboundVersionedString(allData)
    // Visualize3D(fixture.getVersionedData, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)) x unbounded[LocalDate])
    fixture.set(zoinks)(using VersionSelection.Unapproved)
    // Visualize3D(fixture.getVersionedData, 5000, "after Zoinks")

    fixture.getAt(day(5), day(0)) shouldBe Some("Hello")
    fixture.getAt(day(15), day(0)) shouldBe Some("World")
    fixture.getAt(day(0), day(0)) shouldBe Some("Testing")
    fixture.getAt(day(0), day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize3D(fixture.getVersionedData, 15000, "before zoinks is approved")

    fixture.incrementCurrentVersion()
    // fixture.approveAll(unbounded) // approves zoinks
    assert(fixture.approve(zoinks)) // approves zoinks
    assert(!fixture.approve(zoinks)) // already approved

    fixture.remove(interval(day(-5), day(5)) x unbounded[LocalDate])(using VersionSelection.Unapproved)
    // Visualize3D(fixture.getVersionedData, 15000, "after zoinks approved, before remove is approved")

    fixture.getAt(day(0), day(0)) shouldBe Some(zoinks.value)
    fixture.getIntersecting(interval(day(5), day(15)) x unbounded[LocalDate]) should contain theSameElementsAs List(
      (interval(day(1), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )

    fixture.incrementCurrentVersion()
    fixture.approveAll(unbounded[LocalDate] x unbounded[LocalDate]) // approves the unapproved remove
    // Visualize3D(fixture5.getVersionedData, 15000, "after remove is approved")

    fixture.getAt(day(0), day(0)) shouldBe None
    fixture.getIntersecting(interval(day(5), day(15)) x unbounded[LocalDate]) should contain theSameElementsAs List(
      (interval(day(6), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
