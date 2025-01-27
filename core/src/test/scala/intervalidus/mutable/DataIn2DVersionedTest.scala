package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DVersionedTest extends AnyFunSuite with Matchers with DataIn2DVersionedBaseBehaviors:

  import DimensionalVersionedBase.VersionSelection
  import Interval1D.*

  // increment current version with each data element
  def newDataIn2DVersioned(allData: Iterable[ValidData2D[String, Int, Int]]): DataIn2DVersioned[String, Int, Int] =
    val dataIn2DVersioned = DataIn2DVersioned[String, Int, Int]()
    allData.foreach: validData =>
      dataIn2DVersioned.set(validData)
      dataIn2DVersioned.incrementCurrentVersion()
    dataIn2DVersioned

  // shared
  testsFor(stringLookupTests("Mutable", newDataIn2DVersioned, DataIn2DVersioned(_), DataIn2DVersioned.of(_)))
  {
    given Experimental = Experimental("noSearchTree")
    testsFor(
      stringLookupTests(
        "Mutable [experimental noSearchTree]",
        newDataIn2DVersioned,
        DataIn2DVersioned(_),
        DataIn2DVersioned.of(_)
      )
    )
  }

  test("Mutable: Adding and removing data in intervals"):
    val empty: DataIn2DVersioned[String, Int, Int] = immutable.DataIn2DVersioned().toImmutable.toMutable

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
    fixture.recompressAll()
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
        || 0 .. 4                  | 5 .. 9                  | 10 .. 15                | 16 .. 19                | 20 .. 25                | 26 .. +∞                |
        |                                                    | World (-∞..0] x [1..1]                                                                                |
        |                                                                              | World (-∞..0] x [2..2]                                                      |
        |                                                                              | World (-∞..0] x [3..+∞) |
        |                                                                                                                                  | World (-∞..0] x [3..+∞) |
        || Hello [0..+∞) x [0..1]                            |
        || Hello [0..+∞) x [2..+∞) |
        |                          | to (-∞..+∞) x [2..+∞)                             |
        |                                                                                                        | ! (-∞..+∞) x [3..+∞)    |
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
    fixture.recompressAll()
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
    // needed? .recompressAll()
    val expectedData6 = List((intervalTo(0) x unbounded[Int]) -> "Hey")
    fixture.getAll.toList shouldBe expectedData6

    val fixture6 = fixture.copy

    import DiffAction3D.*

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2)
    actionsFrom2To4.toList shouldBe List(
      Create((intervalTo(-1) x unbounded[Int] x intervalAt(4)) -> "Hey"),
      Create((intervalTo(4) x unbounded[Int] x intervalFrom(5)) -> "Hey"),
      Update((interval(0, 4) x intervalFrom(0) x interval(2, 4)) -> "Hello"),
      Update((interval(16, 19) x intervalTo(0) x interval(3, 5)) -> "World"),
      Create((intervalFrom(16) x intervalTo(0) x intervalFrom(6)) -> "World"),
      Update((interval(20, 25) x unbounded[Int] x interval(3, 4)) -> "!"),
      Create((intervalAt(20) x unbounded[Int] x intervalAt(5)) -> "!"),
      Create((intervalAt(20) x intervalFrom(1) x intervalFrom(6)) -> "!"),
      Update((intervalFrom(26) x intervalTo(0) x interval(3, 4)) -> "World")
    )
    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6.toList shouldBe List(
      Update((intervalTo(4) x unbounded[Int] x interval(5, 7)) -> "Hey"),
      Create((intervalTo(0) x unbounded[Int] x intervalFrom(8)) -> "Hey"),
      Update((interval(5, 15) x unbounded[Int] x interval(2, 6)) -> "to"),
      Update((intervalFrom(16) x intervalTo(0) x interval(6, 7)) -> "World"),
      Update((intervalAt(20) x intervalFrom(1) x intervalAt(6)) -> "!")
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
      b.append(d.value).append("->").append(d.interval.horizontal.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture.map: d =>
      d.interval.withHorizontalUpdate(_.to(d.interval.horizontal.end.rightAdjacent)) -> (d.value + "!")
    val expectedData2 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!"
    )

    fixture.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!!!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!!!"
    )
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap(d => DataIn2DVersioned(Seq(d)))
    val expectedData4 = expectedData3
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.filter(_.interval.horizontal ⊆ intervalTo(10))
    val expectedData5 = List((intervalTo(5) x intervalFrom(0)) -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataIn2DVersioned.of[String, Int, Int](d.value))
    val expectedData6 = List((unbounded[Int] x unbounded[Int]) -> "Hey!!!")
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

    val fixture1 = DataIn2DVersioned.from(allData)
    fixture1.compress("Hello")
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (intervalAt(5) x unbounded[Int]) -> "World",
      (intervalAt(6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn2DVersioned.from(allData)
    fixture2.compressAll()
    val expectedData2 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture2.getSelectedDataMutable.getAll.toList shouldBe expectedData2

  test("Mutable: Updating data in intervals"):
    val one: DataIn2DVersioned[String, Int, Int] = DataIn2DVersioned.of("value")
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
    // needed? .recompressAll()
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
        || -∞ .. 4                   | 5 .. 6                    | 7 .. 7                    | 8 .. +∞                   |
        || Hello (-∞..+∞) x [0..+∞)  |
        |                            | World (-∞..+∞) x [1..2]   |
        |                            | World! (-∞..+∞) x [3..+∞)                             |
        |                                                        | Hello (-∞..+∞) x [2..2]                               |
        |                                                                                    | Hello (-∞..+∞) x [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    val fixtureToReset = fixture.copy
    fixtureToReset.remove(intervalTo(4) x unbounded[Int])(using VersionSelection(1))
    // needed? fixtureToReset.recompressAll()
    // println(fixtureToReset.toString)
    // format: off
    fixtureToReset.toString shouldBe
      """current version = 4
        || -∞ .. 4                   | 5 .. 6                    | 7 .. 7                    | 8 .. +∞                   |
        || Hello (-∞..+∞) x [0..0]   |
        |                            | World (-∞..+∞) x [1..2]   |
        |                            | World! (-∞..+∞) x [3..+∞)                             |
        |                                                        | Hello (-∞..+∞) x [2..2]                               |
        |                                                                                    | Hello (-∞..+∞) x [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")
    // format: on
    fixtureToReset.resetToVersion(2)
    // println(fixtureToReset.toString)
    fixtureToReset.toString shouldBe
      """current version = 2
        || -∞ .. 4                  | 5 .. 6                   | 7 .. +∞                  |
        || Hello (-∞..+∞) x [0..0]  |
        |                           | World (-∞..+∞) x [1..+∞) |
        |                                                      | Hello (-∞..+∞) x [2..+∞) |
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
    // needed? fixture.recompressAll()
    val expectedData3 = List(
      (intervalTo(5) x unbounded[Int]) -> "Hello",
      (intervalFrom(6) x unbounded[Int]) -> "World!"
    )
    fixture.getAll.toList shouldBe expectedData3

    // println(fixture.toString)
    // format: off
    fixture.toString shouldBe
      """current version = 6
        || -∞ .. 4                   | 5 .. 5                    | 6 .. 6                    | 7 .. 7                    | 8 .. +∞                   |
        || Hello (-∞..+∞) x [0..+∞)  |
        |                            | World (-∞..+∞) x [1..2]                               |
        |                            | World! (-∞..+∞) x [3..3]                                                          |
        |                            | Hello (-∞..+∞) x [4..4]                               |
        |                            | Hello (-∞..+∞) x [5..+∞)  |
        |                                                        | World! (-∞..+∞) x [5..+∞)                                                         |
        |                                                                                    | Hello (-∞..+∞) x [2..2]                               |
        |                                                                                    | World! (-∞..+∞) x [4..4]  |
        |                                                                                                                | Hello (-∞..+∞) x [3..4]   |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    fixture.collapseVersionHistory
    // println(fixture.toString)
    fixture.toString shouldBe
      """current version = 0
        || -∞ .. 5                   | 6 .. +∞                   |
        || Hello (-∞..+∞) x [0..+∞)  |
        |                            | World! (-∞..+∞) x [0..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Mutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData2D[String, LocalDate, LocalDate]]
    ): DataIn2DVersioned[String, LocalDate, LocalDate] =
      val data = DataIn2DVersioned[String, LocalDate, LocalDate]()
      allData.foreach: validData =>
        data.set(validData)(using VersionSelection.Current)
        data.incrementCurrentVersion()
      data

    val fixture0: DataIn2DVersioned[String, LocalDate, LocalDate] = DataIn2DVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      (unbounded[LocalDate] x unbounded[LocalDate]) -> "Testing",
      (interval(day(1), day(15)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
    val fixture = timeboundVersionedString(allData)
    // Visualize(fixture1.getDataIn2D, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)) x unbounded[LocalDate])
    fixture.set(zoinks)(using VersionSelection.Unapproved)
    // Visualize(fixture.getDataIn2D, 5000, "after Zoinks")

    fixture.getAt(day(5), day(0)) shouldBe Some("Hello")
    fixture.getAt(day(15), day(0)) shouldBe Some("World")
    fixture.getAt(day(0), day(0)) shouldBe Some("Testing")
    fixture.getAt(day(0), day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize(fixture.underlying2D, 15000, "before zoinks is approved")

    fixture.incrementCurrentVersion()
    // fixture.approveAll(unbounded) // approves zoinks
    assert(fixture.approve(zoinks)) // approves zoinks
    assert(!fixture.approve(zoinks)) // already approved

    fixture.remove(interval(day(-5), day(5)) x unbounded[LocalDate])(using VersionSelection.Unapproved)
    // Visualize(fixture.underlying2D, 15000, "after zoinks approved, before remove is approved")

    fixture.getAt(day(0), day(0)) shouldBe Some(zoinks.value)
    fixture.getIntersecting(interval(day(5), day(15)) x unbounded[LocalDate]) should contain theSameElementsAs List(
      (interval(day(1), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )

    fixture.incrementCurrentVersion()
    fixture.approveAll(unbounded[LocalDate] x unbounded[LocalDate]) // approves the unapproved remove
    // Visualize(fixture5.underlying2D, 15000, "after remove is approved")

    fixture.getAt(day(0), day(0)) shouldBe None
    fixture.getIntersecting(interval(day(5), day(15)) x unbounded[LocalDate]) should contain theSameElementsAs List(
      (interval(day(6), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
