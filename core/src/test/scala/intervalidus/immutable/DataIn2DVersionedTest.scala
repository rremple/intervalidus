package intervalidus.immutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DVersionedTest extends AnyFunSuite with Matchers with DataIn2DVersionedBaseBehaviors:

  import DimensionalVersionedBase.VersionSelection
  import DiscreteInterval1D.*

  // increment current version with each data element
  def newDataIn2DVersioned(allData: Iterable[ValidData2D[String, Int, Int]]): DataIn2DVersioned[String, Int, Int] =
    allData.foldLeft(DataIn2DVersioned[String, Int, Int]()): (dataIn2DVersioned, validData) =>
      dataIn2DVersioned
        .set(validData)
        .incrementCurrentVersion()

  // shared
  testsFor(stringLookupTests("Immutable", newDataIn2DVersioned, DataIn2DVersioned(_), DataIn2DVersioned.of(_)))
  {
    given Experimental = Experimental("noSearchTree")
    testsFor(
      stringLookupTests(
        "Immutable [experimental noSearchTree]",
        newDataIn2DVersioned,
        DataIn2DVersioned(_),
        DataIn2DVersioned.of(_)
      )
    )
  }

  test("Immutable: Adding and removing data in intervals"):
    val empty: DataIn2DVersioned[String, Int, Int] = mutable.DataIn2DVersioned().toMutable.toImmutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(DiscreteDomain1D.Top)

    assertThrows[Exception]: // version too small
      empty.setCurrentVersion(DiscreteDomain1D.Bottom)

    val emptyAtMaxVersion = empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      emptyAtMaxVersion.incrementCurrentVersion()

    val allData = List(
      (interval(0, 9) x intervalFrom(0)) -> "Hello",
      (intervalFrom(10) x intervalTo(0)) -> "World"
    )
    val fixture1 = newDataIn2DVersioned(allData)
      .set((interval(5, 15) x unbounded[Int]) -> "to")
      .incrementCurrentVersion()
    val expectedData1 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .set((interval(20, 25) x unbounded[Int]) -> "!") // split
      .incrementCurrentVersion()
      .recompressAll()
    val expectedData2 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (interval(16, 19) x intervalTo(0)) -> "World",
      (interval(20, 25) x unbounded[Int]) -> "!",
      (intervalFrom(26) x intervalTo(0)) -> "World"
    )
    fixture2.getAll.toList shouldBe expectedData2

    // println(fixture2.toString)
    // format: off
    fixture2.toString shouldBe
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

    fixture2.setIfNoConflict((intervalTo(4) x unbounded[Int]) -> "Hey") shouldBe None
    val fixture3 = fixture2.setIfNoConflict((intervalTo(-1) x unbounded[Int]) -> "Hey") match
      case Some(f) =>
        f.incrementCurrentVersion()
          .set((intervalTo(4) x unbounded[Int]) -> "Hey")
          .remove(intervalFrom(21) x unbounded[Int])
          .incrementCurrentVersion()
      case None => fail("unexpected conflict")

    val expectedData3 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (interval(16, 19) x intervalTo(0)) -> "World",
      (intervalAt(20) x unbounded[Int]) -> "!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .set((intervalFrom(20) x intervalTo(0)) -> "World")
      .incrementCurrentVersion()
      .recompressAll()
    val expectedData4 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World",
      (intervalAt(20) x intervalFrom(1)) -> "!"
    )
    fixture4.getAll.toList shouldBe expectedData4

    val fixture5 = fixture4
      .remove(interval(5, 15) x unbounded[Int])
      .remove(intervalAt(20) x intervalFrom(1))
      .incrementCurrentVersion()
    val expectedData5 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture5.getAll.toList shouldBe expectedData5

    val fixture6 = fixture5
      .set((intervalFrom(1) x unbounded[Int]) -> "remove me")
      .remove(intervalFrom(1) x unbounded[Int])
    // needed? .recompressAll()
    val expectedData6 = List((intervalTo(0) x unbounded[Int]) -> "Hey")
    fixture6.getAll.toList shouldBe expectedData6

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
    val fixture2to4 = fixture2.applyDiffActions(actionsFrom2To4)
    fixture2to4.getAll(using VersionSelection(fixture4.getCurrentVersion)).toList shouldBe expectedData4

    val fixture2to6 = fixture2.syncWith(fixture6)
    fixture2to6.getAll(using VersionSelection(fixture6.getCurrentVersion)).toList shouldBe expectedData6

  test("Immutable: Mapping, flat mapping, etc."):
    val allData = List(
      (intervalTo(4) x intervalFrom(0)) -> "Hey",
      (intervalFrom(16) x intervalFrom(0)) -> "World"
    )

    val fixture1 = newDataIn2DVersioned(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.horizontal.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 =
      fixture1.map(d => d.interval.withHorizontalUpdate(_.toAfter(d.interval.horizontal.end)) -> (d.value + "!"))
    val expectedData2 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!"
    )

    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!!!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!!!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .flatMap(d => DataIn2DVersioned.of[String, Int, Int](d.value).map(x => d.interval -> x.value))
    val expectedData4 = expectedData3
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.interval.horizontal ⊆ intervalTo(10))
    val expectedData5 = List((intervalTo(5) x intervalFrom(0)) -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataIn2DVersioned.of[String, Int, Int](d.value))
    val expectedData6 = List((unbounded[Int] x unbounded[Int]) -> "Hey!!!")
    fixture6.getAll.toList shouldBe expectedData6
    fixture6.get shouldBe "Hey!!!"

    val fixture7 = fixture6.filter(_.value == "Planet")
    assert(fixture7.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture7.get

  test("Immutable: Compressing data in intervals"):
    val allData = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (intervalAt(5) x unbounded[Int]) -> "World",
      (intervalAt(6) x unbounded[Int]) -> "World",
      (intervalAt(7) x unbounded[Int]) -> "Hello",
      (interval(8, 9) x unbounded[Int]) -> "Hello",
      (intervalFrom(10) x unbounded[Int]) -> "Hello"
    )

    val fixture1 = DataIn2DVersioned
      .from(allData)
      .compress("Hello")
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (intervalAt(5) x unbounded[Int]) -> "World",
      (intervalAt(6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn2DVersioned
      .from(allData)
      .compressAll()
    val expectedData2 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    fixture2.getSelectedDataMutable.getAll.toList shouldBe expectedData2

  test("Immutable: Updating data in intervals"):
    val one: DataIn2DVersioned[String, Int, Int] = DataIn2DVersioned
      .of("value")
      .incrementCurrentVersion()

    val oneSplit = one.remove(intervalAt(0) x unbounded[Int]) // split
    val expectedDataSplit = List(
      (intervalTo(-1) x unbounded[Int]) -> "value",
      (intervalFrom(1) x unbounded[Int]) -> "value"
    )
    oneSplit.getAll.toList shouldBe expectedDataSplit
    oneSplit.getAt(0, 0) shouldBe None
    oneSplit.getAt(1, 1) shouldBe Some("value")

    val allData = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    val fixture1 = newDataIn2DVersioned(allData)
      .update((interval(5, 7) x unbounded[Int]) -> "World!")
      .incrementCurrentVersion()
    // needed? .recompressAll()
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 7) x unbounded[Int]) -> "World!",
      (intervalFrom(8) x unbounded[Int]) -> "Hello"
    )
    fixture1.getAll.toList shouldBe expectedData1

    // println(fixture1.toString)
    // format: off
    fixture1.toString shouldBe
      """current version = 4
        || -∞ .. 4                   | 5 .. 6                    | 7 .. 7                    | 8 .. +∞                   |
        || Hello (-∞..+∞) x [0..+∞)  |
        |                            | World (-∞..+∞) x [1..2]   |
        |                            | World! (-∞..+∞) x [3..+∞)                             |
        |                                                        | Hello (-∞..+∞) x [2..2]                               |
        |                                                                                    | Hello (-∞..+∞) x [3..+∞)  |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    val fixtureToReset = fixture1
      .remove(intervalTo(4) x unbounded[Int])(using VersionSelection(1))
    // needed? .recompressAll()
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
    val fixtureReset = fixtureToReset.resetToVersion(2)
    // println(fixtureReset.toString)
    fixtureReset.toString shouldBe
      """current version = 2
        || -∞ .. 4                  | 5 .. 6                   | 7 .. +∞                  |
        || Hello (-∞..+∞) x [0..0]  |
        |                           | World (-∞..+∞) x [1..+∞) |
        |                                                      | Hello (-∞..+∞) x [2..+∞) |
        |""".stripMargin.replaceAll("\r", "")

    // println(fixture1.getSelectedData(using VersionSelection(2)).toString)
    fixture1.getSelectedData(using VersionSelection(2)).toString shouldBe
      """|| -∞ .. 4        | 5 .. 6         | 7 .. +∞        |
         || Hello (-∞..+∞) |
         |                 | World (-∞..+∞) |
         |                                  | Hello (-∞..+∞) |
         |""".stripMargin.replaceAll("\r", "")

    // println(fixture1.getSelectedData.toString)
    fixture1.getSelectedData.toString shouldBe
      """|| -∞ .. 4         | 5 .. 7          | 8 .. +∞         |
         || Hello (-∞..+∞)  |
         |                  | World! (-∞..+∞) |
         |                                    | Hello (-∞..+∞)  |
         |""".stripMargin.replaceAll("\r", "")

    val fixture2 = fixture1
      .update((interval(5, 6) x unbounded[Int]) -> "Hello")
      .incrementCurrentVersion()
    val expectedData2 = List(
      (intervalTo(6) x unbounded[Int]) -> "Hello",
      (intervalAt(7) x unbounded[Int]) -> "World!",
      (intervalFrom(8) x unbounded[Int]) -> "Hello"
    )
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2
      .update((intervalFrom(6) x unbounded[Int]) -> "World!")
      .incrementCurrentVersion()
    // needed? .recompressAll()
    val expectedData3 = List(
      (intervalTo(5) x unbounded[Int]) -> "Hello",
      (intervalFrom(6) x unbounded[Int]) -> "World!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    // println(fixture3.toString)
    // format: off
    fixture3.toString shouldBe
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

    val fixture4 = fixture3.collapseVersionHistory
    // println(fixture4.toString)
    fixture4.toString shouldBe
      """current version = 0
        || -∞ .. 5                   | 6 .. +∞                   |
        || Hello (-∞..+∞) x [0..+∞)  |
        |                            | World! (-∞..+∞) x [0..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Immutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData2D[String, LocalDate, LocalDate]]
    ): DataIn2DVersioned[String, LocalDate, LocalDate] =
      allData.foldLeft(DataIn2DVersioned[String, LocalDate, LocalDate]()): (data, validData) =>
        data
          .set(validData)
          .incrementCurrentVersion()

    val fixture0: DataIn2DVersioned[String, LocalDate, LocalDate] = DataIn2DVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      (unbounded[LocalDate] x unbounded[LocalDate]) -> "Testing",
      (interval(day(1), day(15)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
    val fixture1 = timeboundVersionedString(allData)
    // Visualize(fixture1.getDataIn2D, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)) x unbounded[LocalDate])
    val fixture2 = fixture1.set(zoinks)(using VersionSelection.Unapproved)

    // Visualize(fixture2.getDataIn2D, 5000, "after Zoinks")

    fixture2.getAt(day(5), day(0)) shouldBe Some("Hello")
    fixture2.getAt(day(15), day(0)) shouldBe Some("World")
    fixture2.getAt(day(0), day(0)) shouldBe Some("Testing")
    fixture2.getAt(day(0), day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize(fixture.underlying2D, 15000, "before zoinks is approved")

    val fixture3 = fixture2
      .incrementCurrentVersion()
      .approve(zoinks) match // approves zoinks
      case Some(f) =>
        f.approve(zoinks) shouldBe None // already approved
        f
      case None => fail("unexpected failure to approve")

    val fixture4 = fixture3.remove(interval(day(-5), day(5)) x unbounded[LocalDate])(using VersionSelection.Unapproved)
    // Visualize(fixture4.underlying2D, 15000, "after zoinks approved, before remove is approved")

    fixture4.getAt(day(0), day(0)) shouldBe Some(zoinks.value)
    fixture4.getIntersecting(
      interval(day(5), day(15)) x unbounded[LocalDate]
    ) should contain theSameElementsAs List(
      (interval(day(1), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )

    val fixture5 = fixture4
      .incrementCurrentVersion()
      .approveAll(unbounded[LocalDate] x unbounded[LocalDate]) // approves the unapproved remove
    // Visualize(fixture5.underlying2D, 15000, "after remove is approved")

    fixture5.getAt(day(0), day(0)) shouldBe None
    fixture5.getIntersecting(
      interval(day(5), day(15)) x unbounded[LocalDate]
    ) should contain theSameElementsAs List(
      (interval(day(6), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
