package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In2D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}
import scala.language.implicitConversions

class DataIn2DVersionedTest extends AnyFunSuite with Matchers with DataIn2DVersionedBaseBehaviors:

  import DimensionalVersionedBase.VersionSelection
  import Interval1D.*

  // increment current version with each data element
  def newDataIn2DVersioned(
    allData: Iterable[ValidData[String, Dim[Int, Int]]]
  )(using CurrentDateTime): DataVersioned[String, Dim[Int, Int]] =
    allData.foldLeft(DataVersioned[String, Dim[Int, Int]]()): (dataIn2DVersioned, validData) =>
      dataIn2DVersioned
        .set(validData)
        .incrementCurrentVersion()

  def usingBuilder(data: Iterable[ValidData[String, Dim[Int, Int]]]): DataVersioned[String, Dim[Int, Int]] =
    val builder = DataVersioned.newBuilder[String, Dim[Int, Int]]()
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  def usingSetMany(data: Iterable[ValidData[String, Dim[Int, Int]]]): DataVersioned[String, Dim[Int, Int]] =
    DataVersioned[String, Dim[Int, Int]]() ++ data

  // shared
  testsFor(stringLookupTests("Immutable", newDataIn2DVersioned, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Immutable (builder)", usingBuilder, DataVersioned(_), DataVersioned.of(_)))
  testsFor(stringLookupTests("Immutable (setMany)", usingSetMany, DataVersioned(_), DataVersioned.of(_)))

  test("Immutable: Adding and removing data in intervals"):
    val dayZero = LocalDateTime.of(2025, 8, 1, 8, 0)
    val empty: DataVersioned[String, Dim[Int, Int]] =
      mutable.DataVersioned[String, Dim[Int, Int]]().toMutable.toImmutable

    assertThrows[Exception]: // version too large
      empty.setCurrentVersion(Int.MaxValue)

    val emptyAtMaxVersion = empty.setCurrentVersion(Int.MaxValue - 1) // last approved version
    assertThrows[Exception]: // wow, ran out of versions!
      emptyAtMaxVersion.incrementCurrentVersion()

    val allData = List(
      (interval(0, 9) x intervalFrom(0)) -> "Hello",
      (intervalFrom(10) x intervalTo(0)) -> "World"
    )
    val fixture0 = newDataIn2DVersioned(allData)(using CurrentDateTime.simulated(LocalDateTime.of(2025, 8, 1, 8, 0)))
    fixture0.getByHeadDimension(0).getAt(0) shouldBe Some("Hello")
    fixture0.getByHeadDimension[Int](Domain1D.Top).getAt(0) shouldBe Some("World")
    fixture0.getByDimension[Int, Domain.In1D[Int]](1, 0).getAt(0) shouldBe Some("Hello")
    fixture0.getByDimension[Int, Domain.In1D[Int]](1, Domain1D.Bottom).getAt(10) shouldBe Some("World")

    val fixture1 = fixture0
      .set((interval(5, 15) x unbounded[Int]) -> "to")
      .incrementCurrentVersion()(using CurrentDateTime.simulated(LocalDateTime.of(2025, 8, 2, 8, 0)))
    val expectedData1 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .set((interval(20, 25) x unbounded[Int]) -> "!") // split
      .incrementCurrentVersion()(using CurrentDateTime.simulated(LocalDateTime.of(2025, 8, 2, 9, 0)))
      .recompressAll() // not needed, but addresses coverage gap
    val expectedData2 = List(
      (interval(0, 4) x intervalFrom(0)) -> "Hello",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (interval(16, 19) x intervalTo(0)) -> "World",
      (interval(20, 25) x unbounded[Int]) -> "!",
      (intervalFrom(26) x intervalTo(0)) -> "World"
    )
    fixture2.getAll.toList shouldBe expectedData2

    fixture2.getVersionTimestamps.toList should contain theSameElementsAs List(
      0 -> (dayZero, "init"),
      1 -> (dayZero, "incremented"),
      2 -> (dayZero, "incremented"),
      3 -> (dayZero.plusDays(1), "incremented"),
      4 -> (dayZero.plusDays(1).plusHours(1), "incremented")
    )
    // println(fixture2.toString)
    // format: off
    fixture2.toString shouldBe
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

    fixture2.setIfNoConflict((intervalTo(4) x unbounded[Int]) -> "Hey") shouldBe None
    val fixture3 = fixture2.setIfNoConflict((intervalTo(-1) x unbounded[Int]) -> "Hey") match
      case Some(f) =>
        val f2 = f.incrementCurrentVersion() +
          ((intervalTo(4) x unbounded[Int]) -> "Hey") -
          (intervalFrom(21) x unbounded[Int])
        f2.incrementCurrentVersion()
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
    // if needed: .recompressAll()
    val expectedData4 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (interval(5, 15) x unbounded[Int]) -> "to",
      (intervalFrom(16) x intervalTo(0)) -> "World",
      (intervalAt(20) x intervalFrom(1)) -> "!"
    )
    fixture4.getAll.toList shouldBe expectedData4

    val fixture5 = (
      fixture4 -- Seq(
        interval(5, 15) x unbounded[Int],
        intervalAt(20) x intervalFrom(1)
      )
    ).incrementCurrentVersion()
    val expectedData5 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hey",
      (intervalFrom(16) x intervalTo(0)) -> "World"
    )
    fixture5.getAll.toList shouldBe expectedData5

    val fixture6 = fixture5
      .set((intervalFrom(1) x unbounded[Int]) -> "remove me")
      .remove(intervalFrom(1) x unbounded[Int])
    // if needed: .recompressAll()
    val expectedData6 = List((intervalTo(0) x unbounded[Int]) -> "Hey")
    fixture6.getAll.toList shouldBe expectedData6

    val fixture7 = fixture6.fill(Interval.unbounded[Dim[Int, Int]] -> "Filled")
    val expectedFilled = List((intervalTo(0) x unbounded[Int]) -> "Hey", (intervalFrom(1) x unbounded[Int]) -> "Filled")
    fixture7.getAll.toList shouldBe expectedFilled
    fixture7.intervals("Filled") should contain theSameElementsAs List(intervalFrom(1) x unbounded[Int])
    fixture7.removeValue("Filled").getAll.toList shouldBe expectedData6

    val data1 = DataVersioned.from(
      Seq((intervalFrom(1).to(3) x unbounded[Int]) -> "A", (intervalFrom(5) x unbounded[Int]) -> "B")
    )
    val data2 = DataVersioned.from(
      Seq((intervalFrom(2).to(4) x unbounded[Int]) -> "C", (intervalFrom(6) x unbounded[Int]) -> "D")
    )

    // Default merge operation will "prioritize left"
    val defaultMerge = data1.merge(data2)
    defaultMerge.getAll.toList shouldBe List(
      (intervalFrom(1).to(3) x unbounded[Int]) -> "A",
      (intervalFromAfter(3).to(4) x unbounded[Int]) -> "C",
      (intervalFrom(5) x unbounded[Int]) -> "B"
    )
    // Custom merge operation will combine overlapping elements
    val customMerge = data1.merge(data2, _ + _)
    customMerge.getAll.toList shouldBe List(
      (intervalFrom(1).toBefore(2) x unbounded[Int]) -> "A",
      (intervalFrom(2).to(3) x unbounded[Int]) -> "AC",
      (intervalFromAfter(3).to(4) x unbounded[Int]) -> "C",
      (intervalFrom(5).toBefore(6) x unbounded[Int]) -> "B",
      (intervalFrom(6) x unbounded[Int]) -> "BD"
    )
    defaultMerge.getVersionTimestamps should contain theSameElementsAs customMerge.getVersionTimestamps

    val selfMerge = data1.merge(data1)
    selfMerge.getAll should contain theSameElementsAs data1.getAll
    selfMerge.getVersionTimestamps should contain theSameElementsAs data1.getVersionTimestamps

    import DiffAction.*

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2)
    actionsFrom2To4 shouldBe Iterable(
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
    actionsFrom4To6 shouldBe Iterable(
      Update((interval(2, 6) x interval(5, 15) x unbounded[Int]) -> "to"),
      Update((interval(2, 7) x interval(16, 19) x intervalTo(0)) -> "World"),
      Update((intervalFrom(5) x intervalTo(0) x unbounded[Int]) -> "Hey"),
      Create((interval(5, 7) x interval(1, 4) x unbounded[Int]) -> "Hey"),
      Update((interval(6, 7) x intervalFrom(20) x intervalTo(0)) -> "World"),
      Update((intervalAt(6) x intervalAt(20) x intervalFrom(1)) -> "!")
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
    fixture1.copy shouldBe fixture1

    val concat = fixture1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(horizontal(d.interval).toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2a = fixture1
      .mapIntervals(i => i.to(i.end.rightAdjacent))
      .mapValues(_ + "!")

    val fixture2b = fixture1.map: d =>
      d.interval.to(d.interval.end.rightAdjacent) -> (d.value + "!")
    val expectedData2 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!"
    )

    fixture2a.getAll.toList shouldBe expectedData2
    fixture2b.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2b.mapValues(_ + "!!")
    val expectedData3 = List(
      (intervalTo(5) x intervalFrom(0)) -> "Hey!!!",
      (intervalFrom(16) x intervalFrom(0)) -> "World!!!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3
      .flatMap(d => DataVersioned.of[String, Dim[Int, Int]](d.value).map(x => d.interval -> x.value))
    val expectedData4 = expectedData3
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5a = fixture4.collect:
      case v if horizontal(v.interval) ⊆ intervalTo(10) => v
    val fixture5b = fixture4.filter(v => horizontal(v.interval) ⊆ intervalTo(10))
    val expectedData5 = List((intervalTo(5) x intervalFrom(0)) -> "Hey!!!")
    fixture5b.getAll.toList shouldBe expectedData5
    fixture5a.getAll.toList shouldBe expectedData5
    assert(!fixture5b.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5b.get

    val fixture6 = fixture5b.flatMap(d => DataVersioned.of[String, Dim[Int, Int]](d.value))
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

    val fixture1 = DataVersioned
      .from(allData)
      .compress("Hello")
    val expectedData1 = List(
      (intervalTo(4) x unbounded[Int]) -> "Hello",
      (interval(5, 6) x unbounded[Int]) -> "World",
      (intervalFrom(7) x unbounded[Int]) -> "Hello"
    )
    // getSelectedDataMutable compresses
    fixture1.getSelectedDataMutable.getAll.toList shouldBe expectedData1

  test("Immutable: Updating data in intervals"):
    val one: DataVersioned[String, Dim[Int, Int]] = DataVersioned
      .of[String, Dim[Int, Int]]("value")
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
    // if needed: .recompressAll()
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
        || 0 .. 0                   | 1 .. 1                   | 2 .. 2                   | 3 .. +∞                  |
        || Hello (-∞..4] x (-∞..+∞)                                                                                  |
        |                           | World [5..6] x (-∞..+∞)                             |
        |                                                      | Hello [7..+∞) x (-∞..+∞) |
        |                                                                                 | World! [5..7] x (-∞..+∞) |
        |                                                                                 | Hello [8..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")
    // format: on

    val fixtureToReset = fixture1
      .remove(intervalTo(4) x unbounded[Int])(using VersionSelection(1))
    // if needed: .recompressAll()
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
    val fixtureReset = fixtureToReset.resetToVersion(2)
    // println(fixtureReset.toString)
    fixtureReset.toString shouldBe
      """current version = 2
        || 0 .. 0                   | 1 .. 1                   | 2 .. +∞                  |
        || Hello (-∞..4] x (-∞..+∞) |
        |                           | World [5..6] x (-∞..+∞)                             |
        |                                                      | Hello [7..+∞) x (-∞..+∞) |
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
    // if needed: .recompressAll()
    val expectedData3 = List(
      (intervalTo(5) x unbounded[Int]) -> "Hello",
      (intervalFrom(6) x unbounded[Int]) -> "World!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    // println(fixture3.toString)
    // format: off
    fixture3.toString shouldBe
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

    val fixture4 = fixture3.collapseVersionHistory
    // println(fixture4.toString)
    fixture4.toString shouldBe
      """current version = 0
        || 0 .. +∞                   |
        || Hello (-∞..5] x (-∞..+∞)  |
        || World! [6..+∞) x (-∞..+∞) |
        |""".stripMargin.replaceAll("\r", "")

  test("Immutable: Approvals"):
    // increment current version with each data element
    def timeboundVersionedString(
      allData: Iterable[ValidData[String, Dim[LocalDate, LocalDate]]]
    ): DataVersioned[String, Dim[LocalDate, LocalDate]] =
      allData.foldLeft(DataVersioned[String, Dim[LocalDate, LocalDate]]()): (data, validData) =>
        data
          .set(validData)
          .incrementCurrentVersion()

    val fixture0: DataVersioned[String, Dim[LocalDate, LocalDate]] = DataVersioned()
    assert(fixture0.getAll.isEmpty)

    val allData = List(
      (unbounded[LocalDate] x unbounded[LocalDate]) -> "Testing",
      (interval(day(1), day(15)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
    val fixture1 = timeboundVersionedString(allData)
    // Visualize3D(fixture1.getVersionedData, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)) x unbounded[LocalDate])
    val fixture2 = fixture1.set(zoinks)(using VersionSelection.Unapproved)

//     Visualize3D(fixture2.getVersionedData, 5000, "after Zoinks")

    fixture2.getAt(day(5), day(0)) shouldBe Some("Hello")
    fixture2.getAt(day(15), day(0)) shouldBe Some("World")
    fixture2.getAt(day(0), day(0)) shouldBe Some("Testing")
    fixture2.getAt(day(0), day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize3D(fixture2.getVersionedData, 15000, "before zoinks is approved")

    val fixture3 = fixture2
      .incrementCurrentVersion()
      .approve(zoinks) match // approves zoinks
      case Some(f) =>
        f.approve(zoinks) shouldBe None // already approved
        f
      case None => fail("unexpected failure to approve")

    val fixture4 = fixture3.remove(interval(day(-5), day(5)) x unbounded[LocalDate])(using VersionSelection.Unapproved)
    // Visualize3D(fixture4.getVersionedData, 15000, "after zoinks approved, before remove is approved")

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
    // Visualize3D(fixture5.getVersionedData, 15000, "after remove is approved")

    fixture5.getAt(day(0), day(0)) shouldBe None
    fixture5.getIntersecting(
      interval(day(5), day(15)) x unbounded[LocalDate]
    ) should contain theSameElementsAs List(
      (interval(day(6), day(9)) x unbounded[LocalDate]) -> "Hello",
      (intervalFrom(day(10)) x unbounded[LocalDate]) -> "World"
    )
