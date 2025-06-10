package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn4DTest extends AnyFunSuite with Matchers with DataIn4DBaseBehaviors with ImmutableBaseBehaviors:

  import Interval1D.*

  // shared
  testsFor(stringLookupTests("Immutable", DataIn4D(_), DataIn4D.of(_)))
  testsFor(
    stringLookupTests("Immutable [experimental]", DataIn4D(_), DataIn4D.of(_))(using Experimental("noSearchTree"))
  )
  testsFor(
    immutableBaseTests[
      Domain4D[LocalDate, LocalDate, Int, Int],
      Interval4D[LocalDate, LocalDate, Int, Int],
      ValidData4D[String, LocalDate, LocalDate, Int, Int],
      DiffAction4D[String, LocalDate, LocalDate, Int, Int],
      DataIn4D[String, LocalDate, LocalDate, Int, Int]
    ](
      ds => DataIn4D(ds),
      i => unbounded[LocalDate] x unbounded[LocalDate] x i x unbounded[Int],
      (i, s) => (unbounded[LocalDate] x unbounded[LocalDate] x i x unbounded[Int]) -> s,
      Interval4D.unbounded
    )
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData4D[String, Int, Int, Int, Int]*
  )(
    removeOrUpdateInterval: Interval4D[Int, Int, Int, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val hypercube = interval(-9, 9) x interval(-9, 9) x interval(-9, 9) x interval(-9, 9)
    val fixture = DataIn4D.of(hypercube -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ hypercube match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the cube")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    val removeFixture = fixture.remove(removeOrUpdateInterval).recompressAll()
    val updateFixture = fixture.update(removeOrUpdateInterval -> updateValue).recompressAll()
    try assertResult(removeExpected)(removeFixture.getAll.toList)
    catch
      case ex: Exception =>
        println("Test failed (in remove), here's the actual remove data:")
        println(removeFixture.getAll.map(_.toCodeLikeString).mkString(",\n"))
        throw ex
    try assertResult(updateExpected)(updateFixture.getAll.toList)
    catch
      case ex: Exception =>
        println("Test failed (in update), here's the actual update data:")
        println(updateFixture.getAll.map(_.toCodeLikeString).mkString(",\n"))
        throw ex

  testsFor(removeOrUpdateTests("Immutable"))
  testsFor(removeOrUpdateTests("Immutable [experimental noSearchTree]")(using Experimental("noSearchTree")))

  def vertical4D[T2: DiscreteValue](interval3: Interval1D[T2]): Interval4D[LocalDate, LocalDate, T2, Int] =
    unbounded[LocalDate] x unbounded[LocalDate] x interval3 x unbounded[Int]

  test("Immutable: Constructors and getting data by index"):
    val empty: DataIn4D[String, Int, Int, Int, Int] = DataIn4D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10) x unbounded[Int]) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11) x unbounded[Int]) -> "World"
    )
    val fixture = mutable.DataIn4D(allData).toMutable.toImmutable

    fixture.getByHorizontalIndex(dayZero).getByHorizontalIndex(dayZero).getByHorizontalIndex(0).getAt(0) shouldBe Some(
      "Hello"
    )
    fixture.getByVerticalIndex(dayZero).getByHorizontalIndex(dayZero).getByVerticalIndex(0).getAt(0) shouldBe Some(
      "Hello"
    )
    fixture.getByDepthIndex(11).getByDepthIndex(0).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")
    fixture.getByFourthIndex(0).getByDepthIndex(11).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")

  test("Immutable: Simple toString"):
    val fixturePadData = DataIn4D
      .of[String, Int, Int, Int, Int]("H")
      .set((intervalFrom(1) x intervalTo(0) x interval(1, 9) x unbounded[Int]) -> "W")
      .recompressAll()
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 0                          | 1 .. +∞                          |
         |                                   | W (-∞..0] x [1..9] x (-∞..+∞)    |
         || H (-∞..+∞) x (-∞..0] x (-∞..+∞)                                     |
         || H (-∞..+∞) x [1..9] x (-∞..+∞)   |
         || H (-∞..+∞) x [10..+∞) x (-∞..+∞)                                    |
         |                                   | H [1..+∞) x [1..9] x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

    val concat = fixturePadData.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.start.toString).append(" ")
    concat.result() shouldBe
      "H->{-∞, -∞, -∞, -∞} H->{-∞, -∞, 1, -∞} H->{-∞, -∞, 10, -∞} W->{1, -∞, 1, -∞} H->{1, 1, 1, -∞} "

    val fixturePadLabel = DataIn4D
      .of[String, Int, Int, Int, Int]("Helloooooooooo")
      .set((intervalFrom(1) x unbounded[Int] x unbounded[Int] x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 0                                       | 1 .. +∞                                       |
         || Helloooooooooo (-∞..+∞) x (-∞..+∞) x (-∞..+∞) |
         |                                                | Wooooooorld (-∞..+∞) x (-∞..+∞) x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

  test("Immutable [experimental noSearchTree]: Constructors and getting data by index"):
    given Experimental = Experimental("noSearchTree")

    val empty: DataIn4D[String, Int, Int, Int, Int] = DataIn4D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10) x unbounded[Int]) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11) x unbounded[Int]) -> "World"
    )
    val fixture = mutable.DataIn4D(allData).toMutable.toImmutable

    fixture.getByHorizontalIndex(dayZero).getByHorizontalIndex(dayZero).getByHorizontalIndex(0).getAt(0) shouldBe Some(
      "Hello"
    )
    fixture.getByVerticalIndex(dayZero).getByHorizontalIndex(dayZero).getByVerticalIndex(0).getAt(0) shouldBe Some(
      "Hello"
    )
    fixture.getByDepthIndex(11).getByDepthIndex(0).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")
    fixture.getByFourthIndex(0).getByDepthIndex(11).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")

  test("Immutable: Diff actions"):
    val expectedData2 = List(
      (unboundedDate x unboundedDate x interval(0, 4) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15) x unbounded[Int]) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19) x unbounded[Int]) -> "World",
      (unboundedDate x unboundedDate x interval(20, 25) x unbounded[Int]) -> "!",
      (unboundedDate x unboundedDate x intervalFrom(26) x unbounded[Int]) -> "World"
    )
    val fixture2 = DataIn4D(expectedData2)
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(4) x unbounded[Int]) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15) x unbounded[Int]) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )
    val fixture3 = DataIn4D(expectedData3)
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(4) x unbounded[Int]) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )

    val fixture4 = DataIn4D(expectedData4)
    val fixture5 = fixture4
      .set((intervalFrom(day(1)) x unboundedDate x intervalFrom(1) x unbounded[Int]) -> "remove me")
      .remove(intervalFrom(day(1)) x unboundedDate x intervalFrom(1) x unbounded[Int])
      .recompressAll()
    val expectedData5 = List(
      (unboundedDate x unboundedDate x intervalTo(0) x unbounded[Int]) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x interval(1, 4) x unbounded[Int]) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )
    fixture5.getAll.toList shouldBe expectedData5

    import DiffAction4D.*
    import Domain1D.{Bottom, Point}

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(vertical4D(intervalTo(4)) -> "Hey"),
      Delete(Domain4D[Int, Int, Int, Int](Bottom, Bottom, 0, Bottom)),
      Update(vertical4D(intervalFrom(16)) -> "World"),
      Delete(Domain4D[Int, Int, Int, Int](Bottom, Bottom, 20, Bottom)),
      Delete(Domain4D[Int, Int, Int, Int](Bottom, Bottom, 26, Bottom))
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction4D.Create((unbounded x unbounded x intervalTo(4) x unbounded) -> \"Hey\")",
      "DiffAction4D.Delete(Bottom x Bottom x Point(0) x Bottom)",
      "DiffAction4D.Update((unbounded x unbounded x intervalFrom(16) x unbounded) -> \"World\")",
      "DiffAction4D.Delete(Bottom x Bottom x Point(20) x Bottom)",
      "DiffAction4D.Delete(Bottom x Bottom x Point(26) x Bottom)"
    )

    val actionsFrom3To5 = fixture5.diffActionsFrom(fixture3)
    actionsFrom3To5.toList shouldBe List(
      Update((unboundedDate x unboundedDate x intervalTo(0) x unbounded[Int]) -> "Hey"),
      Create((intervalTo(day(0)) x unboundedDate x interval(1, 4) x unbounded[Int]) -> "Hey"),
      Delete(Domain4D[Int, Int, Int, Int](Bottom, Bottom, Point(5), Bottom)),
      Update((intervalTo(day(0)) x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World")
    )

    val f3Sync = fixture2.applyDiffActions(actionsFrom2To3)
    f3Sync.getAll.toList shouldBe expectedData3

    val f5sync = fixture2.syncWith(fixture5)
    f5sync.getAll.toList shouldBe expectedData5

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4) x unbounded[Int]) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )

    val fixture1 = DataIn4D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val fixture2 = fixture1.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.withDepthUpdate(_.to(d.interval.depth.end.rightAdjacent))
      )
    )
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(5) x unbounded[Int]) -> "Hey!",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World!"
    )
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(5) x unbounded[Int]) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World!!!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 =
      fixture3.flatMap: d =>
        DataIn4D.of[String, LocalDate, LocalDate, Int, Int](d.value).map(x => d.interval -> x.value)
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(5) x unbounded[Int]) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World!!!"
    )
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 =
      fixture4.filter(_.value == "Hey!!!").flatMap(d => DataIn4D.of[String, LocalDate, LocalDate, Int, Int](d.value))
    val expectedData5 = List((unboundedDate x unboundedDate x unbounded[Int] x unbounded[Int]) -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    fixture5.get shouldBe "Hey!!!"

  test("Immutable: Updating data in intervals - bounded r1"):
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(-6) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2) x unbounded[Int]) -> "World!",
      (unboundedDate x unboundedDate x interval(-1, 1) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10) x unbounded[Int]) -> "Hello"
    )
    val fixture3 = DataIn4D(expectedData3)
    val fixture4 = fixture3
      .set((intervalFrom(day(0)) x unboundedDate x intervalFrom(1) x unbounded[Int]) -> "update me")
      .update((intervalFrom(day(1)) x unboundedDate x intervalFrom(0) x unbounded[Int]) -> "updated me")
      .recompressAll()

    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(-6) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2) x unbounded[Int]) -> "World!",
      (unboundedDate x unboundedDate x intervalAt(-1) x unbounded[Int]) -> "Hello",
      (intervalTo(day(0)) x unboundedDate x intervalAt(0) x unbounded[Int]) -> "Hello",
      (intervalTo(day(-1)) x unboundedDate x intervalAt(1) x unbounded[Int]) -> "Hello",
      (intervalTo(day(-1)) x unboundedDate x intervalFrom(10) x unbounded[Int]) -> "Hello",
      (intervalAt(day(0)) x unboundedDate x intervalFrom(1) x unbounded[Int]) -> "update me",
      (intervalFrom(day(1)) x unboundedDate x intervalFrom(0) x unbounded[Int]) -> "updated me"
    )
    fixture4.getAll.toList shouldBe expectedData4
    fixture4.domain.toList shouldBe List(
      vertical4D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical4D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x unboundedDate x interval(2, 9) x unbounded[Int] // the remaining bits of the 7th and 8th
    )
    fixture4.domainComplement.toList shouldBe List(
      intervalToBefore(day(0)) x unboundedDate x interval(2, 9) x unbounded[Int]
    )
