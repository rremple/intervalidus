package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In4D as Dim
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn4DTest extends AnyFunSuite with Matchers with DataIn4DBaseBehaviors with MutableBaseBehaviors:

  import Interval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", Data(_), Data.of(_)))

  testsFor(
    mutableBaseTests[Dim[LocalDate, LocalDate, Int, Int], Data[String, Dim[LocalDate, LocalDate, Int, Int]]](
      Data(_),
      i => unbounded[LocalDate] x unbounded[LocalDate] x i x unbounded[Int],
      d =>
        d.copy(
          value = d.value + "!",
          interval = d.interval.to(d.interval.end.leftAdjacent)
        )
    )
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData[String, Dim[Int, Int, Int, Int]]*
  )(
    removeOrUpdateInterval: Interval[Dim[Int, Int, Int, Int]],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val hypercube = interval(-9, 9) x interval(-9, 9) x interval(-9, 9) x interval(-9, 9)
    val removeFixture = Data.of(hypercube -> "World")
    val updateFixture = Data.of(hypercube -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ hypercube match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the cube")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    removeFixture.remove(removeOrUpdateInterval)
    removeFixture.recompressAll()
    updateFixture.update(removeOrUpdateInterval -> updateValue)
    updateFixture.recompressAll()
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

  testsFor(removeOrUpdateTests("Mutable"))

  def vertical4D[T2: DiscreteValue](interval3: Interval1D[T2]): Interval[Dim[LocalDate, LocalDate, T2, Int]] =
    unbounded[LocalDate] x unbounded[LocalDate] x interval3 x unbounded[Int]

  test("Mutable: Constructors and getting data by index"):
    val empty: Data[String, Dim[Int, Int, Int, Int]] = Data()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10) x unbounded[Int]) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11) x unbounded[Int]) -> "World"
    )
    val fixture = immutable.Data(allData).toImmutable.toMutable
    fixture.getAll.toList shouldBe allData

    fixture.getByHeadIndex(dayZero).getByHeadIndex(dayZero).getByHeadIndex(0).getAt(0) shouldBe Some("Hello")

  test("Mutable: Simple toString"):
    val fixturePadData = Data
      .of[String, Dim[Int, Int, Int, Int]]("H")
    fixturePadData.set((intervalFrom(1) x intervalTo(0) x interval(1, 9) x unbounded[Int]) -> "W")
    fixturePadData.recompressAll()
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 0                          | 1 .. +∞                          |
         || H (-∞..+∞) x (-∞..0] x (-∞..+∞)                                     |
         || H (-∞..+∞) x [1..9] x (-∞..+∞)   |
         || H (-∞..+∞) x [10..+∞) x (-∞..+∞)                                    |
         |                                   | W (-∞..0] x [1..9] x (-∞..+∞)    |
         |                                   | H [1..+∞) x [1..9] x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

    val concat = fixturePadData.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.start.toStringly).append(" ")
    concat.result() shouldBe
      "H->{-∞, -∞, -∞, -∞} H->{-∞, -∞, 1, -∞} H->{-∞, -∞, 10, -∞} W->{1, -∞, 1, -∞} H->{1, 1, 1, -∞} "

    val fixturePadLabel = Data
      .of[String, Dim[Int, Int, Int, Int]]("Helloooooooooo")
    fixturePadLabel.set((intervalFrom(1) x unbounded[Int] x unbounded[Int] x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 0                                       | 1 .. +∞                                       |
         || Helloooooooooo (-∞..+∞) x (-∞..+∞) x (-∞..+∞) |
         |                                                | Wooooooorld (-∞..+∞) x (-∞..+∞) x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

  test("Mutable: Diff actions"):
    val expectedData2 = List(
      (unboundedDate x unboundedDate x interval(0, 4) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15) x unbounded[Int]) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19) x unbounded[Int]) -> "World",
      (unboundedDate x unboundedDate x interval(20, 25) x unbounded[Int]) -> "!",
      (unboundedDate x unboundedDate x intervalFrom(26) x unbounded[Int]) -> "World"
    )
    val fixture2 = Data(expectedData2)
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(4) x unbounded[Int]) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15) x unbounded[Int]) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )
    val fixture3 = Data(expectedData3)
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(4) x unbounded[Int]) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )

    val fixture = Data(expectedData4)
    fixture.set((intervalFrom(day(1)) x unboundedDate x intervalFrom(1) x unbounded[Int]) -> "remove me")
    fixture.remove(intervalFrom(day(1)) x unboundedDate x intervalFrom(1) x unbounded[Int])
    fixture.recompressAll()
    val expectedData5 = List(
      (unboundedDate x unboundedDate x intervalTo(0) x unbounded[Int]) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x interval(1, 4) x unbounded[Int]) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData5

    val fixture5 = fixture.copy

    import DiffAction.*
    val bottomInt: Domain1D[Int] = Domain1D.Bottom

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(vertical4D(intervalTo(4)) -> "Hey"),
      Delete(bottomInt x bottomInt x 0 x bottomInt),
      Update(vertical4D(intervalFrom(16)) -> "World"),
      Delete(bottomInt x bottomInt x 20 x bottomInt),
      Delete(bottomInt x bottomInt x 26 x bottomInt)
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction.Create((unbounded x unbounded x intervalTo(4) x unbounded) -> \"Hey\")",
      "DiffAction.Delete(Bottom x Bottom x Point(0) x Bottom)",
      "DiffAction.Update((unbounded x unbounded x intervalFrom(16) x unbounded) -> \"World\")",
      "DiffAction.Delete(Bottom x Bottom x Point(20) x Bottom)",
      "DiffAction.Delete(Bottom x Bottom x Point(26) x Bottom)"
    )

    val actionsFrom3To5 = fixture5.diffActionsFrom(fixture3)
    actionsFrom3To5.toList shouldBe List(
      Update((unboundedDate x unboundedDate x intervalTo(0) x unbounded[Int]) -> "Hey"),
      Create((intervalTo(day(0)) x unboundedDate x interval(1, 4) x unbounded[Int]) -> "Hey"),
      Delete(bottomInt x bottomInt x 5 x bottomInt),
      Update((intervalTo(day(0)) x unboundedDate x intervalFrom(16) x unbounded[Int]) -> "World")
    )
    fixture2.applyDiffActions(actionsFrom2To3)
    fixture2.getAll.toList shouldBe expectedData3

    fixture2.syncWith(fixture5)
    fixture2.getAll.toList shouldBe expectedData5

  test("Mutable: Updating data in intervals - bounded r1"):
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(-6) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2) x unbounded[Int]) -> "World!",
      (unboundedDate x unboundedDate x interval(-1, 1) x unbounded[Int]) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10) x unbounded[Int]) -> "Hello"
    )
    val fixture = Data(expectedData3)

    fixture.set((intervalFrom(day(0)) x unboundedDate x intervalFrom(1) x unbounded[Int]) -> "update me")
    fixture.update((intervalFrom(day(1)) x unboundedDate x intervalFrom(0) x unbounded[Int]) -> "updated me")
    fixture.recompressAll()
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
    fixture.getAll.toList shouldBe expectedData4
    fixture.domain.toList shouldBe List(
      vertical4D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical4D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x unboundedDate x interval(2, 9) x unbounded[Int] // the remaining bits of the 7th and 8th
    )

    fixture.domainComplement.toList shouldBe List(
      intervalToBefore(day(0)) x unboundedDate x interval(2, 9) x unbounded[Int]
    )
