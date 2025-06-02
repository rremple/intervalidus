package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DTest extends AnyFunSuite with Matchers with DataIn2DBaseBehaviors with MutableBaseBehaviors:

  import Interval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn2D(_), DataIn2D.of(_)))
  testsFor(
    stringLookupTests("Mutable [experimental noSearchTree]", DataIn2D(_), DataIn2D.of(_))(using
      Experimental("noSearchTree")
    )
  )
  testsFor(
    mutableBaseTests[
      Domain2D[LocalDate, Int],
      Interval2D[LocalDate, Int],
      ValidData2D[String, LocalDate, Int],
      DiffAction2D[String, LocalDate, Int],
      DataIn2D[String, LocalDate, Int]
    ](
      ds => DataIn2D(ds),
      i => unbounded[LocalDate] x i,
      (i, s) => (unbounded[LocalDate] x i) -> s,
      d =>
        d.copy(
          value = d.value + "!",
          interval = d.interval.withVerticalUpdate(_.to(d.interval.vertical.end.leftAdjacent))
        ),
      Interval2D.unbounded
    )
  )

  protected def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData2D[String, LocalDate, Int]*
  )(
    removeOrUpdateInterval: Interval2D[LocalDate, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val rectangle = interval(day(-14), day(14)) x interval(4, 7)
    val removeFixture = DataIn2D.of(rectangle -> "World")
    val updateFixture = DataIn2D.of(rectangle -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ rectangle match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the rectangle")

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
  testsFor(removeOrUpdateTests("Mutable [experimental noSearchTree]")(using Experimental("noSearchTree")))
  testsFor(removeOrUpdateTests("Mutable [experimental noBruteForceUpdate]")(using Experimental("noBruteForceUpdate")))

  def vertical2D[T: DiscreteValue](interval2: Interval1D[T]): Interval2D[LocalDate, T] =
    unbounded[LocalDate] x interval2

  test("Mutable: Constructors and getting data by index"):
    val empty: DataIn2D[String, Int, Int] = DataIn2D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = immutable.DataIn2D(allData).toImmutable.toMutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Mutable [experimental noSearchTree]: Constructors and getting data by index"):
    given Experimental = Experimental("noSearchTree")

    val allData = List(
      (intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = immutable.DataIn2D(allData).toImmutable.toMutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Mutable: Diff actions"):
    val expectedData2 = List(
      (unbounded[LocalDate] x interval(0, 4)) -> "Hello",
      (unbounded[LocalDate] x interval(5, 15)) -> "to",
      (unbounded[LocalDate] x interval(16, 19)) -> "World",
      (unbounded[LocalDate] x interval(20, 25)) -> "!",
      (unbounded[LocalDate] x intervalFrom(26)) -> "World"
    )
    val fixture2 = DataIn2D(expectedData2)
    val expectedData3 = List(
      (unbounded[LocalDate] x intervalTo(4)) -> "Hey",
      (unbounded[LocalDate] x interval(5, 15)) -> "to",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World"
    )
    val fixture3 = DataIn2D(expectedData3)
    val expectedData4 = List(
      (unbounded[LocalDate] x intervalTo(4)) -> "Hey",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World"
    )
    val fixture = DataIn2D(expectedData4)
    fixture.set((intervalFrom(day(1)) x intervalFrom(1)) -> "remove me")
    fixture.remove(intervalFrom(day(1)) x intervalFrom(1))
    fixture.recompressAll()
    val expectedData5 = List(
      (unbounded[LocalDate] x intervalTo(0)) -> "Hey",
      (intervalTo(day(0)) x interval(1, 4)) -> "Hey",
      (intervalTo(day(0)) x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData5

    val fixture5 = fixture.copy

    import DiffAction2D.*
    import Domain1D.{Bottom, Point}

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(vertical2D(intervalTo(4)) -> "Hey"),
      Delete(Domain2D[Int, Int](Bottom, 0)),
      Update(vertical2D(intervalFrom(16)) -> "World"),
      Delete(Domain2D[Int, Int](Bottom, 20)),
      Delete(Domain2D[Int, Int](Bottom, 26))
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction2D.Create((unbounded x intervalTo(4)) -> \"Hey\")",
      "DiffAction2D.Delete(Bottom x Point(0))",
      "DiffAction2D.Update((unbounded x intervalFrom(16)) -> \"World\")",
      "DiffAction2D.Delete(Bottom x Point(20))",
      "DiffAction2D.Delete(Bottom x Point(26))"
    )

    val actionsFrom3To5 = fixture5.diffActionsFrom(fixture3)
    actionsFrom3To5.toList shouldBe List(
      Update(vertical2D(intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x interval(1, 4)) -> "Hey"),
      Delete(Domain2D[Int, Int](Bottom, Point(5))),
      Update((intervalTo(day(0)) x intervalFrom(16)) -> "World")
    )
    fixture2.applyDiffActions(actionsFrom2To3)
    fixture2.getAll.toList shouldBe expectedData3

    fixture2.syncWith(fixture5)
    fixture2.getAll.toList shouldBe expectedData5

  test("Mutable: Updating data in intervals - bounded r1"):
    val expectedData3 = List(
      (unbounded[LocalDate] x intervalTo(-6)) -> "Hello",
      (unbounded[LocalDate] x interval(-5, -2)) -> "World!",
      (unbounded[LocalDate] x interval(-1, 1)) -> "Hello",
      (unbounded[LocalDate] x intervalFrom(10)) -> "Hello"
    )

    val fixture = DataIn2D(expectedData3)
    fixture.set((intervalFrom(day(0)) x intervalFrom(1)) -> "update me")
    fixture.update((intervalFrom(day(1)) x intervalFrom(0)) -> "update me")
    fixture.recompressAll()
    val expectedData4 = List(
      (unbounded[LocalDate] x intervalTo(-6)) -> "Hello",
      (unbounded[LocalDate] x interval(-5, -2)) -> "World!",
      (unbounded[LocalDate] x intervalAt(-1)) -> "Hello",
      (intervalTo(day(0)) x intervalAt(0)) -> "Hello",
      (intervalTo(day(-1)) x intervalAt(1)) -> "Hello",
      (intervalTo(day(-1)) x intervalFrom(10)) -> "Hello",
      (intervalFrom(day(0)) x intervalFrom(1)) -> "update me",
      (intervalFrom(day(1)) x intervalAt(0)) -> "update me"
    )
    fixture.getAll.toList shouldBe expectedData4
    fixture.domain.toList shouldBe List(
      vertical2D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical2D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x interval(2, 9) // the remaining bits of the 7th and 8th
    )
    fixture.domainComplement.toList shouldBe List(
      intervalToBefore(day(0)) x interval(2, 9)
    )

  test("Mutable: Simple toString"):
    val fixturePadData = DataIn2D.of[String, LocalDate, Int]("H")
    fixturePadData.set((intervalFrom(day(0)) x unbounded[Int]) -> "W")
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 2024-07-14 | 2024-07-15 .. +∞ |
         || H (-∞..+∞)       |
         |                   | W (-∞..+∞)       |
         |""".stripMargin.replaceAll("\r", "")

    val concat = fixturePadData.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.horizontal.toString).append(" ")
    concat.result() shouldBe "H->(-∞..2024-07-14] W->[2024-07-15..+∞) "

    val fixturePadLabel = DataIn2D.of[String, LocalDate, Int]("Helloooooooooo")
    fixturePadLabel.set((intervalFrom(day(0)) x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 2024-07-14        | 2024-07-15 .. +∞        |
         || Helloooooooooo (-∞..+∞) |
         |                          | Wooooooorld (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")
