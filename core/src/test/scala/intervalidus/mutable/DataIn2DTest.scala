package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In2D as Dim
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DTest extends AnyFunSuite with Matchers with DataIn2DBaseBehaviors with MutableBaseBehaviors:

  import Interval1D.*
  import Interval.Patterns.*

  // shared
  testsFor(stringLookupTests("Mutable", Data(_), Data.of(_)))

  def usingBuilder(data: Iterable[ValidData[String, Dim[LocalDate, Int]]]): Data[String, Dim[LocalDate, Int]] =
    data.foldLeft(Data.newBuilder[String, Dim[LocalDate, Int]])(_.addOne(_)).result()

  def usingSetMany(data: Iterable[ValidData[String, Dim[LocalDate, Int]]]): Data[String, Dim[LocalDate, Int]] =
    val newStructure = Data[String, Dim[LocalDate, Int]]()
    newStructure ++ data
    newStructure

  def asVertical(interval1D: Interval1D[Int]): Interval[Dim[LocalDate, Int]] =
    unbounded[LocalDate] x interval1D

  def mapf(d: ValidData[String, Dim[LocalDate, Int]]): ValidData[String, Dim[LocalDate, Int]] =
    d.copy(
      value = d.value + "!",
      interval = d.interval.to(d.interval.end.leftAdjacent)
    )

  testsFor(mutableBaseTests[Dim[LocalDate, Int], Data[String, Dim[LocalDate, Int]]](Data(_), asVertical, mapf))
  testsFor(
    mutableBaseTests[Dim[LocalDate, Int], Data[String, Dim[LocalDate, Int]]](
      usingBuilder,
      asVertical,
      mapf,
      "Immutable (builder)"
    )
  )
  testsFor(
    mutableBaseTests[Dim[LocalDate, Int], Data[String, Dim[LocalDate, Int]]](
      usingSetMany,
      asVertical,
      mapf,
      "Immutable (setMany)"
    )
  )

  testsFor(mutableCompressionTests[Dim[LocalDate, Int], Data[String, Dim[LocalDate, Int]]](Data(_), asVertical))
  testsFor(
    mutableCompressionTests[Dim[LocalDate, Int], Data[String, Dim[LocalDate, Int]]](
      usingBuilder,
      asVertical,
      "Immutable (builder)"
    )
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData[String, Dim[LocalDate, Int]]*
  )(
    removeOrUpdateInterval: Interval[Dim[LocalDate, Int]],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val rectangle = interval(day(-14), day(14)) x interval(4, 7)
    val removeFixture = Data.of(rectangle -> "World")
    val updateFixture = Data.of(rectangle -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ rectangle match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the rectangle")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    removeFixture.remove(removeOrUpdateInterval)
    // if needed: removeFixture.recompressAll()
    updateFixture.update(removeOrUpdateInterval -> updateValue)
    // if needed: updateFixture.recompressAll()
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

  def vertical2D[T: DiscreteValue](interval2: Interval1D[T]): Interval[Dim[LocalDate, T]] =
    unbounded[LocalDate] x interval2

  test("Mutable: Constructors and getting data by index"):
    val empty: Data[String, Dim[Int, Int]] = Data()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = immutable.Data(allData).toImmutable.toMutable
    fixture.getAll.toList shouldBe allData

    fixture.getByHeadDimension(dayZero).getAt(0) shouldBe Some("Hello")

  test("Mutable: Diff actions"):
    val expectedData2 = List(
      (unbounded[LocalDate] x interval(0, 4)) -> "Hello",
      (unbounded[LocalDate] x interval(5, 15)) -> "to",
      (unbounded[LocalDate] x interval(16, 19)) -> "World",
      (unbounded[LocalDate] x interval(20, 25)) -> "!",
      (unbounded[LocalDate] x intervalFrom(26)) -> "World"
    )
    val fixture2 = Data(expectedData2)
    val expectedData3 = List(
      (unbounded[LocalDate] x intervalTo(4)) -> "Hey",
      (unbounded[LocalDate] x interval(5, 15)) -> "to",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World"
    )
    val fixture3 = Data(expectedData3)
    val expectedData4 = List(
      (unbounded[LocalDate] x intervalTo(4)) -> "Hey",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World"
    )
    val fixture = Data(expectedData4)
    fixture.set((intervalFrom(day(1)) x intervalFrom(1)) -> "remove me")
    fixture.remove(intervalFrom(day(1)) x intervalFrom(1))
    // if needed: fixture.recompressAll()
    val expectedData5 = List(
      (unbounded[LocalDate] x intervalTo(0)) -> "Hey",
      (intervalTo(day(0)) x interval(1, 4)) -> "Hey",
      (intervalTo(day(0)) x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData5

    val fixture5 = fixture.copy

    import DiffAction.*
    val bottomInt: Domain1D[Int] = Domain1D.Bottom

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(vertical2D(intervalTo(4)) -> "Hey"),
      Delete(bottomInt x 0),
      Update(vertical2D(intervalFrom(16)) -> "World"),
      Delete(bottomInt x 20),
      Delete(bottomInt x 26)
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction.Create((unbounded x intervalTo(4)) -> \"Hey\")",
      "DiffAction.Delete(Bottom x Point(0))",
      "DiffAction.Update((unbounded x intervalFrom(16)) -> \"World\")",
      "DiffAction.Delete(Bottom x Point(20))",
      "DiffAction.Delete(Bottom x Point(26))"
    )

    val actionsFrom3To5 = fixture5.diffActionsFrom(fixture3)
    actionsFrom3To5.toList shouldBe List(
      Update(vertical2D(intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x interval(1, 4)) -> "Hey"),
      Delete(bottomInt x 5),
      Update((intervalTo(day(0)) x intervalFrom(16)) -> "World")
    )
    fixture2.applyDiffActions(actionsFrom2To3)
    fixture2.getAll.toList shouldBe expectedData3

    fixture2.syncWith(fixture5)
    fixture2.getAll.toList shouldBe expectedData5

  test("Mutable: Mapping extras"):
    val allData = List(
      (unbounded[LocalDate] x intervalTo(4)) -> "Hey",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World"
    )

    extension (d: Data[String, Dim[LocalDate, Int]])
      def flipEverything: Data[String, Dim[Int, LocalDate]] =
        val flippedValidData = d.getAll.map:
          case (horizontal x_: vertical) ->: v => (vertical x horizontal) -> v.reverse
        Data(flippedValidData)

    val fixture1 = Data(allData)

    fixture1.flipEverything.getAll.toList shouldBe List(
      (intervalTo(4) x unbounded[LocalDate]) -> "yeH",
      (intervalFrom(16) x unbounded[LocalDate]) -> "dlroW"
    )

    val fixture2a = fixture1.copy
    fixture2a.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.to(d.interval.end.rightAdjacent)
      )
    )
    val fixture2b = fixture1.copy
    fixture2b.mapValues(_ + "!")
    fixture2b.mapIntervals(i => i.to(i.end.rightAdjacent))
    val expectedData2 = List(
      (unbounded[LocalDate] x intervalTo(5)) -> "Hey!",
      (unbounded[LocalDate] x intervalFrom(16)) -> "World!"
    )
    fixture2a.getAll.toList shouldBe expectedData2
    fixture2b.getAll.toList shouldBe expectedData2

  test("Mutable: Updating data in intervals - bounded r1"):
    val expectedData3 = List(
      (unbounded[LocalDate] x intervalTo(-6)) -> "Hello",
      (unbounded[LocalDate] x interval(-5, -2)) -> "World!",
      (unbounded[LocalDate] x interval(-1, 1)) -> "Hello",
      (unbounded[LocalDate] x intervalFrom(10)) -> "Hello"
    )

    val fixture = Data(expectedData3)
    fixture.set((intervalFrom(day(0)) x intervalFrom(1)) -> "update me")
    fixture.update((intervalFrom(day(1)) x intervalFrom(0)) -> "update me")
    // if needed: fixture.recompressAll()
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
    Interval.compress(fixture.domain ++ fixture.domainComplement).toList shouldBe List(
      Interval.unbounded[Dim[LocalDate, Int]]
    )

  test("Mutable: Simple toString"):
    val fixturePadData = Data.of[String, Dim[LocalDate, Int]]("H")
    fixturePadData.set((intervalFrom(day(0)) x unbounded[Int]) -> "W")
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 2024-07-14 | 2024-07-15 .. +∞ |
         || H (-∞..+∞)       |
         |                   | W (-∞..+∞)       |
         |""".stripMargin.replaceAll("\r", "")

    val concat = fixturePadData.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.headInterval1D[LocalDate].toString).append(" ")
    concat.result() shouldBe "H->(-∞..2024-07-14] W->[2024-07-15..+∞) "

    val fixturePadLabel = Data.of[String, Dim[LocalDate, Int]]("Helloooooooooo")
    fixturePadLabel.set((intervalFrom(day(0)) x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 2024-07-14        | 2024-07-15 .. +∞        |
         || Helloooooooooo (-∞..+∞) |
         |                          | Wooooooorld (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")
