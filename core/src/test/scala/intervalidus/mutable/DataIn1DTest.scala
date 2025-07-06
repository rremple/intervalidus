package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DTest extends AnyFunSuite with Matchers with DataIn1DBaseBehaviors with MutableBaseBehaviors:

  import Interval1D.*
  import Domain1D.Point

  // shared
  testsFor(stringLookupTests("Mutable", Data(_), Data.of(_)))

  testsFor(
    mutableBaseTests[Dim[Int], Data[String, Dim[Int]]](
      Data(_),
      identity,
      d =>
        d.copy(
          value = d.value + "!",
          interval = d.interval.to(d.interval.end.leftAdjacent)
        )
    )
  )

  testsFor(doubleUseCaseTests("Mutable", Data(_)))

  testsFor(removeOrUpdateTests("Mutable"))

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData[String, Dim[Int]]*
  )(
    removeOrUpdateInterval: Interval1D[Int],
    updateValue: String = "update"
  )(using Experimental, DomainValueLike[Int]): Assertion =
    val fixtureInterval = interval(-7, 7)
    val removeFixture = Data.of(fixtureInterval -> "World")
    val updateFixture = removeFixture.copy
    val expectedUpdateInterval = removeOrUpdateInterval ∩ fixtureInterval match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the fixture interval")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    removeFixture.remove(removeOrUpdateInterval)
    updateFixture.update(removeOrUpdateInterval -> updateValue)
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

  test("Mutable: Constructors"):
    val empty: Data[String, Dim[Int]] = Data()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

  test("Mutable: String representations and diff actions"):
    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val f1 = immutable.Data(allData).toImmutable.toMutable
    f1.getAll.toList shouldBe allData
    f1.getAt(0) shouldBe Some("Hello")

    // Appropriately fails in one dimension because the compiler cannot prove that
    // Domain1D[Int] *: EmptyTuple =:=
    // Domain1D[Int] *: Domain.NonEmptyTail[Domain1D[Int] *: EmptyTuple].
    """f1.getByHeadIndex(0)""" shouldNot typeCheck

    val concat = f1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.toString).append(" ")
    concat.result() shouldBe "Hello->[0..9] World->[10..+∞) "

    val expectedData2 = List(
      interval(0, 4) -> "Hello",
      interval(5, 15) -> "to",
      interval(16, 19) -> "World",
      interval(20, 25) -> "!",
      intervalFrom(26) -> "World"
    )
    val fixture = Data(expectedData2)

    val expectedString =
      """|| 0 .. 4   | 5 .. 15  | 16 .. 19 | 20 .. 25 | 26 .. +∞ |
         || Hello    |
         |           | to       |
         |                      | World    |
         |                                 | !        |
         |                                            | World    |
         |""".stripMargin.replaceAll("\r", "")

    fixture.toString shouldBe expectedString
    fixture.recompressAll() // does nothing in 1D
    fixture.toString shouldBe expectedString

    val fixture2 = fixture.copy
    val expectedData3 = List(intervalTo(4) -> "Hey", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    val fixture3 = Data(expectedData3)

    fixture.syncWith(fixture3)
    fixture.set(intervalFrom(1) -> "remove me")
    fixture.remove(intervalFrom(1))
    val expectedData4 = List(intervalTo(0) -> "Hey")
    fixture.getAll.toList shouldBe expectedData4

    val fixture4 = fixture.copy

    import DiffAction.*

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(intervalTo(4) -> "Hey"),
      Delete(Point(0)),
      Update(intervalFrom(16) -> "World"),
      Delete(Point(20)),
      Delete(Point(26))
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction.Create(intervalTo(4) -> \"Hey\")",
      "DiffAction.Delete(Point(0))",
      "DiffAction.Update(intervalFrom(16) -> \"World\")",
      "DiffAction.Delete(Point(20))",
      "DiffAction.Delete(Point(26))"
    )

    val actionsFrom3To4 = fixture4.diffActionsFrom(fixture3)
    actionsFrom3To4.toList shouldBe List(
      Update(intervalTo(0) -> "Hey"),
      Delete(Point(5)),
      Delete(Point(16))
    )
    fixture2.applyDiffActions(actionsFrom2To3)
    fixture2.getAll.toList shouldBe expectedData3

    fixture2.syncWith(fixture4)
    fixture2.getAll.toList shouldBe expectedData4
