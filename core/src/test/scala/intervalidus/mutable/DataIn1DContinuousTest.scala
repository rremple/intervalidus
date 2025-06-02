package intervalidus.mutable

import intervalidus.*
import intervalidus.ContinuousValue.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DContinuousTest extends AnyFunSuite with Matchers with DataIn1DBaseBehaviors with MutableBaseBehaviors:

  import Interval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn1D(_), DataIn1D.of(_)))
  testsFor(
    stringLookupTests("Mutable [experimental noSearchTree]", DataIn1D(_), DataIn1D.of(_))(using
      Experimental("noSearchTree")
    )
  )
  testsFor(
    mutableBaseTests[
      Domain1D[Int],
      Interval1D[Int],
      ValidData1D[String, Int],
      DiffAction1D[String, Int],
      DataIn1D[String, Int]
    ](
      DataIn1D(_),
      identity,
      _ -> _,
      d =>
        d.copy(
          value = d.value + "!",
          interval = d.interval.to(d.interval.end.leftAdjacent)
        ),
      Interval1D.unbounded
    )
  )

  testsFor(doubleUseCaseTests("Mutable", DataIn1D(_)))

  testsFor(removeOrUpdateTests("Mutable"))
  testsFor(removeOrUpdateTests("Mutable [experimental noSearchTree]")(using Experimental("noSearchTree")))
  testsFor(removeOrUpdateTests("Mutable [experimental noBruteForceUpdate]")(using Experimental("noBruteForceUpdate")))

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData1D[String, Int]*
  )(
    removeOrUpdateInterval: Interval1D[Int],
    updateValue: String = "update"
  )(using Experimental, DomainValueLike[Int]): Assertion =
    val fixtureInterval = interval(-7, 7)
    val removeFixture = DataIn1D.of(fixtureInterval -> "World")
    val updateFixture = removeFixture.copy
    val expectedUpdateInterval = removeOrUpdateInterval ∩ fixtureInterval match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the fixture interval")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    removeFixture.remove(removeOrUpdateInterval)
    updateFixture.update(removeOrUpdateInterval -> updateValue)
    assertResult(removeExpected)(removeFixture.getAll.toList)
    assertResult(updateExpected)(updateFixture.getAll.toList)

  test("Mutable: Constructors"):
    val empty: DataIn1D[String, Int] = DataIn1D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

  test("Mutable: String representations and diff actions"):
    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val f1 = immutable.DataIn1D(allData).toImmutable.toMutable
    f1.getAll.toList shouldBe allData

    val concat = f1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.toString).append(" ")
    concat.result() shouldBe "Hello->[0, 9] World->[10, +∞) "

    val expectedData2 = List(
      intervalFrom(0).toBefore(5) -> "Hello",
      intervalFrom(5).toBefore(16) -> "to",
      intervalFrom(16).toBefore(20) -> "World",
      intervalFrom(20).toBefore(26) -> "!",
      intervalFrom(26) -> "World"
    )
    val fixture = DataIn1D(expectedData2)

    val expectedString =
      """|[ 0, 5 )   [ 5, 16 )  [ 16, 20 ) [ 20, 26 ) [ 26, +∞ ) |
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
    val expectedData3 = List(
      intervalToBefore(5) -> "Hey",
      intervalFrom(5).toBefore(16) -> "to",
      intervalFrom(16) -> "World"
    )
    val fixture3 = DataIn1D(expectedData3)

    fixture.syncWith(fixture3)
    fixture.set(intervalFromAfter(0) -> "remove me")
    fixture.remove(intervalFromAfter(0))
    val expectedData4 = List(intervalTo(0) -> "Hey")
    fixture.getAll.toList shouldBe expectedData4

    val fixture4 = fixture.copy

    import DiffAction1D.*

    val actionsFrom2To3 = fixture3.diffActionsFrom(fixture2)
    actionsFrom2To3.toList shouldBe List(
      Create(intervalToBefore(5) -> "Hey"),
      Delete(0),
      Update(intervalFrom(16) -> "World"),
      Delete(20),
      Delete(26)
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction1D.Create(intervalToBefore(5) -> \"Hey\")",
      "DiffAction1D.Delete(Point(0))",
      "DiffAction1D.Update(intervalFrom(16) -> \"World\")",
      "DiffAction1D.Delete(Point(20))",
      "DiffAction1D.Delete(Point(26))"
    )

    val actionsFrom3To4 = fixture4.diffActionsFrom(fixture3)
    actionsFrom3To4.toList shouldBe List(
      Update(intervalTo(0) -> "Hey"),
      Delete(5),
      Delete(16)
    )
    fixture2.applyDiffActions(actionsFrom2To3)
    fixture2.getAll.toList shouldBe expectedData3

    fixture2.syncWith(fixture4)
    fixture2.getAll.toList shouldBe expectedData4
