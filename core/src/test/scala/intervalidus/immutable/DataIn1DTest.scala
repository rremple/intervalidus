package intervalidus.immutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DTest extends AnyFunSuite with Matchers with DataIn1DBaseBehaviors with ImmutableBaseBehaviors:

  import DiscreteInterval1D.*
  import DiscreteDomain1D.Point

  // shared
  testsFor(stringLookupTests("Immutable", DataIn1D(_), DataIn1D.of(_)))
  testsFor(
    stringLookupTests("Immutable [experimental noSearchTree]", DataIn1D(_), DataIn1D.of(_))(using
      Experimental("noSearchTree")
    )
  )
  testsFor(
    immutableBaseTests[
      DiscreteDomain1D[Int],
      DiscreteInterval1D[Int],
      ValidData1D[String, Int],
      DiffAction1D[String, Int],
      DataIn1D[String, Int]
    ](
      DataIn1D(_),
      identity,
      _ -> _,
      DiscreteInterval1D.unbounded
    )
  )

  testsFor(doubleUseCaseTests("Immutable", DataIn1D(_)))

  testsFor(removeOrUpdateTests("Immutable"))
  testsFor(removeOrUpdateTests("Immutable [experimental noSearchTree]")(using Experimental("noSearchTree")))
  testsFor(removeOrUpdateTests("Immutable [experimental bruteForceUpdate]")(using Experimental("bruteForceUpdate")))

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData1D[String, Int]*
  )(
    removeOrUpdateInterval: DiscreteInterval1D[Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val fixtureInterval = interval(-7, 7)
    val fixture = DataIn1D.of(fixtureInterval -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ fixtureInterval match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the fixture interval")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    val removeFixture = fixture.remove(removeOrUpdateInterval)
    val updateFixture = fixture.update(removeOrUpdateInterval -> updateValue)
    assertResult(removeExpected)(removeFixture.getAll.toList)
    assertResult(updateExpected)(updateFixture.getAll.toList)

  test("Immutable: Practical use case: tax brackets using zip"):
    val brackets = DataIn1D(taxBrackets)

    def taxUsingZip(income: Int): Double =
      val incomeInterval: DataIn1D[Unit, Int] = DataIn1D(Seq(interval(1, income) -> ()))
      val taxesByBracket = incomeInterval
        .zip(brackets)
        .getAll
        .map:
          case ValidData1D((_, rate), DiscreteInterval1D(Point(bottomInBracket), Point(topInBracket))) =>
            rate * (topInBracket - bottomInBracket + 1)
          case unexpected => fail(s"invalid bracket: $unexpected")
      taxesByBracket.sum

    val expectedTax = 0.1 * (23200 - 0) +
      0.12 * (94300 - 23200) +
      0.22 * (201050 - 94300) +
      0.24 * (250000 - 201050)

    taxUsingZip(250000) shouldBe expectedTax // 2320 + 8532 + 23485 + 11747

  test("Immutable: Constructors"):
    val empty: DataIn1D[String, Int] = DataIn1D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

  test("Immutable: String representations and diff actions"):
    val allData = testData("Hello" -> interval(0, 9), "World" -> intervalFrom(10))
    val f1 = mutable.DataIn1D(allData).toMutable.toImmutable
    f1.getAll.toList shouldBe allData

    val concat = f1.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.toString).append(" ")
    concat.result() shouldBe "Hello->[0..9] World->[10..+∞) "

    val expectedData2 = testData(
      "Hello" -> interval(0, 4),
      "to" -> interval(5, 15),
      "World" -> interval(16, 19),
      "!" -> interval(20, 25),
      "World" -> intervalFrom(26)
    )
    val f2 = DataIn1D(expectedData2)

    val expectedString =
      """|| 0 .. 4   | 5 .. 15  | 16 .. 19 | 20 .. 25 | 26 .. +∞ |
         || Hello    |
         |           | to       |
         |                      | World    |
         |                                 | !        |
         |                                            | World    |
         |""".stripMargin.replaceAll("\r", "")

    f2.toString shouldBe expectedString
    f2.recompressAll().toString shouldBe expectedString // does nothing in 1D

    val expectedData3 = testData("Hey" -> intervalTo(4), "to" -> interval(5, 15), "World" -> intervalFrom(16))
    val f3 = DataIn1D(expectedData3)
    val f4 = f3
      .set(intervalFrom(1) -> "remove me")
      .remove(intervalFrom(1))
    val expectedData4 = testData("Hey" -> intervalTo(0))
    f4.getAll.toList shouldBe expectedData4

    import DiffAction1D.*

    val actionsFrom2To3 = f3.diffActionsFrom(f2)
    actionsFrom2To3.toList shouldBe List(
      Create(intervalTo(4) -> "Hey"),
      Delete(0),
      Update(intervalFrom(16) -> "World"),
      Delete(20),
      Delete(26)
    )
    actionsFrom2To3.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction1D.Create(intervalTo(4) -> \"Hey\")",
      "DiffAction1D.Delete(Point(0))",
      "DiffAction1D.Update(intervalFrom(16) -> \"World\")",
      "DiffAction1D.Delete(Point(20))",
      "DiffAction1D.Delete(Point(26))"
    )

    val actionsFrom3To4 = f4.diffActionsFrom(f3)
    actionsFrom3To4.toList shouldBe List(
      Update(intervalTo(0) -> "Hey"),
      Delete(5),
      Delete(16)
    )
    val f3sync = f2.applyDiffActions(actionsFrom2To3)
    f3sync.getAll.toList shouldBe expectedData3

    val f4sync = f2.syncWith(f4)
    f4sync.getAll.toList shouldBe expectedData4

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))

    val fixture1 = DataIn1D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val fixture2 = fixture1.map(d => d.interval.endingWith(d.interval.end.successor) -> (d.value + "!"))
    val expectedData2 = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(16))
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3.flatMap(d => DataIn1D.of[String, Int](d.value).map(x => d.interval -> x.value))
    val expectedData4 = testData("Hey!!!" -> intervalTo(5), "World!!!" -> intervalFrom(16))
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.value == "Hey!!!").flatMap(d => DataIn1D.of[String, Int](d.value))
    val expectedData5 = testData("Hey!!!" -> unbounded)
    fixture5.getAll.toList shouldBe expectedData5
    fixture5.get shouldBe "Hey!!!"

  // do map again experimentally to get coverage for clear and addAll
  test("Immutable [experimental noSearchTree]: Mapping, flatmapping, etc. x"):
    given Experimental = Experimental("noSearchTree")

    val allData = testData("Hey" -> intervalTo(4), "World" -> intervalFrom(16))

    val fixture1 = DataIn1D(allData)
    val fixture2 = fixture1.map(d => d.interval.endingWith(d.interval.end.successor) -> (d.value + "!"))
    val expectedData2 = testData("Hey!" -> intervalTo(5), "World!" -> intervalFrom(16))
    fixture2.getAll.toList shouldBe expectedData2
