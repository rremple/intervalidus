package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DTest extends AnyFunSuite with Matchers with DataIn1DBaseBehaviors with ImmutableBaseBehaviors:

  import Interval1D.*
  import Domain1D.Point

  // shared
  testsFor(stringLookupTests("Immutable", Data(_), Data.of(_)))

  def usingBuilder(data: Iterable[ValidData[String, Dim[Int]]]): Data[String, Dim[Int]] =
    val builder = Data.newBuilder[String, Dim[Int]]
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  def usingSetMany(data: Iterable[ValidData[String, Dim[Int]]]): Data[String, Dim[Int]] =
    Data[String, Dim[Int]]() ++ data

  testsFor(immutableBaseTests[Dim[Int], Data[String, Dim[Int]]](Data(_), identity))
  testsFor(immutableBaseTests[Dim[Int], Data[String, Dim[Int]]](usingBuilder, identity, "Immutable (builder)"))
  testsFor(immutableBaseTests[Dim[Int], Data[String, Dim[Int]]](usingSetMany, identity, "Immutable (setMany)"))

  testsFor(immutableCompressionTests[Dim[Int], Data[String, Dim[Int]]](Data(_), identity))
  testsFor(immutableCompressionTests[Dim[Int], Data[String, Dim[Int]]](usingBuilder, identity, "Immutable (builder)"))

  testsFor(doubleUseCaseTests("Immutable", Data(_)))

  testsFor(removeOrUpdateTests("Immutable"))

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData[String, Dim[Int]]*
  )(
    removeOrUpdateInterval: Interval1D[Int],
    updateValue: String = "update"
  )(using Experimental, DomainValueLike[Int]): Assertion =
    val fixtureInterval = interval(-7, 7)
    val fixture = Data.of(fixtureInterval -> "World")
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
    val brackets = Data(taxBrackets)

    def taxUsingZip(income: Int): Double =
      val incomeInterval: Data[Unit, Dim[Int]] = Data(Seq(interval(1, income) -> ()))
      val taxesByBracket = incomeInterval
        .zip(brackets)
        .getAll
        .map:
          case ValidData(
                (_, rate),
                Interval(Point(bottomInBracket) *: EmptyTuple, Point(topInBracket) *: EmptyTuple)
              ) =>
            rate * (topInBracket - bottomInBracket + 1)
          case unexpected => fail(s"invalid bracket: $unexpected")
      taxesByBracket.sum

    val expectedTax = 0.1 * (23200 - 0) +
      0.12 * (94300 - 23200) +
      0.22 * (201050 - 94300) +
      0.24 * (250000 - 201050)

    taxUsingZip(250000) shouldBe expectedTax // 2320 + 8532 + 23485 + 11747

  test("Immutable: Constructors"):
    val empty: Data[String, Dim[Int]] = Data()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

  test("Immutable: String representations and diff actions"):
    val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
    val f1 = mutable.Data(allData).toMutable.toImmutable
    f1.getAll.toList shouldBe allData
    f1.getAt(0) shouldBe Some("Hello")

    // Appropriately fails in one dimension because Tuple.Tail[Domain.In1D[Int]] is empty.
    """f1.getByHeadDimension(0)""" shouldNot typeCheck

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
    val f2 = Data(expectedData2)

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

    val expectedData3 = List(intervalTo(4) -> "Hey", interval(5, 15) -> "to", intervalFrom(16) -> "World")
    val f3 = Data(expectedData3)
    val f4 = f3
      .set(intervalFrom(1) -> "remove me")
      .remove(intervalFrom(1))
    val expectedData4 = List(intervalTo(0) -> "Hey")
    f4.getAll.toList shouldBe expectedData4

    import DiffAction.*

    val actionsFrom2To3 = f3.diffActionsFrom(f2)
    actionsFrom2To3 shouldBe Iterable(
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

    val actionsFrom3To4 = f4.diffActionsFrom(f3)
    actionsFrom3To4 shouldBe Iterable(
      Update(intervalTo(0) -> "Hey"),
      Delete(Point(5)),
      Delete(Point(16))
    )
    val f3sync = f2.applyDiffActions(actionsFrom2To3)
    f3sync.getAll.toList shouldBe expectedData3

    val f4sync = f2.syncWith(f4)
    f4sync.getAll.toList shouldBe expectedData4

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = List(intervalTo(4) -> "Hey", intervalFrom(16) -> "World")

    val fixture1 = Data(allData)
    fixture1.copy shouldBe fixture1

    val fixture2 = fixture1.map(d => d.interval.to(d.interval.end.rightAdjacent) -> (d.value + "!"))
    val expectedData2 = List(intervalTo(5) -> "Hey!", intervalFrom(16) -> "World!")
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3.flatMap(d => Data.of[String, Dim[Int]](d.value).map(x => d.interval -> x.value))
    val expectedData4 = List(intervalTo(5) -> "Hey!!!", intervalFrom(16) -> "World!!!")
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.value == "Hey!!!").flatMap(d => Data.of[String, Dim[Int]](d.value))
    val expectedData5 = List(unbounded[Int] -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    fixture5.get shouldBe "Hey!!!"
