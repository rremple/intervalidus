package intervalidus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import DataIn1DBase.ValidData1D
  import DiscreteDomain1D.Point
  import DiscreteInterval1D.*

  protected def testData[T](
    values: (T, DiscreteInterval1D[Int])*
  ): List[ValidData1D[T, Int]] = values.map(ValidData1D(_, _)).toList

  def stringLookupTests[S <: DataIn1DBase[String, Int]](
    dataIn1DFrom: Iterable[ValidData1D[String, Int]] => S,
    dataIn1DOf: String => S
  ): Any = test("Looking up data in intervals"):
    assertThrows[IllegalArgumentException]:
      // not valid as it overlaps on [10, +∞)
      val badFixture = dataIn1DFrom(testData("Hello" -> interval(0, 10), "World" -> unbounded))

    val empty: S = dataIn1DFrom(List.empty)
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    dataIn1DOf("Hello").zip(dataIn1DOf("world")).get shouldBe ("Hello", "world")

    val single = dataIn1DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(unbounded[Int])

    val bounded = ValidData1D("Hello world", intervalFrom(0))
    bounded.toString shouldBe "ValidData1D(Hello world, [0..+∞))"
    assert(bounded.isDefinedAt(0))
    assert(!bounded.isDefinedAt(-1))
    bounded(0) shouldBe "Hello world"
    assertThrows[Exception]:
      val bad = bounded(-1)

    val fixture1: S = dataIn1DFrom(Seq(ValidData1D("Hello world", intervalFrom(0))))
    fixture1.getOption shouldBe None
    assert(fixture1.isDefinedAt(0))
    fixture1(0) shouldBe "Hello world"
    assert(!fixture1.isDefinedAt(-1))
    assertThrows[Exception]:
      val missingData = fixture1(-1)

    val allData2 = testData("Hello" -> interval(0, 10), "World" -> intervalFrom(11))
    val fixture2 = dataIn1DFrom(allData2)
    fixture2.domain.toList shouldBe List(intervalFrom(0))
    fixture2.getAt(5) shouldBe Some("Hello")
    fixture2.getAt(15) shouldBe Some("World")
    fixture2.getAt(-1) shouldBe None
    assert(fixture2.intersects(interval(5, 15)))
    fixture2.getIntersecting(interval(5, 15)) shouldBe allData2

    dataIn1DFrom(testData("Hello" -> intervalTo(10)))
      .zip(dataIn1DFrom(testData("world" -> intervalFrom(5))))
      .getAll
      .toList shouldBe testData(("Hello", "world") -> interval(5, 10))

    val allData3a = testData("Hello" -> interval(0, 9), "World" -> interval(12, 20))
    val allData3b = testData("Goodbye" -> interval(-4, -2), "Cruel" -> interval(6, 14), "World" -> interval(16, 24))

    val fixture3 = dataIn1DFrom(allData3a).zip(dataIn1DFrom(allData3b))
    val expected3 = testData(
      ("Hello", "Cruel") -> interval(6, 9),
      ("World", "Cruel") -> interval(12, 14),
      ("World", "World") -> interval(16, 20)
    )
    fixture3.getAll.toList shouldBe expected3

    val fixture4 = dataIn1DFrom(allData3a).zipAll(dataIn1DFrom(allData3b), "<", ">")
    val expected4 = testData(
      ("<", "Goodbye") -> interval(-4, -2),
      ("Hello", ">") -> interval(0, 5),
      ("Hello", "Cruel") -> interval(6, 9),
      ("<", "Cruel") -> interval(10, 11),
      ("World", "Cruel") -> interval(12, 14),
      ("World", ">") -> interval(15, 15),
      ("World", "World") -> interval(16, 20),
      ("<", "World") -> interval(21, 24)
    )
    fixture4.getAll.toList shouldBe expected4

  // https://www.fidelity.com/learning-center/personal-finance/tax-brackets
  val taxBrackets = testData( // value is tax rate (a double)
    0.1 -> interval(1, 23200),
    0.12 -> interval(23201, 94300),
    0.22 -> interval(94301, 201050),
    0.24 -> interval(201051, 383900),
    0.32 -> interval(383901, 487450),
    0.35 -> interval(487451, 731200),
    0.37 -> intervalFrom(731201)
  )

  def doubleUseCaseTests[S <: DataIn1DBase[Double, Int]](
    dataIn1DFrom: Iterable[ValidData1D[Double, Int]] => S
  ): Any = test("Practical use case: tax brackets"):
    val fixture: S = dataIn1DFrom(taxBrackets)

    def taxUsingFold(income: Int): Double = fixture.foldLeft(0.0): (priorTax, bracket) =>
      bracket.interval match
        case DiscreteInterval1D(Point(start), _) if start > income => // bracket above income
          priorTax // no tax in this bracket
        case DiscreteInterval1D(Point(start), _) if bracket.interval contains income => // income in bracket
          priorTax + bracket.value * (income - start + 1) // add calculation based on income
        case DiscreteInterval1D(Point(start), Point(end)) => // income above bracket
          priorTax + bracket.value * (end - start + 1) // add full tax in this bracket
        case unexpected => fail(s"invalid bracket: $unexpected")

    val expectedTax = 0.1 * (23200 - 0) +
      0.12 * (94300 - 23200) +
      0.22 * (201050 - 94300) +
      0.24 * (250000 - 201050)

    taxUsingFold(250000) shouldBe expectedTax // 2320 + 8532 + 23485 + 11747
