package intervalidus

import intervalidus.DomainLike.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Domain1D.Point
  import Interval1D.*

  def stringLookupTests[S <: DimensionalBase.In1D[String, Int]](
    prefix: String,
    dataIn1DFrom: Experimental ?=> Iterable[ValidData.In1D[String, Int]] => S,
    dataIn1DOf: Experimental ?=> String => S
  )(using Experimental, DomainValueLike[Int]): Unit = test(s"$prefix: Looking up data in intervals"):
    val domainValue = summon[DomainValueLike[Int]]
    {
      given Experimental = Experimental("requireDisjoint")

      assertThrows[IllegalArgumentException]:
        // not valid as it overlaps on [10, +∞)
        val _ = dataIn1DFrom(List(interval(0, 10) -> "Hello", unbounded -> "World"))
    }

    val empty: S = dataIn1DFrom(List.empty)
    assert(empty.isEmpty)
    empty.toString shouldBe "<nothing is valid>"
    assert((empty: Any) != ("<nothing is valid>": Any))
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)
    empty.domainComplement.toList shouldBe List(Interval.unbounded[Domain.In1D[Int]])

    dataIn1DOf("Hello").zip(dataIn1DOf("world")).get shouldBe ("Hello", "world")

    val single = dataIn1DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(Interval.unbounded[Domain.In1D[Int]])
    assert(single.domainComplement.isEmpty)

    val bracePunctuation = domainValue.bracePunctuation
    val boundedInt = intervalFrom(0) -> 1
    boundedInt.toString shouldBe s"[0$bracePunctuation+∞) -> 1"
    boundedInt.toCodeLikeString shouldBe "intervalFrom(0) -> 1"

    val bounded = intervalFrom(0) -> "Hello world"
    bounded.toString shouldBe s"[0$bracePunctuation+∞) -> Hello world"
    bounded.toCodeLikeString shouldBe "intervalFrom(0) -> \"Hello world\""
    assert(bounded.isDefinedAt(0))
    assert(!bounded.isDefinedAt(-1))
    bounded(0) shouldBe "Hello world"
    assertThrows[Exception]:
      val _ = bounded(-1)

    val fixture1: S = dataIn1DFrom(Seq(intervalFrom(0) -> "Hello world"))
    fixture1.getOption shouldBe None
    assert(fixture1.isDefinedAt(0))
    fixture1(0) shouldBe "Hello world"
    assert(!fixture1.isDefinedAt(-1))
    assertThrows[Exception]:
      val _ = fixture1(-1)

    val allData2 = List(interval(0, 10) -> "Hello", intervalFromAfter(10) -> "World")
    val fixture2 = dataIn1DFrom(allData2)
    fixture2.domain.toList shouldBe List(Interval.in1D(intervalFrom(0)))
    fixture2.domainComplement.toList shouldBe List(Interval.in1D(intervalToBefore(0)))
    fixture2.values should contain theSameElementsAs List("Hello", "World")
    fixture2.getAt(5) shouldBe Some("Hello")
    fixture2.getAt(15) shouldBe Some("World")
    fixture2.getAt(-1) shouldBe None
    assert(fixture2.intersects(interval(5, 15)))
    fixture2.getIntersecting(interval(5, 15)) should contain theSameElementsAs allData2

    dataIn1DFrom(List(intervalTo(10) -> "Hello"))
      .zip(dataIn1DFrom(List(intervalFrom(5) -> "world")))
      .getAll
      .toList shouldBe List(interval(5, 10) -> ("Hello", "world"))

    val allData3a = List(interval(0, 9) -> "Hello", interval(12, 20) -> "World")
    val allData3b = List(interval(-4, -2) -> "Goodbye", interval(6, 14) -> "Cruel", interval(16, 24) -> "World")

    val fixture3 = dataIn1DFrom(allData3a).zip(dataIn1DFrom(allData3b))
    val expected3 = List(
      interval(6, 9) -> ("Hello", "Cruel"),
      interval(12, 14) -> ("World", "Cruel"),
      interval(16, 20) -> ("World", "World")
    )
    fixture3.getAll.toList shouldBe expected3

    val fixture4 = dataIn1DFrom(allData3a).zipAll(dataIn1DFrom(allData3b), "<", ">")
    val expected4 = List(
      interval(-4, -2) -> ("<", "Goodbye"),
      intervalFrom(0).toBefore(6) -> ("Hello", ">"),
      interval(6, 9) -> ("Hello", "Cruel"),
      intervalFromAfter(9).toBefore(12) -> ("<", "Cruel"),
      interval(12, 14) -> ("World", "Cruel"),
      intervalFromAfter(14).toBefore(16) -> ("World", ">"),
      interval(16, 20) -> ("World", "World"),
      intervalFromAfter(20).to(24) -> ("<", "World")
    )
    fixture4.getAll.toList shouldBe expected4

  // https://www.fidelity.com/learning-center/personal-finance/tax-brackets
  def taxBrackets(using DomainValueLike[Int]): Seq[ValidData.In1D[Double, Int]] = List( // value is tax rate (a double)
    interval(1, 23200) -> 0.1,
    interval(23201, 94300) -> 0.12,
    interval(94301, 201050) -> 0.22,
    interval(201051, 383900) -> 0.24,
    interval(383901, 487450) -> 0.32,
    interval(487451, 731200) -> 0.35,
    intervalFrom(731201) -> 0.37
  )

  def doubleUseCaseTests[S <: DimensionalBase.In1D[Double, Int]](
    prefix: String,
    dataIn1DFrom: Iterable[ValidData.In1D[Double, Int]] => S
  )(using DomainValueLike[Int]): Unit = test(s"$prefix: Practical use case: tax brackets"):
    val fixture: S = dataIn1DFrom(taxBrackets)

    def taxUsingFold(income: Int): Double = fixture.foldLeft(0.0): (priorTax, bracket) =>
      bracket.interval match
        case Interval(Point(start: Int) *: EmptyTuple, _) if start > income => // bracket above income
          priorTax // no tax in this bracket
        case Interval(Point(start: Int) *: EmptyTuple, _) if bracket.interval contains income => // income in bracket
          priorTax + bracket.value * (income - start + 1) // add calculation based on income
        case Interval(Point(start: Int) *: EmptyTuple, Point(end: Int) *: EmptyTuple) => // income above bracket
          priorTax + bracket.value * (end - start + 1) // add full tax in this bracket
        case unexpected => fail(s"invalid bracket: $unexpected")

    val expectedTax = 0.1 * (23200 - 0) +
      0.12 * (94300 - 23200) +
      0.22 * (201050 - 94300) +
      0.24 * (250000 - 201050)

    taxUsingFold(250000) shouldBe expectedTax // 2320 + 8532 + 23485 + 11747

  /*
   * One-dimensional removals and updates can have 3 cases.:
   *  (1) simple
   *  (2) partial
   *  (3) split
   */
  protected def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData.In1D[String, Int]*
  )(
    removeOrUpdateInterval: Interval1D[Int],
    updateValue: String = "update"
  )(using Experimental, DomainValueLike[Int]): Assertion

  def removeOrUpdateTests(prefix: String)(using Experimental, DomainValueLike[Int]): Unit =
    test(s"$prefix: All remove/update by interval - (1) simple"):
      // same size
      assertRemoveOrUpdateResult(
      )(interval(-7, 7))
      // larger
      assertRemoveOrUpdateResult(
      )(interval(-8, 8))

    test(s"$prefix: All remove/update by interval - (2) partial"):
      // on left
      assertRemoveOrUpdateResult(
        intervalFromAfter(0).to(7) -> "World"
      )(intervalTo(0))

      // on right
      assertRemoveOrUpdateResult(
        intervalFrom(-7).toBefore(0) -> "World"
      )(intervalFrom(0))

    test(s"$prefix: All remove/update by interval - (3) split"):
      assertRemoveOrUpdateResult(
        intervalFrom(-7).toBefore(0) -> "World",
        intervalFromAfter(0).to(7) -> "World"
      )(intervalAt(0))
