package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.Interval1D.*
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import intervalidus.DimensionalVersionedBase.Versioned
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DVersionedBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  val dayZero: LocalDate = LocalDate.of(2024, 6, 30)

  def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  def validString(s: String, validTime: Interval[Dim[LocalDate]]): ValidData[String, Dim[LocalDate]] =
    validTime -> s

  def horizontal[H: DomainValueLike](interval: Interval[Versioned[Domain.In1D[H]]]): Interval1D[H] = interval(1)

  protected def testDataIn2D[T](
    current: Domain1D[Int],
    values: List[ValidData[T, Dim[Int]]]
  ): List[ValidData[T, Versioned[Dim[Int]]]] =
    values.map(d => (d.interval withHead intervalFrom(current)) -> d.value)

  def stringLookupTests[S <: DimensionalVersionedBase[String, Dim[Int]]](
    prefix: String,
    dataIn1DVersionedFrom1D: Experimental ?=> Iterable[ValidData[String, Dim[Int]]] => S,
    dataIn1DVersionedFrom2D: Experimental ?=> Iterable[ValidData[String, Versioned[Dim[Int]]]] => S,
    dataIn1DVersionedOf: Experimental ?=> String => S
  )(using Experimental): Unit =
    test(s"$prefix: General setup"):
      {
        given Experimental = Experimental("requireDisjoint")

        assertThrows[IllegalArgumentException]:
          // not valid as it overlaps on [10, +∞)
          val _ =
            dataIn1DVersionedFrom2D(testDataIn2D(0, List(interval(0, 10) -> "Hello", unbounded[Int] -> "World")))
      }

      val empty: S = dataIn1DVersionedFrom1D(Seq.empty)
      assert(empty.getAll.isEmpty)
      assert(empty.domain.isEmpty)
      assert((empty: Any) != ("<nothing is valid>": Any))

      val single = dataIn1DVersionedOf("Hello world")
      single.get shouldBe "Hello world"
      single.getOption shouldBe Some("Hello world")
      single.domain.toList shouldBe List(Interval.unbounded[Dim[Int]])
      assert(single.domainComplement.isEmpty)

      val fixture1: S = dataIn1DVersionedFrom1D(List(intervalFrom(0) -> "Hello world"))
      fixture1.getOption shouldBe None
      assert(fixture1.isDefinedAt(0, 0))
      assert(fixture1.isValidAt(0))
      fixture1(0, 0) shouldBe "Hello world"
      assert(!fixture1.isDefinedAt(-1, 0))
      assert(!fixture1.isValidAt(-1))
      assertThrows[Exception]:
        val _ = fixture1(-1, 0)

      val allData2 = List(interval(0, 10) -> "Hello", intervalFrom(11) -> "World")
      val fixture2 = dataIn1DVersionedFrom2D(testDataIn2D(0, allData2))
      fixture2.domain.toList shouldBe List(intervalFrom(0).tupled)
      fixture2.domainComplement.toList shouldBe List(intervalToBefore(0).tupled)
      fixture2.values should contain theSameElementsAs List("Hello", "World")
      fixture2.allIntervals should contain theSameElementsAs List(interval(0, 10), intervalFrom(11)).map(_.tupled)
      fixture2.getAt(5) shouldBe Some("Hello")
      fixture2.getDataAt(15) shouldBe Some(intervalFrom(11) -> "World")
      fixture2.getAt(-1) shouldBe None
      assert(fixture2.intersects(interval(5, 15)))
      fixture2.getIntersecting(interval(5, 15)) should contain theSameElementsAs allData2

      val allData3a = List(interval(0, 9) -> "Hello", interval(12, 20) -> "World")
      val allData3b = List(interval(-4, -2) -> "Goodbye", interval(6, 14) -> "Cruel", interval(16, 24) -> "World")

      val fixture3 = dataIn1DVersionedFrom1D(allData3a).zip(dataIn1DVersionedFrom1D(allData3b))
      val expected3 = List(
        interval(6, 9) -> ("Hello", "Cruel"),
        interval(12, 14) -> ("World", "Cruel"),
        interval(16, 20) -> ("World", "World")
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = dataIn1DVersionedFrom1D(allData3a).zipAll(dataIn1DVersionedFrom1D(allData3b), "<", ">")
      val expected4 = List(
        interval(-4, -2) -> ("<", "Goodbye"),
        interval(0, 5) -> ("Hello", ">"),
        interval(6, 9) -> ("Hello", "Cruel"),
        interval(10, 11) -> ("<", "Cruel"),
        interval(12, 14) -> ("World", "Cruel"),
        intervalAt(15) -> ("World", ">"),
        interval(16, 20) -> ("World", "World"),
        interval(21, 24) -> ("<", "World")
      )
      fixture4.getAll.toList shouldBe expected4

    test(s"$prefix: Looking up data in intervals"):
      val allData = List(interval(0, 9) -> "Hello", intervalFrom(10) -> "World")
      val fixture = dataIn1DVersionedFrom1D(allData)
      fixture.getAt(5) shouldBe Some("Hello")
      fixture.getAt(15) shouldBe Some("World")
      fixture.getAt(-1) shouldBe None
      fixture.getIntersecting(interval(5, 15)) should contain theSameElementsAs allData
