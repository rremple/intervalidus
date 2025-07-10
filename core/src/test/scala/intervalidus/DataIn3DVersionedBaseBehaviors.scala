package intervalidus

import intervalidus.DomainLike.given
import intervalidus.Domain.In3D as Dim
import intervalidus.DimensionalVersionedBase.Versioned
import intervalidus.Interval.Patterns.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn3DVersionedBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import DiscreteValue.IntDiscreteValue
  import Interval1D.*

  val dayZero: LocalDate = LocalDate.of(2024, 6, 30)

  def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  def horizontal[H: DomainValueLike, V: DomainValueLike, D: DomainValueLike](
    interval: Interval[Versioned[Domain.In3D[H, V, D]]]
  ): Interval1D[H] = interval match
    case _ :+: horizontal :+: _ :+|: _ => horizontal
    case _ => throw Exception(s"Expected versioned three-dimensional interval, got $interval")

  protected def testDataIn4D[T](
    current: Domain1D[Int],
    values: List[ValidData[T, Dim[Int, Int, Int]]]
  )(using DomainValueLike[Int]): List[ValidData[T, Versioned[Dim[Int, Int, Int]]]] =
    values.map(d => (d.interval withHead intervalFrom(current)) -> d.value)

  def stringLookupTests[S <: DimensionalVersionedBase[String, Dim[Int, Int, Int]]](
    prefix: String,
    dataIn3DVersionedFrom3D: Experimental ?=> Iterable[ValidData[String, Dim[Int, Int, Int]]] => S,
    dataIn3DVersionedFrom4D: Experimental ?=> Iterable[ValidData[String, Versioned[Dim[Int, Int, Int]]]] => S,
    dataIn3DVersionedOf: Experimental ?=> String => S
  )(using Experimental, DomainValueLike[Int]): Unit =
    test(s"$prefix: General setup"):
      {
        given Experimental = Experimental("requireDisjoint")

        assertThrows[IllegalArgumentException]:
          // not valid as it overlaps on [10, +∞)
          val _ = dataIn3DVersionedFrom4D(
            testDataIn4D(
              0,
              List(
                (interval(0, 10) x interval(0, 10) x unbounded[Int]) -> "Hello",
                Interval.unbounded[Dim[Int, Int, Int]] -> "World"
              )
            )
          )
      }

      val empty: S = dataIn3DVersionedFrom3D(Seq.empty)
      assert(empty.getAll.isEmpty)
      assert(empty.domain.isEmpty)

      val single = dataIn3DVersionedOf("Hello world")
      single.get shouldBe "Hello world"
      single.getOption shouldBe Some("Hello world")
      single.domain.toList shouldBe List(Interval.unbounded[Dim[Int, Int, Int]])
      single.domainComplement.toList shouldBe List.empty

      val fixture1: S =
        dataIn3DVersionedFrom3D(List((intervalFrom(0) x intervalFrom(0) x unbounded[Int]) -> "Hello world"))
      fixture1.getOption shouldBe None
      assert(fixture1.isDefinedAt(0, 0, 0, 0))
      assert(fixture1.isValidAt(0, 0, 0))
      fixture1(0, 0, 0, 0) shouldBe "Hello world"
      assert(!fixture1.isDefinedAt(-1, 0, 0, 0))
      assert(!fixture1.isValidAt(-1, 0, 0))
      assertThrows[Exception]:
        val _ = fixture1(-1, 0, 0, 0)

      val allData2 = List(
        (interval(0, 10) x interval(0, 10) x unbounded[Int]) -> "Hello",
        (intervalFrom(11) x interval(0, 10) x unbounded[Int]) -> "World"
      )
      val fixture2 = dataIn3DVersionedFrom4D(testDataIn4D(0, allData2))
      fixture2.domain.toList shouldBe List(intervalFrom(0) x interval(0, 10) x unbounded[Int])
      fixture2.domainComplement.toList shouldBe List(
        unbounded[Int] x intervalToBefore(0) x unbounded[Int],
        intervalToBefore(0) x intervalFrom(0) x unbounded[Int],
        intervalFrom(0) x intervalFromAfter(10) x unbounded[Int]
      )
      fixture2.getAt(5, 5, 0) shouldBe Some("Hello")
      fixture2.getDataAt(15, 5, 0) shouldBe Some((intervalFrom(11) x interval(0, 10) x unbounded[Int]) -> "World")
      fixture2.getAt(-1, -1, 0) shouldBe None
      assert(fixture2.intersects(interval(5, 15) x interval(5, 15) x unbounded[Int]))
      fixture2.getIntersecting(
        interval(5, 15) x interval(5, 15) x unbounded[Int]
      ) should contain theSameElementsAs allData2

      val allData3a = List(
        (interval(0, 9) x interval(0, 9) x unbounded[Int]) -> "Hello",
        (interval(12, 20) x interval(12, 20) x unbounded[Int]) -> "World"
      )
      val allData3b = List(
        (interval(-4, -2) x interval(-4, -2) x unbounded[Int]) -> "Goodbye",
        (interval(6, 14) x interval(6, 14) x unbounded[Int]) -> "Cruel",
        (interval(16, 24) x interval(16, 24) x unbounded[Int]) -> "World"
      )

      val fixture3 = dataIn3DVersionedFrom3D(allData3a).zip(dataIn3DVersionedFrom3D(allData3b))
      val expected3 = List(
        (interval(6, 9) x interval(6, 9) x unbounded[Int]) -> ("Hello", "Cruel"),
        (interval(12, 14) x interval(12, 14) x unbounded[Int]) -> ("World", "Cruel"),
        (interval(16, 20) x interval(16, 20) x unbounded[Int]) -> ("World", "World")
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = dataIn3DVersionedFrom3D(allData3a).zipAll(dataIn3DVersionedFrom3D(allData3b), "<", ">")
      val expected4 = List(
        (interval(-4, -2) x interval(-4, -2) x unbounded[Int]) -> ("<", "Goodbye"),
        (interval(0, 9) x interval(0, 5) x unbounded[Int]) -> ("Hello", ">"),
        (interval(0, 5) x interval(6, 9) x unbounded[Int]) -> ("Hello", ">"),
        (interval(6, 9) x interval(6, 9) x unbounded[Int]) -> ("Hello", "Cruel"),
        (interval(6, 14) x interval(10, 11) x unbounded[Int]) -> ("<", "Cruel"),
        (interval(6, 11) x interval(12, 14) x unbounded[Int]) -> ("<", "Cruel"),
        (interval(10, 14) x interval(6, 9) x unbounded[Int]) -> ("<", "Cruel"),
        (interval(12, 14) x interval(12, 14) x unbounded[Int]) -> ("World", "Cruel"),
        (interval(12, 20) x intervalAt(15) x unbounded[Int]) -> ("World", ">"),
        (interval(12, 15) x interval(16, 20) x unbounded[Int]) -> ("World", ">"),
        (interval(15, 20) x interval(12, 14) x unbounded[Int]) -> ("World", ">"),
        (interval(16, 20) x interval(16, 20) x unbounded[Int]) -> ("World", "World"),
        (interval(16, 24) x interval(21, 24) x unbounded[Int]) -> ("<", "World"),
        (interval(21, 24) x interval(16, 20) x unbounded[Int]) -> ("<", "World")
      )
      fixture4.getAll.toList shouldBe expected4

    test(s"$prefix: Looking up data in intervals"):
      val allData = List(
        (interval(0, 9) x interval(0, 9) x unbounded[Int]) -> "Hello",
        (intervalFrom(10) x intervalFrom(10) x unbounded[Int]) -> "World"
      )
      val fixture = dataIn3DVersionedFrom3D(allData)
      fixture.getAt(5, 5, 0) shouldBe Some("Hello")
      fixture.getAt(15, 15, 0) shouldBe Some("World")
      fixture.getAt(-1, -1, 0) shouldBe None
      fixture.getIntersecting(
        interval(5, 15) x interval(5, 15) x unbounded[Int]
      ) should contain theSameElementsAs allData
