package intervalidus

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn2DVersionedBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  val dayZero: LocalDate = LocalDate.of(2024, 6, 30)

  def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  def validString(
    s: String,
    validTime: Interval2D[LocalDate, LocalDate]
  ): ValidData2D[String, LocalDate, LocalDate] = validTime -> s

  protected def testDataIn3D[T](
    current: Domain1D[Int],
    values: List[ValidData2D[T, Int, Int]]
  ): List[ValidData3D[T, Int, Int, Int]] = values.map(d => (d.interval x intervalFrom(current)) -> d.value)

  def stringLookupTests[S <: DataIn2DVersionedBase[String, Int, Int]](
    prefix: String,
    dataIn2DVersionedFrom2D: Experimental ?=> Iterable[ValidData2D[String, Int, Int]] => S,
    dataIn2DVersionedFrom3D: Experimental ?=> Iterable[ValidData3D[String, Int, Int, Int]] => S,
    dataIn2DVersionedOf: Experimental ?=> String => S
  )(using Experimental): Unit =
    test(s"$prefix: General setup"):
      {
        given Experimental = Experimental("requireDisjoint")

        assertThrows[IllegalArgumentException]:
          // not valid as it overlaps on [10, +∞)
          val _ = dataIn2DVersionedFrom3D(
            testDataIn3D(
              0,
              List(
                (interval(0, 10) x interval(0, 10)) -> "Hello",
                Interval2D.unbounded[Int, Int] -> "World"
              )
            )
          )
      }

      val empty: S = dataIn2DVersionedFrom2D(Seq.empty)
      assert(empty.getAll.isEmpty)
      assert(empty.domain.isEmpty)

      val single = dataIn2DVersionedOf("Hello world")
      single.get shouldBe "Hello world"
      single.getOption shouldBe Some("Hello world")
      single.domain.toList shouldBe List(Interval2D.unbounded[Int, Int])

      val fixture1: S = dataIn2DVersionedFrom2D(List((intervalFrom(0) x intervalFrom(0)) -> "Hello world"))
      fixture1.getOption shouldBe None
      assert(fixture1.isDefinedAt(0, 0, 0))
      assert(fixture1.isValidAt(0, 0))
      fixture1(0, 0, 0) shouldBe "Hello world"
      assert(!fixture1.isDefinedAt(-1, 0, 0))
      assert(!fixture1.isValidAt(-1, 0))
      assertThrows[Exception]:
        val _ = fixture1(-1, 0, 0)

      val allData2 = List(
        (interval(0, 10) x interval(0, 10)) -> "Hello",
        (intervalFrom(11) x interval(0, 10)) -> "World"
      )
      val fixture2 = dataIn2DVersionedFrom3D(testDataIn3D(0, allData2))
      fixture2.domain.toList shouldBe List(intervalFrom(0) x interval(0, 10))
      fixture2.getAt(5, 5) shouldBe Some("Hello")
      fixture2.getAt(15, 5) shouldBe Some("World")
      fixture2.getAt(-1, -1) shouldBe None
      assert(fixture2.intersects(interval(5, 15) x interval(5, 15)))
      fixture2.getIntersecting(interval(5, 15) x interval(5, 15)) should contain theSameElementsAs allData2

      val allData3a = List(
        (interval(0, 9) x interval(0, 9)) -> "Hello",
        (interval(12, 20) x interval(12, 20)) -> "World"
      )
      val allData3b = List(
        (interval(-4, -2) x interval(-4, -2)) -> "Goodbye",
        (interval(6, 14) x interval(6, 14)) -> "Cruel",
        (interval(16, 24) x interval(16, 24)) -> "World"
      )

      val fixture3 = dataIn2DVersionedFrom2D(allData3a).zip(dataIn2DVersionedFrom2D(allData3b))
      val expected3 = List(
        (interval(6, 9) x interval(6, 9)) -> ("Hello", "Cruel"),
        (interval(12, 14) x interval(12, 14)) -> ("World", "Cruel"),
        (interval(16, 20) x interval(16, 20)) -> ("World", "World")
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = dataIn2DVersionedFrom2D(allData3a).zipAll(dataIn2DVersionedFrom2D(allData3b), "<", ">")
      val expected4 = List(
        (interval(-4, -2) x interval(-4, -2)) -> ("<", "Goodbye"),
        (interval(0, 9) x interval(0, 5)) -> ("Hello", ">"),
        (interval(0, 5) x interval(6, 9)) -> ("Hello", ">"),
        (interval(6, 9) x interval(6, 9)) -> ("Hello", "Cruel"),
        (interval(6, 14) x interval(10, 11)) -> ("<", "Cruel"),
        (interval(6, 11) x interval(12, 14)) -> ("<", "Cruel"),
        (interval(10, 14) x interval(6, 9)) -> ("<", "Cruel"),
        (interval(12, 14) x interval(12, 14)) -> ("World", "Cruel"),
        (interval(12, 20) x intervalAt(15)) -> ("World", ">"),
        (interval(12, 15) x interval(16, 20)) -> ("World", ">"),
        (interval(15, 20) x interval(12, 14)) -> ("World", ">"),
        (interval(16, 20) x interval(16, 20)) -> ("World", "World"),
        (interval(16, 24) x interval(21, 24)) -> ("<", "World"),
        (interval(21, 24) x interval(16, 20)) -> ("<", "World")
      )
      fixture4.getAll.toList shouldBe expected4

    test(s"$prefix: Looking up data in intervals"):
      val allData = List(
        (interval(0, 9) x interval(0, 9)) -> "Hello",
        (intervalFrom(10) x intervalFrom(10)) -> "World"
      )
      val fixture = dataIn2DVersionedFrom2D(allData)
      fixture.getAt(5, 5) shouldBe Some("Hello")
      fixture.getAt(15, 15) shouldBe Some("World")
      fixture.getAt(-1, -1) shouldBe None
      fixture.getIntersecting(interval(5, 15) x interval(5, 15)) should contain theSameElementsAs allData
