package intervalidus

import intervalidus.DiscreteValue.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn2DBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  val unboundedDate = unbounded[LocalDate]
  val unboundedInt = unbounded[Int]

  protected val dayZero: LocalDate = LocalDate.of(2024, 7, 15)

  protected def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  def stringLookupTests[S <: DataIn2DBase[String, LocalDate, Int]](
    prefix: String,
    dataIn2DFrom: Experimental ?=> Iterable[ValidData2D[String, LocalDate, Int]] => S,
    dataIn2DOf: Experimental ?=> String => S
  )(using Experimental): Unit = test(s"$prefix: Looking up data in intervals"):
    {
      given Experimental = Experimental("requireDisjoint")

      assertThrows[IllegalArgumentException]:
        // not valid as it overlaps in the second dimension on [10, +∞)
        val _ = dataIn2DFrom(
          List(
            (unboundedDate x interval(0, 10)) -> "Hello",
            (unboundedDate x unboundedInt) -> "World"
          )
        )
    }

    val empty = dataIn2DFrom(List.empty)
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val single = dataIn2DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(Interval2D.unbounded[Int, Int])

    val bounded = (intervalFrom(0) x intervalTo(0)) -> "Hello world"
    bounded.toString shouldBe "{[0..+∞), (-∞..0]} -> Hello world"
    bounded.toCodeLikeString shouldBe "(intervalFrom(0) x intervalTo(0)) -> \"Hello world\""
    assert(bounded.isDefinedAt(0, 0))
    assert(!bounded.isDefinedAt(-1, 0))
    bounded(0, 0) shouldBe "Hello world"
    assertThrows[Exception]:
      val _ = bounded(-1, 0)

    val fixture1 = dataIn2DFrom(
      Seq((intervalFrom(dayZero) x intervalFrom(0)) -> "Hello world")
    )
    fixture1.getOption shouldBe None
    assert(fixture1.isDefinedAt(dayZero, 0))
    Domain2D(dayZero, 0).flip shouldBe Domain2D(0, dayZero)
    fixture1(dayZero, 0) shouldBe "Hello world"
    fixture1.flip(0, dayZero) shouldBe "Hello world"
    assert(!fixture1.isDefinedAt(day(-1), 0))
    assertThrows[Exception]:
      val _ = fixture1(day(-1), 0)

    val now = LocalDate.now // since all the dates are unbounded, this value shouldn't matter

    val allData2 = List(
      (unboundedDate x interval(0, 10)) -> "Hello",
      (unboundedDate x intervalFrom(11)) -> "World"
    )

    val fixture2 = dataIn2DFrom(allData2)
    fixture2.domain.toList shouldBe List(unboundedDate x intervalFrom(0))
    fixture2.getAt(now, 5) shouldBe Some("Hello")
    fixture2.getAt(now, 15) shouldBe Some("World")
    fixture2.getAt(now, -1) shouldBe None
    assert(fixture2.intersects(unbounded x interval(5, 15)))
    fixture2.getIntersecting(unbounded x interval(5, 15)) should contain theSameElementsAs allData2

    val allData3a = List(
      (unboundedDate x interval(0, 9)) -> "Hello",
      (unboundedDate x interval(12, 20)) -> "World"
    )
    val allData3b = List(
      (unboundedDate x interval(-4, -2)) -> "Goodbye",
      (unboundedDate x interval(6, 14)) -> "Cruel",
      (unboundedDate x interval(16, 24)) -> "World"
    )

    val fixture3 = dataIn2DFrom(allData3a).zip(dataIn2DFrom(allData3b))
    val expected3 = List(
      (unboundedDate x interval(6, 9)) -> ("Hello", "Cruel"),
      (unboundedDate x interval(12, 14)) -> ("World", "Cruel"),
      (unboundedDate x interval(16, 20)) -> ("World", "World")
    )
    fixture3.getAll.toList shouldBe expected3

    val fixture4 = dataIn2DFrom(allData3a).zipAll(dataIn2DFrom(allData3b), "<", ">")
    val expected4 = List(
      (unboundedDate x interval(-4, -2)) -> ("<", "Goodbye"),
      (unboundedDate x interval(0, 5)) -> ("Hello", ">"),
      (unboundedDate x interval(6, 9)) -> ("Hello", "Cruel"),
      (unboundedDate x interval(10, 11)) -> ("<", "Cruel"),
      (unboundedDate x interval(12, 14)) -> ("World", "Cruel"),
      (unboundedDate x intervalAt(15)) -> ("World", ">"),
      (unboundedDate x interval(16, 20)) -> ("World", "World"),
      (unboundedDate x interval(21, 24)) -> ("<", "World")
    )
    fixture4.getAll.toList shouldBe expected4

  /*
   * Two-dimensional removals and updates can have 3 x 3 = 9 cases. But there is symmetry for 3 of
   * them, so logically only 6:
   *  (1) simple + simple = simple (1)
   *  (2) simple + partial or partial + simple = edge (2), each with or without a common start
   *  (3) simple + split or split + simple = slice (2), vertical or horizontal
   *  (4) partial + partial = corner (1)
   *  (5) partial + split or split + partial = bite (2)
   *  (6) split + split = hole (1)
   */
  protected def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData2D[String, LocalDate, Int]*
  )(
    removeOrUpdateInterval: Interval2D[LocalDate, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion

  def removeOrUpdateTests(prefix: String)(using Experimental): Unit =
    test(s"$prefix: All remove/update by interval - (1) simple + simple = simple"):
      assertRemoveOrUpdateResult()(
        interval(day(-14), day(14)) x interval(4, 7)
      )
      assertRemoveOrUpdateResult()(
        interval(day(-15), day(15)) x interval(3, 8)
      )

    test(s"$prefix: All remove/update by interval - (2) simple + partial or partial + simple = edge (2)"):
      // each with or without a common start

      // partial + simple, to the right (remainder with common start)
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(7)) x interval(4, 7)) -> "World"
      )(interval(day(8), day(14)) x interval(3, 8))

      // partial + simple, to the left (remainder does not have a common start)
      assertRemoveOrUpdateResult(
        (interval(day(0), day(14)) x interval(4, 7)) -> "World"
      )(interval(day(-15), day(-1)) x interval(3, 8))

      // simple + partial, above (remainder with common start)
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(4, 5)) -> "World"
      )(interval(day(-15), day(15)) x interval(6, 8))

      // simple + partial, below (remainder does not have a common start)
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(6, 7)) -> "World"
      )(interval(day(-15), day(15)) x interval(1, 5))

    test(s"$prefix: All remove/update by interval - (3) simple + split or split + simple = slice (2)"):
      // vertical slice, resulting in a left and right elements
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(-2)) x interval(4, 7)) -> "World",
        (interval(day(2), day(14)) x interval(4, 7)) -> "World"
      )(interval(day(-1), day(1)) x interval(3, 8))

      // horizontal slice, resulting in a lower and upper elements
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x intervalAt(4)) -> "World",
        (interval(day(-14), day(14)) x interval(6, 7)) -> "World"
      )(interval(day(-15), day(15)) x intervalAt(5))

    test(s"$prefix: All remove/update by interval - (4) partial + partial = corner (1)"):

      // lower left
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(6, 7)) -> "World",
        (interval(day(-7), day(14)) x interval(4, 5)) -> "World"
      )(interval(day(-15), day(-8)) x interval(3, 5))

      // upper left
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(4, 5)) -> "World",
        (interval(day(-7), day(14)) x interval(6, 7)) -> "World"
      )(interval(day(-15), day(-8)) x interval(6, 8))

      // lower right
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(7)) x interval(4, 7)) -> "World",
        (interval(day(8), day(14)) x interval(6, 7)) -> "World"
      )(interval(day(8), day(15)) x interval(3, 5))

      // upper right
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(4, 5)) -> "World",
        (interval(day(-14), day(7)) x interval(6, 7)) -> "World"
      )(interval(day(8), day(15)) x interval(6, 8))

    test(s"$prefix: All remove/update by interval - (5) partial + split or split + partial = bite (2)"):

      // bite left
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x intervalAt(4)) -> "World",
        (interval(day(-14), day(14)) x intervalAt(7)) -> "World",
        (interval(day(-7), day(14)) x interval(5, 6)) -> "World"
      )(interval(day(-15), day(-8)) x interval(5, 6))

      // bite right
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x intervalAt(4)) -> "World",
        (interval(day(-14), day(7)) x interval(5, 7)) -> "World",
        (interval(day(8), day(14)) x intervalAt(7)) -> "World"
      )(interval(day(8), day(15)) x interval(5, 6))

      // bite below
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(-7)) x interval(4, 7)) -> "World",
        (interval(day(-6), day(14)) x interval(6, 7)) -> "World",
        (interval(day(7), day(14)) x interval(4, 5)) -> "World"
      )(interval(day(-6), day(6)) x interval(3, 5))

      // bite above
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x interval(4, 5)) -> "World",
        (interval(day(-14), day(-7)) x interval(6, 7)) -> "World",
        (interval(day(7), day(14)) x interval(6, 7)) -> "World"
      )(interval(day(-6), day(6)) x interval(6, 8))

    test(s"$prefix: All remove/update by interval - (6) split + split = hole (1)"):
      assertRemoveOrUpdateResult(
        (interval(day(-14), day(14)) x intervalAt(4)) -> "World",
        (interval(day(-14), day(-7)) x interval(5, 7)) -> "World",
        (interval(day(-6), day(14)) x intervalAt(7)) -> "World",
        (interval(day(7), day(14)) x interval(5, 6)) -> "World"
      )(interval(day(-6), day(6)) x interval(5, 6))
