package intervalidus

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

  import DiscreteInterval1D.*

  protected def testData[T](
    values: (T, DiscreteInterval1D[LocalDate], DiscreteInterval1D[Int])*
  ): List[ValidData2D[T, LocalDate, Int]] = values.map(d => (d._2 x d._3) -> d._1).toList

  protected val dayZero: LocalDate = LocalDate.of(2024, 7, 15)

  protected def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  def stringLookupTests[S <: DataIn2DBase[String, LocalDate, Int]](
    prefix: String,
    dataIn2DFrom: Experimental ?=> Iterable[ValidData2D[String, LocalDate, Int]] => S,
    dataIn2DOf: Experimental ?=> String => S
  )(using Experimental): Unit = test(s"$prefix: Looking up data in intervals - unbounded r1"):
    {
      given Experimental = Experimental("requireDisjoint")

      assertThrows[IllegalArgumentException]:
        // not valid as it overlaps in the second dimension on [10, +∞)
        val badFixture = dataIn2DFrom(testData(("Hello", unbounded, interval(0, 10)), ("World", unbounded, unbounded)))
    }

    val empty = dataIn2DFrom(List.empty)
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val single = dataIn2DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

    val bounded = (intervalFrom(0) x intervalTo(0)) -> "Hello world"
    bounded.toString shouldBe "{[0..+∞), (-∞..0]} -> Hello world"
    bounded.toCodeLikeString shouldBe "(intervalFrom(0) x intervalTo(0)) -> \"Hello world\""
    assert(bounded.isDefinedAt(DiscreteDomain2D(0, 0)))
    assert(!bounded.isDefinedAt(DiscreteDomain2D(-1, 0)))
    bounded(DiscreteDomain2D(0, 0)) shouldBe "Hello world"
    assertThrows[Exception]:
      val bad = bounded(DiscreteDomain2D(-1, 0))

    val fixture1 = dataIn2DFrom(
      Seq((intervalFrom(dayZero) x intervalFrom(0)) -> "Hello world")
    )
    fixture1.getOption shouldBe None
    assert(fixture1.isDefinedAt(DiscreteDomain2D(dayZero, 0)))
    DiscreteDomain2D(dayZero, 0).flip shouldBe DiscreteDomain2D(0, dayZero)
    fixture1(DiscreteDomain2D(dayZero, 0)) shouldBe "Hello world"
    fixture1.flip(DiscreteDomain2D(0, dayZero)) shouldBe "Hello world"
    assert(!fixture1.isDefinedAt(DiscreteDomain2D(day(-1), 0)))
    assertThrows[Exception]:
      val missingData = fixture1(DiscreteDomain2D(day(-1), 0))

    val now = LocalDate.now // since all the dates are unbounded, this value shouldn't matter

    val allData2 = testData(("Hello", unbounded, interval(0, 10)), ("World", unbounded, intervalFrom(11)))
    val fixture2 = dataIn2DFrom(allData2)
    fixture2.domain.toList shouldBe List(unbounded[Int] x intervalFrom(0))
    fixture2.getAt(DiscreteDomain2D(now, 5)) shouldBe Some("Hello")
    fixture2.getAt(DiscreteDomain2D(now, 15)) shouldBe Some("World")
    fixture2.getAt(DiscreteDomain2D(now, -1)) shouldBe None
    assert(fixture2.intersects(unbounded x interval(5, 15)))
    fixture2.getIntersecting(unbounded x interval(5, 15)) should contain theSameElementsAs allData2

    val allData3a = testData(
      ("Hello", unbounded, interval(0, 9)),
      ("World", unbounded, interval(12, 20))
    )
    val allData3b = testData(
      ("Goodbye", unbounded, interval(-4, -2)),
      ("Cruel", unbounded, interval(6, 14)),
      ("World", unbounded, interval(16, 24))
    )

    val fixture3 = dataIn2DFrom(allData3a).zip(dataIn2DFrom(allData3b))
    val expected3 = testData(
      (("Hello", "Cruel"), unbounded, interval(6, 9)),
      (("World", "Cruel"), unbounded, interval(12, 14)),
      (("World", "World"), unbounded, interval(16, 20))
    )
    fixture3.getAll.toList shouldBe expected3

    val fixture4 = dataIn2DFrom(allData3a).zipAll(dataIn2DFrom(allData3b), "<", ">")
    val expected4 = testData(
      (("<", "Goodbye"), unbounded, interval(-4, -2)),
      (("Hello", ">"), unbounded, interval(0, 5)),
      (("Hello", "Cruel"), unbounded, interval(6, 9)),
      (("<", "Cruel"), unbounded, interval(10, 11)),
      (("World", "Cruel"), unbounded, interval(12, 14)),
      (("World", ">"), unbounded, interval(15, 15)),
      (("World", "World"), unbounded, interval(16, 20)),
      (("<", "World"), unbounded, interval(21, 24))
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
    removeOrUpdateInterval: DiscreteInterval2D[LocalDate, Int],
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
