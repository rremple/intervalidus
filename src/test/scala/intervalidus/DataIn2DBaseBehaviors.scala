package intervalidus

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
    dataIn2DFrom: Iterable[ValidData2D[String, LocalDate, Int]] => S,
    dataIn2DOf: String => S
  ): Unit = test(s"$prefix: Looking up data in intervals - unbounded r1"):
    assertThrows[IllegalArgumentException]:
      // not valid as it overlaps in the second dimension on [10, +∞)
      val badFixture = dataIn2DFrom(testData(("Hello", unbounded, interval(0, 10)), ("World", unbounded, unbounded)))

    val empty = dataIn2DFrom(List.empty)
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val single = dataIn2DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

    val bounded = (intervalFrom(0) x intervalTo(0)) -> "Hello world"
    bounded.toString shouldBe "{[0..+∞), (-∞..0]} -> Hello world"
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
    fixture2.getIntersecting(unbounded x interval(5, 15)) shouldBe allData2

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