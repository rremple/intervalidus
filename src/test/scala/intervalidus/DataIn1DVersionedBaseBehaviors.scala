package intervalidus

import intervalidus.DataIn1DBase.ValidData1D
import intervalidus.DataIn2DBase.ValidData2D
import intervalidus.DiscreteInterval1D.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DVersionedBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  protected def testData[T](values: (T, DiscreteInterval1D[Int])*): List[ValidData1D[T, Int]] =
    values.map(ValidData1D(_, _)).toList

  protected def testDataIn2D[T](
    current: DiscreteDomain1D[Int],
    values: List[ValidData1D[T, Int]]
  ): List[ValidData2D[T, Int, Int]] =
    values.map(d => (d.interval x intervalFrom(current)) -> d.value)

  def stringLookupTests[S <: DataIn1DVersionedBase[String, Int]](
    prefix: String,
    dataIn1DVersionedFrom1D: Iterable[ValidData1D[String, Int]] => S,
    dataIn1DVersionedFrom2D: Iterable[ValidData2D[String, Int, Int]] => S,
    dataIn1DVersionedOf: String => S
  ): Unit =
    test(s"$prefix: General setup"):
      assertThrows[IllegalArgumentException]:
        // not valid as it overlaps on [10, +∞)
        val badFixture =
          dataIn1DVersionedFrom2D(testDataIn2D(0, testData("Hello" -> interval(0, 10), "World" -> unbounded[Int])))

      val empty: S = dataIn1DVersionedFrom1D(Seq.empty)
      assert(empty.getAll.isEmpty)
      assert(empty.domain.isEmpty)

      val single = dataIn1DVersionedOf("Hello world")
      single.get shouldBe "Hello world"
      single.getOption shouldBe Some("Hello world")
      single.domain.toList shouldBe List(unbounded[Int])

      val fixture1: S = dataIn1DVersionedFrom1D(testData("Hello world" -> intervalFrom(0)))
      fixture1.getOption shouldBe None
      assert(fixture1.isDefinedAt(DiscreteDomain2D(0, 0)))
      assert(fixture1.isValidAt(0))
      fixture1(DiscreteDomain2D(0, 0)) shouldBe "Hello world"
      assert(!fixture1.isDefinedAt(DiscreteDomain2D(-1, 0)))
      assert(!fixture1.isValidAt(-1))
      assertThrows[Exception]:
        val missingData = fixture1(DiscreteDomain2D(-1, 0))

      val allData2 = testData("Hello" -> interval(0, 10), "World" -> intervalFrom(11))
      val fixture2 = dataIn1DVersionedFrom2D(testDataIn2D(0, allData2))
      fixture2.domain.toList shouldBe List(intervalFrom(0))
      fixture2.getAt(5) shouldBe Some("Hello")
      fixture2.getAt(15) shouldBe Some("World")
      fixture2.getAt(-1) shouldBe None
      assert(fixture2.intersects(interval(5, 15)))
      fixture2.getIntersecting(interval(5, 15)) shouldBe allData2

      val allData3a = testData("Hello" -> interval(0, 9), "World" -> interval(12, 20))
      val allData3b = testData("Goodbye" -> interval(-4, -2), "Cruel" -> interval(6, 14), "World" -> interval(16, 24))

      val fixture3 = dataIn1DVersionedFrom1D(allData3a).zip(dataIn1DVersionedFrom1D(allData3b))
      val expected3 = testData(
        ("Hello", "Cruel") -> interval(6, 9),
        ("World", "Cruel") -> interval(12, 14),
        ("World", "World") -> interval(16, 20)
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = dataIn1DVersionedFrom1D(allData3a).zipAll(dataIn1DVersionedFrom1D(allData3b), "<", ">")
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

    test(s"$prefix: Looking up data in intervals"):
      val allData = testData("Hello" -> interval(0, 9), "World" -> intervalFrom(10))
      val fixture = dataIn1DVersionedFrom1D(allData)
      fixture.getAt(5) shouldBe Some("Hello")
      fixture.getAt(15) shouldBe Some("World")
      fixture.getAt(-1) shouldBe None
      fixture.getIntersecting(interval(5, 15)) shouldBe allData
