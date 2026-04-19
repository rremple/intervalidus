package intervalidus

import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.Interval1D.*
import intervalidus.DomainLike.given
import intervalidus.DimensionalVersionedBase.Versioned
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DVersionedBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  type IntDim = Domain.In1D[Int]
  type MixedDim = Domain.In1D[LocalDate]

  val dayZero: LocalDate = LocalDate.of(2024, 6, 30)

  def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  extension (t: LocalDateTime)
    def instant: Instant = t.atZone(ZoneId.of("America/Los_Angeles")).toInstant
    def asCurrent: CurrentInstant = CurrentInstant.simulated(t.instant)

  def validString(s: String, validTime: Interval.In1D[LocalDate]): ValidData.In1D[String, LocalDate] =
    validTime -> s

  def horizontal[H: DomainValueLike](interval: Interval[Versioned[Domain.In1D[H]]]): Interval1D[H] = interval(1)

  protected def testDataIn2D[T](
    current: Domain1D[Int],
    values: List[ValidData[T, IntDim]]
  ): List[ValidData[T, Versioned[IntDim]]] =
    values.map(d => (d.interval withHead intervalFrom(current)) -> d.value)

  def stringLookupTests[S <: DimensionalVersionedBase[String, IntDim]](
    prefix: String,
    dataIn1DVersionedFrom1D: CoreConfig[Versioned[IntDim]] ?=> Iterable[ValidData[String, IntDim]] => S,
    dataIn1DVersionedFrom2D: CoreConfig[Versioned[IntDim]] ?=> Iterable[ValidData[String, Versioned[IntDim]]] => S,
    dataIn1DVersionedOf: CoreConfig[Versioned[IntDim]] ?=> String => S
  )(using config: CoreConfig[Versioned[IntDim]]): Unit =
    test(s"$prefix: General setup"):
      {
        given CoreConfig[Versioned[IntDim]] = config.withExperimental(Experimental("requireDisjoint"))

        assertThrows[IllegalArgumentException]:
          // not valid as it overlaps on [10, +∞)
          val _ =
            dataIn1DVersionedFrom2D(testDataIn2D(0, List(interval(0, 10) -> "Hello", unbounded[Int] -> "World")))
      }

      val empty: S = dataIn1DVersionedFrom1D(Seq.empty)
      assert(empty.getAll.isEmpty)
      assert(empty.domain.isEmpty)
      assert((empty: Any) != ("<nothing is valid>": Any))
      empty.size shouldBe 0

      val single = dataIn1DVersionedOf("Hello world")
      single.get shouldBe "Hello world"
      single.getOption shouldBe Some("Hello world")
      single.domain.toList shouldBe List(Interval.unbounded[IntDim])
      assert(single.domainComplement.isEmpty)
      single shouldBe dataIn1DVersionedOf("Hello world")
      single.hashCode() shouldBe dataIn1DVersionedOf("Hello world").hashCode()
      single.size shouldBe 1

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
