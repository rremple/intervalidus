package intervalidus

import intervalidus.DiscreteValue.given
import intervalidus.Domain.In3D as Dim
import intervalidus.DomainLike.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn3DBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  protected val dayZero: LocalDate = LocalDate.of(2024, 7, 15)

  protected def day(offsetDays: Int): LocalDate = dayZero.plusDays(offsetDays)

  protected val unboundedDate = unbounded[LocalDate]

  def stringLookupTests[S <: DimensionalBase[String, Dim[LocalDate, LocalDate, Int]]](
    prefix: String,
    dataIn3DFrom: Experimental ?=> Iterable[ValidData[String, Dim[LocalDate, LocalDate, Int]]] => S,
    dataIn3DOf: Experimental ?=> String => S
  )(using Experimental): Unit = test(s"$prefix: Looking up data in intervals"):
    {
      given Experimental = Experimental("requireDisjoint")

      assertThrows[IllegalArgumentException]:
        // not valid as it overlaps in the second dimension on [10, +∞)
        val _ = dataIn3DFrom(
          List(
            (unboundedDate x unboundedDate x interval(0, 10)) -> "Hello",
            (unboundedDate x unboundedDate x unbounded[Int]) -> "World"
          )
        )
    }

    val empty = dataIn3DFrom(List.empty)
    assert(empty.isEmpty)
    empty.toString shouldBe "<nothing is valid>"
    assert((empty: Any) != ("<nothing is valid>": Any))
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)
    empty.domainComplement.toList shouldBe List(Interval.unbounded[Dim[LocalDate, LocalDate, Int]])

    val single = dataIn3DOf("Hello world")
    single.get shouldBe "Hello world"
    single.getOption shouldBe Some("Hello world")
    single.domain.toList shouldBe List(Interval.unbounded[Dim[LocalDate, LocalDate, Int]])
    assert(single.domainComplement.isEmpty)

    val bounded = (intervalFrom(0) x intervalTo(0) x intervalTo(0)) -> "Hello world"
    bounded.toString shouldBe "{[0..+∞), (-∞..0], (-∞..0]} -> Hello world"
    bounded.toCodeLikeString shouldBe
      "(intervalFrom(0) x intervalTo(0) x intervalTo(0)) -> \"Hello world\""
    assert(bounded.isDefinedAt(0, 0, 0))
    assert(!bounded.isDefinedAt(-1, 0, 0))
    bounded(0, 0, 0) shouldBe "Hello world"
    assertThrows[Exception]:
      val _ = bounded(-1, 0, 0)

    val fixture1 = dataIn3DFrom(
      List((intervalFrom(dayZero) x intervalTo(dayZero) x intervalFrom(0)) -> "Hello world")
    )
    fixture1.getOption shouldBe None
    assert(fixture1.isDefinedAt(day(1), dayZero, 0))
    fixture1(day(1), dayZero, 0) shouldBe "Hello world"
    assert(!fixture1.isDefinedAt(day(-1), day(1), 0))
    assertThrows[Exception]:
      val _ = fixture1(day(-1), day(1), 0)

    val now: Domain1D[LocalDate] =
      LocalDate.now // since all the dates are unbounded, this value shouldn't matter

    val allData2 = List(
      (unboundedDate x unboundedDate x interval(0, 10)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(11)) -> "World"
    )
    val fixture2 = dataIn3DFrom(allData2)
    fixture2.domain.toList shouldBe List(unbounded[Int] x unbounded[Int] x intervalFrom(0))
    fixture2.domainComplement.toList shouldBe List(unbounded[Int] x unbounded[Int] x intervalToBefore(0))
    fixture2.values should contain theSameElementsAs List("Hello", "World")
    fixture2.getAt(now x now x 5) shouldBe Some("Hello")
    fixture2.getAt(now x now x 15) shouldBe Some("World")
    fixture2.getAt(now x now x -1) shouldBe None
    assert(fixture2.intersects(unboundedDate x unboundedDate x interval(5, 15)))
    fixture2.getIntersecting(unboundedDate x unboundedDate x interval(5, 15)) should contain theSameElementsAs allData2

    val allData3a = List(
      (unboundedDate x unboundedDate x interval(0, 9)) -> "Hello",
      (unboundedDate x unboundedDate x interval(12, 20)) -> "World"
    )
    val allData3b = List(
      (unboundedDate x unboundedDate x interval(-4, -2)) -> "Goodbye",
      (unboundedDate x unboundedDate x interval(6, 14)) -> "Cruel",
      (unboundedDate x unboundedDate x interval(16, 24)) -> "World"
    )

    val fixture3 = dataIn3DFrom(allData3a).zip(dataIn3DFrom(allData3b))
    val expected3 = List(
      (unboundedDate x unboundedDate x interval(6, 9)) -> ("Hello", "Cruel"),
      (unboundedDate x unboundedDate x interval(12, 14)) -> ("World", "Cruel"),
      (unboundedDate x unboundedDate x interval(16, 20)) -> ("World", "World")
    )
    fixture3.getAll.toList shouldBe expected3

    val fixture4 = dataIn3DFrom(allData3a).zipAll(dataIn3DFrom(allData3b), "<", ">")
    val expected4 = List(
      (unboundedDate x unboundedDate x interval(-4, -2)) -> ("<", "Goodbye"),
      (unboundedDate x unboundedDate x interval(0, 5)) -> ("Hello", ">"),
      (unboundedDate x unboundedDate x interval(6, 9)) -> ("Hello", "Cruel"),
      (unboundedDate x unboundedDate x interval(10, 11)) -> ("<", "Cruel"),
      (unboundedDate x unboundedDate x interval(12, 14)) -> ("World", "Cruel"),
      (unboundedDate x unboundedDate x intervalAt(15)) -> ("World", ">"),
      (unboundedDate x unboundedDate x interval(16, 20)) -> ("World", "World"),
      (unboundedDate x unboundedDate x interval(21, 24)) -> ("<", "World")
    )
    fixture4.getAll.toList shouldBe expected4

  /*
   * Three-dimensional exclusions and updates can have 3 x 3 x 3 = 27 cases. But there are symmetries, so logically
   * only 10:
   * (1) simple = none + none + none (1 case)
   * (2) corner = single + single + single (1 case)
   * (3) core = split + split + split (1 case)
   * (4) face = single + none + none (3 symmetric cases)
   * (5) edge = single + single + none (3 symmetric cases)
   * (6) slice = split + none + none (3 symmetric cases)
   * (7) hole = split + split + none (3 symmetric cases)
   * (8) notch = split + single + single (3 symmetric cases)
   * (9) divot = split + split + single (3 symmetric cases)
   * (10) bite = split + single + none (6 symmetric cases)
   */
  protected def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData[String, Dim[Int, Int, Int]]*
  )(
    removeOrUpdateInterval: Interval[Dim[Int, Int, Int]],
    updateValue: String = "update"
  )(using Experimental): Assertion

  def removeOrUpdateTests(prefix: String)(using Experimental): Unit =
    test(s"$prefix: All remove/update by interval - (1) simple = none + none + none (1 case)"):
      // the same
      assertRemoveOrUpdateResult()(
        interval(-9, 9) x interval(-9, 9) x interval(-9, 9)
      )

      // larger
      assertRemoveOrUpdateResult()(
        intervalFrom(-15) x unbounded[Int] x intervalTo(15)
      )

    test(s"$prefix: All remove/update by interval - (2) corner = single + single + single (1 case)"):
      // each single remainder can be before or after: 2 * 2 * 2 = 8 subcases (each of the 8 corners)

      // remove left, below, back
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(1, 9)) -> "World", // left and right, below and above, front (4)
        (interval(-9, 9) x interval(1, 9) x interval(-9, 0)) -> "World", // left and right, above, back (2)
        (interval(1, 9) x interval(-9, 0) x interval(-9, 0)) -> "World" // right, below, back (1)
      )(
        intervalTo(0) x intervalTo(0) x intervalTo(0)
      )

      // remove left, below, front
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -1)) -> "World", // left and right, below and above, back (4)
        (interval(-9, 9) x interval(1, 9) x interval(0, 9)) -> "World", // left and right, above, front
        (interval(1, 9) x interval(-9, 0) x interval(0, 9)) -> "World" // right, below, front
      )(
        intervalTo(0) x intervalTo(0) x intervalFrom(0)
      )

      // remove left, above, back
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -1) x interval(-9, 9)) -> "World", // left and right, below, back and front
        (interval(-9, 9) x interval(0, 9) x interval(1, 9)) -> "World", // left and right, above, front
        (interval(1, 9) x interval(0, 9) x interval(-9, 0)) -> "World" // right, above, back
      )(
        intervalTo(0) x intervalFrom(0) x intervalTo(0)
      )

      // remove left, above, front
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -1) x interval(-9, 9)) -> "World", // left and right, below, back and front
        (interval(-9, 9) x interval(0, 9) x interval(-9, -1)) -> "World", // left and right, above, back
        (interval(1, 9) x interval(0, 9) x interval(0, 9)) -> "World" // right, above, front
      )(
        intervalTo(0) x intervalFrom(0) x intervalFrom(0)
      )

      // remove right, below, back
      assertRemoveOrUpdateResult(
        (interval(-9, -1) x interval(-9, 9) x interval(-9, 0)) -> "World", // left, below and above, back
        (interval(-9, 9) x interval(-9, 9) x interval(1, 9)) -> "World", // left and right, below and above, front
        (interval(0, 9) x interval(1, 9) x interval(-9, 0)) -> "World" // right, above, back
      )(
        intervalFrom(0) x intervalTo(0) x intervalTo(0)
      )

      // remove right, below, front
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -1)) -> "World", // left and right, below and above, back
        (interval(-9, -1) x interval(-9, 9) x interval(0, 9)) -> "World", // left, below and above, front
        (interval(0, 9) x interval(1, 9) x interval(0, 9)) -> "World" // right, above, front
      )(
        intervalFrom(0) x intervalTo(0) x intervalFrom(0)
      )

      // remove right, above, back
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -1) x interval(-9, 9)) -> "World", // left and right, below, back and front
        (interval(-9, -1) x interval(0, 9) x interval(-9, 9)) -> "World", // left, above, back and front
        (interval(0, 9) x interval(0, 9) x interval(1, 9)) -> "World" // right, above, front
      )(
        intervalFrom(0) x intervalFrom(0) x intervalTo(0)
      )

      // remove right, above, front
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -1) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(0, 9) x interval(-9, -1)) -> "World",
        (interval(-9, -1) x interval(0, 9) x interval(0, 9)) -> "World"
      )(
        intervalFrom(0) x intervalFrom(0) x intervalFrom(0)
      )

    test(s"$prefix: All remove/update by interval - (3) core = split + split + split (1 case)"):
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World", // left/mid/right, below, back/mid/front
        (interval(-9, 9) x interval(-4, 9) x interval(-9, -5)) -> "World", // left/mid/right, mid/above, back
        (interval(-9, -5) x interval(-4, 9) x interval(-4, 4)) -> "World", // left, mid/above, mid
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World", // left/mid/right, mid/above, front
        (interval(-4, 9) x interval(5, 9) x interval(-4, 4)) -> "World", // mid/right, above, mid
        (interval(5, 9) x interval(-4, 4) x interval(-4, 4)) -> "World" // right, mid, mid
      )(
        interval(-4, 4) x interval(-4, 4) x interval(-4, 4)
      )

    test(s"$prefix: All remove/update by interval -  (4) face = single + none + none (3 symmetric cases)"):
      // the single remainder can be before or after: 2 * 3 = 6 subcases (each of the 6 faces)

      /* symmetric case 1: left/right faces */

      // remove left face
      assertRemoveOrUpdateResult(
        (interval(-3, 9) x interval(-9, 9) x interval(-9, 9)) -> "World"
      )(
        intervalTo(-4) x unbounded[Int] x unbounded[Int]
      )

      // remove right face

      assertRemoveOrUpdateResult(
        (interval(-9, 3) x interval(-9, 9) x interval(-9, 9)) -> "World"
      )(
        intervalFrom(4) x unbounded[Int] x unbounded[Int]
      )

      /* symmetric case 2: bottom/top faces */

      // remove bottom face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-3, 9) x interval(-9, 9)) -> "World"
      )(
        unbounded[Int] x intervalTo(-4) x unbounded[Int]
      )

      // remove top face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 3) x interval(-9, 9)) -> "World"
      )(
        unbounded[Int] x intervalFrom(4) x unbounded[Int]
      )

      /* symmetric case 3: back/front faces */

      // remove back face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World"
      )(
        unbounded[Int] x unbounded[Int] x intervalTo(-4)
      )

      // remove front face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, 3)) -> "World"
      )(
        unbounded[Int] x unbounded[Int] x intervalFrom(4)
      )

    test(s"$prefix: All remove/update by interval -  (5) edge = single + single + none (3 symmetric cases)"):
      // the single remainders can be before or after, so each case can affect 2 * 2 = 4 edges, i.e., those
      // perpendicular
      // to that face: 2 * 2 * 3 = 12 subcases (each of the 12 edges)

      /* symmetric case 1: edges perpendicular to the left/right faces */

      // remove bottom, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(-9, 9) x interval(-3, 9) x interval(-9, -4)) -> "World"
      )(
        unbounded[Int] x intervalTo(-4) x intervalTo(-4)
      )

      // remove bottom, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(-3, 9) x interval(-9, 4)) -> "World"
      )(
        unbounded[Int] x intervalTo(-4) x intervalTo(4)
      )

      // remove top, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, -4)) -> "World"
      )(
        unbounded[Int] x intervalTo(4) x intervalTo(-4)
      )

      // remove top, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 4)) -> "World"
      )(
        unbounded[Int] x intervalTo(4) x intervalTo(4)
      )

      /* symmetric case 2: edges perpendicular to the top/bottom faces */

      // remove left, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(-3, 9) x interval(-9, 9) x interval(-9, -4)) -> "World"
      )(
        intervalTo(-4) x unbounded[Int] x intervalTo(-4)
      )

      // remove left, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-3, 9) x interval(-9, 9) x interval(-9, 4)) -> "World"
      )(
        intervalTo(-4) x unbounded[Int] x intervalTo(4)
      )

      // remove right, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-9, -4)) -> "World"
      )(
        intervalTo(4) x unbounded[Int] x intervalTo(-4)
      )

      // remove right, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-9, 4)) -> "World"
      )(
        intervalTo(4) x unbounded[Int] x intervalTo(4)
      )

      /* symmetric case 3: edges perpendicular to the front/back faces */

      // remove left, bottom edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-3, 9) x interval(-9, 9)) -> "World",
        (interval(-3, 9) x interval(-9, -4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(-4) x intervalTo(-4) x unbounded[Int]
      )

      // remove left, top edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(-3, 9) x interval(-9, 4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(-4) x intervalTo(4) x unbounded[Int]
      )

      // remove right, bottom edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-3, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(4) x intervalTo(-4) x unbounded[Int]
      )

      // remove right, top edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-9, 4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(4) x intervalTo(4) x unbounded[Int]
      )

    test(s"$prefix: All remove/update by interval -  (6) slice = split + none + none (3 symmetric cases)"):
      // no subcases

      /* symmetric case 1: slice horizontal */

      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 9)) -> "World", // left
        (interval(5, 9) x interval(-9, 9) x interval(-9, 9)) -> "World" // right
      )(
        interval(-4, 4) x unbounded[Int] x unbounded[Int]
      )

      /* symmetric case 2: slice vertical */

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World", // below
        (interval(-9, 9) x interval(5, 9) x interval(-9, 9)) -> "World" // above
      )(
        unbounded[Int] x interval(-4, 4) x unbounded[Int]
      )

      /* symmetric case 3: slice depth */

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World", // back
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World" // front
      )(
        unbounded[Int] x unbounded[Int] x interval(-4, 4)
      )

    test(s"$prefix: All remove/update by interval -  (7) hole = split + split + none (3 symmetric cases)"):
      // no subcases

      /* symmetric case 1: punch left to right */

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, -5) x interval(-4, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-4, 4)) -> "World"
      )(
        unbounded[Int] x interval(-4, 4) x interval(-4, 4)
      )

      /* symmetric case 2: punch bottom to top */

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, -5) x interval(-9, 9) x interval(-4, 9)) -> "World",
        (interval(-4, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-4, 4)) -> "World"
      )(
        interval(-4, 4) x unbounded[Int] x interval(-4, 4)
      )

      /* symmetric case 3: punch back to front */

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, -5) x interval(-4, 9) x interval(-9, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(-9, 9)) -> "World"
      )(
        interval(-4, 4) x interval(-4, 4) x unbounded[Int]
      )

    test(s"$prefix: All remove/update by interval -  (8) notch = split + single + single (3 symmetric cases)"):
      // the single remainders can be before or after, so each case can affect 2 * 2 = 4 edges to notch, i.e., those
      // perpendicular to that face: 2 * 2 * 3 = 12 subcases (notching each of the 12 edges)

      /* symmetric case 1: edges perpendicular to the left/right faces */

      // notch bottom, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, -4)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(-4, 9) x interval(-3, 9) x interval(-9, -4)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-9, -4)) -> "World"
      )(
        interval(-4, 4) x intervalTo(-4) x intervalTo(-4)
      )

      // notch bottom, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 4)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-4, 9) x interval(-3, 9) x interval(-9, 4)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-9, 4)) -> "World"
      )(
        interval(-4, 4) x intervalTo(-4) x intervalTo(4)
      )

      // notch top, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, -4)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(-9, -4)) -> "World",
        (interval(5, 9) x interval(-9, 4) x interval(-9, -4)) -> "World"
      )(
        interval(-4, 4) x intervalTo(4) x intervalTo(-4)
      )

      // notch top, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 4)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(-9, 4)) -> "World",
        (interval(5, 9) x interval(-9, 4) x interval(-9, 4)) -> "World"
      )(
        interval(-4, 4) x intervalTo(4) x intervalTo(4)
      )

      /* symmetric case 2: edges perpendicular to the top/bottom faces */

      // notch left, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-3, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, -4)) -> "World",
        (interval(-3, 9) x interval(-4, 4) x interval(-9, -4)) -> "World"
      )(
        intervalTo(-4) x interval(-4, 4) x intervalTo(-4)
      )

      // notch left, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 4)) -> "World",
        (interval(-3, 9) x interval(-4, 4) x interval(-9, 4)) -> "World"
      )(
        intervalTo(-4) x interval(-4, 4) x intervalTo(4)
      )

      // notch right, back edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-3, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, -4)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(-9, -4)) -> "World"
      )(
        intervalTo(4) x interval(-4, 4) x intervalTo(-4)
      )

      // notch right, front edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 4)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(-9, 4)) -> "World"
      )(
        intervalTo(4) x interval(-4, 4) x intervalTo(4)
      )

      /* symmetric case 3: edges perpendicular to the front/back faces */

      // notch left, bottom edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(-3, 9) x interval(-4, 4)) -> "World",
        (interval(-3, 9) x interval(-9, -4) x interval(-4, 4)) -> "World"
      )(
        intervalTo(-4) x intervalTo(-4) x interval(-4, 4)
      )

      // notch left, top edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-4, 4)) -> "World",
        (interval(-3, 9) x interval(-9, 4) x interval(-4, 4)) -> "World"
      )(
        intervalTo(-4) x intervalTo(4) x interval(-4, 4)
      )

      // notch right, bottom edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(-3, 9) x interval(-4, 4)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-4, 4)) -> "World"
      )(
        intervalTo(4) x intervalTo(-4) x interval(-4, 4)
      )

      // notch right, top edge
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-4, 4)) -> "World",
        (interval(5, 9) x interval(-9, 4) x interval(-4, 4)) -> "World"
      )(
        intervalTo(4) x intervalTo(4) x interval(-4, 4)
      )

    test(s"$prefix: All remove/update by interval -  (9) divot = split + split + single (3 symmetric cases)"):
      // the single remainder can be before or after: 2 * 3 = 6 subcases (each of the 6 faces)

      /* symmetric case 1: left/right faces */

      // divot in left face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-4, 4)) -> "World",
        (interval(-3, 9) x interval(-4, 4) x interval(-4, 4)) -> "World"
      )(
        intervalTo(-4) x interval(-4, 4) x interval(-4, 4)
      )

      // divot in right face

      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 3) x interval(-4, 9) x interval(-4, 4)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(4, 9) x interval(5, 9) x interval(-4, 4)) -> "World"
      )(
        intervalFrom(4) x interval(-4, 4) x interval(-4, 4)
      )

      /* symmetric case 2: bottom/top faces */

      // divot in bottom face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, -5) x interval(-9, 9) x interval(-4, 4)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-4, 9) x interval(-3, 9) x interval(-4, 4)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-4, 4)) -> "World"
      )(
        interval(-4, 4) x intervalTo(-4) x interval(-4, 4)
      )

      // divot in top face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 3) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(4, 9) x interval(-9, -5)) -> "World",
        (interval(-9, -5) x interval(4, 9) x interval(-4, 9)) -> "World",
        (interval(-4, 9) x interval(4, 9) x interval(5, 9)) -> "World",
        (interval(5, 9) x interval(4, 9) x interval(-4, 4)) -> "World"
      )(
        interval(-4, 4) x intervalFrom(4) x interval(-4, 4)
      )

      /* symmetric case 3: back/front faces */

      // divot in back face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, -5) x interval(-4, 9) x interval(-9, -4)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-3, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(-9, -4)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(-9, -4)) -> "World"
      )(
        interval(-4, 4) x interval(-4, 4) x intervalTo(-4)
      )

      // divot in front face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-9, 3)) -> "World",
        (interval(-9, -5) x interval(-4, 9) x interval(4, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(4, 9)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(4, 9)) -> "World"
      )(
        interval(-4, 4) x interval(-4, 4) x intervalFrom(4)
      )

    test(s"$prefix: All remove/update by interval -  (10) bite = split + single + none (6 symmetric cases)"):
      // the single remainder can be before or after, so each case can affect 2 faces: 2 * 6 = 12 subcases (biting each
      // of the 6 faces edges left to right or bottom to top)

      /* symmetric case 1: left and right faces scored bottom to top (single + none + split) */

      // bite left face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-3, 9) x interval(-9, 9) x interval(-4, 4)) -> "World"
      )(
        intervalTo(-4) x unbounded[Int] x interval(-4, 4)
      )

      // bite right face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-4, 4)) -> "World"
      )(
        intervalTo(4) x unbounded[Int] x interval(-4, 4)
      )

      /* symmetric case 2: left and right faces scored back to front (single + split + none) */

      // bite left face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(-3, 9) x interval(-4, 4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(-4) x interval(-4, 4) x unbounded[Int]
      )

      // bite right face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-4, 4) x interval(-9, 9)) -> "World"
      )(
        intervalTo(4) x interval(-4, 4) x unbounded[Int]
      )

      /* symmetric case 3: bottom and top faces scored left to right (none + single + split) */

      // bite bottom face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(-3, 9) x interval(-4, 4)) -> "World"
      )(
        unbounded[Int] x intervalTo(-4) x interval(-4, 4)
      )

      // bite top face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, 9) x interval(-9, -5)) -> "World",
        (interval(-9, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-4, 4)) -> "World"
      )(
        unbounded[Int] x intervalTo(4) x interval(-4, 4)
      )

      /* symmetric case 4: bottom and top faces scored back to front (split + single + none) */
      // bite bottom face
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 9)) -> "World",
        (interval(-4, 9) x interval(-3, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-9, -4) x interval(-9, 9)) -> "World"
      )(
        interval(-4, 4) x intervalTo(-4) x unbounded[Int]
      )

      // bite top face
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 9)) -> "World",
        (interval(-4, 9) x interval(5, 9) x interval(-9, 9)) -> "World",
        (interval(5, 9) x interval(-9, 4) x interval(-9, 9)) -> "World"
      )(
        interval(-4, 4) x intervalTo(4) x unbounded[Int]
      )

      /* symmetric case 5: back and front faces scored left to right (none + split + single) */
      // bite left face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(-3, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, -4)) -> "World"
      )(
        unbounded[Int] x interval(-4, 4) x intervalTo(-4)
      )

      // bite right face
      assertRemoveOrUpdateResult(
        (interval(-9, 9) x interval(-9, -5) x interval(-9, 9)) -> "World",
        (interval(-9, 9) x interval(-4, 9) x interval(5, 9)) -> "World",
        (interval(-9, 9) x interval(5, 9) x interval(-9, 4)) -> "World"
      )(
        unbounded[Int] x interval(-4, 4) x intervalTo(4)
      )

      /* symmetric case 6: back and front faces scored bottom to top (split + none + single) */
      // bite left face
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 9)) -> "World",
        (interval(-4, 9) x interval(-9, 9) x interval(-3, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-9, -4)) -> "World"
      )(
        interval(-4, 4) x unbounded[Int] x intervalTo(-4)
      )

      // bite right face
      assertRemoveOrUpdateResult(
        (interval(-9, -5) x interval(-9, 9) x interval(-9, 9)) -> "World",
        (interval(-4, 9) x interval(-9, 9) x interval(5, 9)) -> "World",
        (interval(5, 9) x interval(-9, 9) x interval(-9, 4)) -> "World"
      )(
        interval(-4, 4) x unbounded[Int] x intervalTo(4)
      )
