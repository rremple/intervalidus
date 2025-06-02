package intervalidus

import intervalidus.ContinuousValue.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

class ContinuousIntervalTest extends AnyFunSuite with Matchers:

  import Domain1D.{Bottom, Point, Top, open}
  import Interval1D.*

  test("Int interval validity"):
    assertThrows[IllegalArgumentException]:
      val _ = interval(2, 1) // end before start

    assertThrows[IllegalArgumentException]:
      val _ = Interval1D[Int](Top, Bottom) // end before start

  test("Int interval adjacency, etc."):
    assert(interval(1, 2) isLeftAdjacentTo intervalFromAfter(2).to(4))
    assert(!(interval(3, 4) isLeftAdjacentTo interval(1, 2)))
    assert(!(interval(1, 3) isLeftAdjacentTo interval(2, 4)))
    assert(!(interval(1, 3) isLeftAdjacentTo interval(3, 4)))
    assert(intervalTo(2) isLeftAdjacentTo intervalFromAfter(2))
    assert(intervalToBefore(3) isLeftAdjacentTo intervalFrom(3))
    assert(intervalFromAfter(1) isRightAdjacentTo intervalTo(1))
    assert(!(intervalFrom(2) isLeftAdjacentTo intervalTo(3)))

    assert(interval(3, 4) isRightAdjacentTo intervalFrom(1).toBefore(3))
    assert(!(interval(1, 2) isRightAdjacentTo interval(3, 4)))
    assert(!(interval(2, 4) isRightAdjacentTo interval(1, 3)))
    assert(!(interval(3, 4) isRightAdjacentTo interval(1, 3)))
    assert(intervalFrom(3) isRightAdjacentTo intervalToBefore(3))
    assert(!(intervalTo(3) isRightAdjacentTo intervalFrom(2)))

    assert(interval(1, 2) isAdjacentTo intervalFromAfter(2).to(4))
    assert(interval(3, 4) isAdjacentTo intervalFrom(1).toBefore(3))
    assert(intervalTo(2) isAdjacentTo intervalFromAfter(2))
    assert(intervalFrom(3) isAdjacentTo intervalToBefore(3))
    assert(!(intervalFrom(2) isAdjacentTo intervalTo(3)))
    assert(!(intervalTo(3) isAdjacentTo intervalFrom(2)))

    interval(1, 2).after shouldBe Some(intervalFromAfter(2))
    interval(1, 2).before shouldBe Some(intervalToBefore(1))
    intervalFrom(2).after shouldBe None
    intervalTo(2).before shouldBe None
    interval(1, 3).points.toList shouldBe List.empty

    intervalFrom(0).toCodeLikeString shouldBe "intervalFrom(0)"
    intervalTo(0).toCodeLikeString shouldBe "intervalTo(0)"
    intervalAt(0).toCodeLikeString shouldBe "intervalAt(0)"
    interval(0, 1).toCodeLikeString shouldBe "interval(0, 1)"
    unbounded[Int].toCodeLikeString shouldBe "unbounded"

    intervalFrom(LocalDate.of(2025, 1, 10)).toCodeLikeString shouldBe "intervalFrom(LocalDate.of(2025,1,10))"
    intervalTo(LocalDate.of(2025, 1, 10)).toCodeLikeString shouldBe "intervalTo(LocalDate.of(2025,1,10))"
    intervalAt(LocalDate.of(2025, 1, 10)).toCodeLikeString shouldBe "intervalAt(LocalDate.of(2025,1,10))"
    intervalAt(open(LocalDate.of(2025, 1, 10))).toCodeLikeString shouldBe "intervalAt(LocalDate.of(2025,1,10))"
    interval(LocalDate.of(2025, 1, 10), LocalDate.of(2025, 1, 11)).toCodeLikeString shouldBe
      "interval(LocalDate.of(2025,1,10), LocalDate.of(2025,1,11))"
    intervalFromAfter(LocalDate.of(2025, 1, 10).atTime(1, 2, 3, 4)).toCodeLikeString shouldBe
      "intervalFromAfter(LocalDate.of(2025,1,10).atTime(1,2,3,4))"
    unbounded[LocalDate].toCodeLikeString shouldBe "unbounded"
    intervalFrom[LocalDate](Top).toCodeLikeString shouldBe "intervalFrom(Top)"
    intervalTo[LocalDate](Bottom).toCodeLikeString shouldBe "intervalTo(Bottom)"
    intervalFromAfter(0).toBefore(1).toCodeLikeString shouldBe "interval(open(0), open(1))"

  test("Int 2D interval adjacency, etc."):
    val now = LocalDate.now
    val d: Interval2D[LocalDate, Int] = intervalTo(now) x intervalFrom(0)
    d.flip shouldBe (intervalFrom(0) x intervalTo(now))

    def interval2d(hs: Domain1D[Int], he: Domain1D[Int], vs: Domain1D[Int], ve: Domain1D[Int]) =
      interval(hs, he) x interval(vs, ve)

    def interval2dFrom(hs: Domain1D[Int], vs: Domain1D[Int]) = intervalFrom(hs) x intervalFrom(vs)

    def interval2dTo(he: Domain1D[Int], ve: Domain1D[Int]) = intervalTo(he) x intervalTo(ve)

    interval2d(1, 2, 3, 4).toString shouldBe "{[1, 2], [3, 4]}"

    interval2d(1, 2, 3, 4) intersectionWith interval2d(2, 3, 4, 5) shouldBe Some(interval2d(2, 2, 4, 4))
    interval2d(1, 2, 3, 4) ∩ interval2d(2, 3, 4, 5) shouldBe Some(interval2d(2, 2, 4, 4))
    assert(interval2d(1, 2, 3, 4) contains interval2d(2, 2, 4, 4))
    assert(interval2d(2, 2, 4, 4) isSubsetOf interval2d(1, 2, 3, 4))
    assert(interval2d(2, 2, 4, 4) ⊆ interval2d(1, 2, 3, 4))

    assert(interval2d(1, 2, 3, 4) isLeftAdjacentTo interval2d(open(2), 4, 3, 4))
    assert(interval2d(1, 2, 3, 4) isAdjacentTo interval2d(open(2), 4, 3, 4))
    assert(!(interval2d(3, 4, 3, 4) isLeftAdjacentTo interval2d(1, 2, 3, 4)))
    assert(!(interval2d(1, 3, 3, 4) isLeftAdjacentTo interval2d(2, 4, 3, 4)))
    assert(!(interval2d(1, 3, 3, 4) isLeftAdjacentTo interval2d(3, 4, 3, 4)))
    assert(interval2dTo(2, 2) isLeftAdjacentTo (intervalFromAfter(2) x intervalTo(2)))
    assert(!(interval2dFrom(2, 2) isLeftAdjacentTo interval2dTo(3, 3)))

    assert(interval2d(open(2), 4, 1, 2) isRightAdjacentTo interval2d(1, 2, 1, 2))
    assert(interval2d(open(2), 4, 1, 2) isAdjacentTo interval2d(1, 2, 1, 2))
    assert(!(interval2d(1, 2, 1, 2) isRightAdjacentTo interval2d(3, 4, 1, 2)))
    assert(!(interval2d(2, 4, 1, 2) isRightAdjacentTo interval2d(1, 3, 1, 2)))
    assert(!(interval2d(3, 4, 1, 2) isRightAdjacentTo interval2d(1, 3, 1, 2)))
    assert(interval2dFrom(open(2), 3) isRightAdjacentTo (intervalTo(2) x intervalFrom(3)))
    assert(!(interval2dTo(open(2), 3) isRightAdjacentTo interval2dFrom(2, 2)))

    assert(interval2d(3, 4, 1, 2) isLowerAdjacentTo interval2d(3, 4, open(2), 4))
    assert(interval2d(3, 4, 1, 2) isAdjacentTo interval2d(3, 4, open(2), 4))
    assert(!(interval2d(3, 4, 3, 4) isLowerAdjacentTo interval2d(3, 4, 1, 2)))
    assert(!(interval2d(3, 4, 1, 3) isLowerAdjacentTo interval2d(3, 4, 2, 4)))
    assert(!(interval2d(3, 4, 1, 3) isLowerAdjacentTo interval2d(3, 4, 3, 4)))
    assert(interval2dTo(2, 2) isLowerAdjacentTo (intervalTo(2) x intervalFromAfter(2)))
    assert(!(interval2dFrom(2, 2) isLowerAdjacentTo interval2dTo(3, 3)))

    assert(interval2d(1, 2, open(2), 4) isUpperAdjacentTo interval2d(1, 2, 1, 2))
    assert(interval2d(1, 2, open(2), 4) isAdjacentTo interval2d(1, 2, 1, 2))
    assert(!(interval2d(1, 2, 1, 2) isUpperAdjacentTo interval2d(1, 2, 3, 4)))
    assert(!(interval2d(1, 2, 2, 4) isUpperAdjacentTo interval2d(1, 2, 1, 3)))
    assert(!(interval2d(1, 2, 3, 4) isUpperAdjacentTo interval2d(1, 2, 1, 3)))
    assert(interval2dFrom(3, 3) isUpperAdjacentTo (intervalFrom(3) x intervalToBefore(3)))
    assert(!(interval2dTo(3, 3) isUpperAdjacentTo interval2dFrom(2, 2)))

    interval2d(1, 2, 3, 4).after shouldBe Some(interval2dFrom(open(2), open(4)))
    interval2d(1, 2, 3, 4).before shouldBe Some(interval2dTo(open(1), open(3)))
    interval2dFrom(2, 2).after shouldBe None
    interval2dTo(2, 2).before shouldBe None
    interval2d(1, 3, 1, 3).points.toList shouldBe List.empty

    assert(interval2d(1, 2, 3, 4) hasSameStartAs interval2dFrom(1, 3))
    assert(interval2d(1, 2, 3, 4) hasSameEndAs interval2dTo(2, 4))
    (intervalFrom(0) x intervalTo(0)).toCodeLikeString shouldBe
      "intervalFrom(0) x intervalTo(0)"

    interval2d(1, 2, 3, 4).fromBottom shouldBe interval2dTo(2, 4)
    interval2d(1, 2, 3, 4).toTop shouldBe interval2dFrom(1, 3)
    interval2d(1, 2, 3, 4).from(0, 0) shouldBe interval2d(0, 2, 0, 4)
    interval2d(1, 2, 3, 4).fromAfter(1, 1) shouldBe interval2d(open(1), 2, open(1), 4)
    interval2d(1, 2, 3, 4).to(5, 5) shouldBe interval2d(1, 5, 3, 5)
    interval2d(1, 2, 3, 4).toBefore(7, 7) shouldBe interval2d(1, open(7), 3, open(7))
    interval2d(1, 2, 3, 4).atStart shouldBe interval2d(1, 1, 3, 3)
    interval2d(1, 2, 3, 4).atEnd shouldBe interval2d(2, 2, 4, 4)
    interval2d(1, 2, 3, 4).gapWith(interval2d(5, 6, 7, 8)) shouldBe Some(interval2d(open(2), open(5), open(4), open(7)))
    // even though neither adjacent nor intersecting in 2D, no gap in _all_ dimensions
    interval2d(1, 2, 3, 4).gapWith(interval2d(5, 6, open(4), 6)) shouldBe None
    Interval2D.intervalAt(0, 0).toCodeLikeString shouldBe "intervalAt(0) x intervalAt(0)"

  test("Int 3D interval adjacency, etc."):
    val now = LocalDate.now
    val d: Interval3D[LocalDate, LocalDate, Int] = intervalTo(now) x intervalFrom(now) x intervalFrom(0)
    d.flipAboutHorizontal shouldBe (intervalTo(now) x intervalFrom(0) x intervalFrom(now))
    d.flipAboutVertical shouldBe (intervalFrom(0) x intervalFrom(now) x intervalTo(now))
    d.flipAboutDepth shouldBe (intervalFrom(now) x intervalTo(now) x intervalFrom(0))

    def interval3d(
      hs: Domain1D[Int],
      he: Domain1D[Int],
      vs: Domain1D[Int],
      ve: Domain1D[Int],
      ds: Domain1D[Int],
      de: Domain1D[Int]
    ) = interval(hs, he) x interval(vs, ve) x interval(ds, de)

    def interval3dFrom(hs: Domain1D[Int], vs: Domain1D[Int], ds: Domain1D[Int]) =
      intervalFrom(hs) x intervalFrom(vs) x intervalFrom(ds)

    def interval3dTo(he: Domain1D[Int], ve: Domain1D[Int], de: Domain1D[Int]) =
      intervalTo(he) x intervalTo(ve) x intervalTo(de)

    interval3d(1, 2, 3, 4, 5, 6).toString shouldBe "{[1, 2], [3, 4], [5, 6]}"

    interval3d(1, 2, 3, 4, 5, 6) intersectionWith interval3d(2, 3, 4, 5, 6, 7) shouldBe Some(
      interval3d(2, 2, 4, 4, 6, 6)
    )
    interval3d(1, 2, 3, 4, 5, 6) ∩ interval3d(2, 3, 4, 5, 6, 7) shouldBe Some(interval3d(2, 2, 4, 4, 6, 6))
    assert(interval3d(1, 2, 3, 4, 5, 6) contains interval3d(2, 2, 4, 4, 6, 6))
    assert(interval3d(2, 2, 4, 4, 6, 6) isSubsetOf interval3d(1, 2, 3, 4, 5, 6))
    assert(interval3d(2, 2, 4, 4, 6, 6) ⊆ interval3d(1, 2, 3, 4, 5, 6))

    assert(interval3d(1, 2, 3, 4, 5, 6) isLeftAdjacentTo interval3d(open(2), 4, 3, 4, 5, 6))
    assert(interval3d(1, 2, 3, 4, 5, 6) isAdjacentTo interval3d(open(2), 4, 3, 4, 5, 6))
    assert(!(interval3d(open(2), 4, 3, 4, 3, 4) isLeftAdjacentTo interval3d(1, 2, 3, 4, 5, 6)))
    assert(!(interval3d(1, 3, 3, 4, 5, 6) isLeftAdjacentTo interval3d(2, 4, 3, 4, 5, 6)))
    assert(!(interval3d(1, 3, 3, 4, 5, 6) isLeftAdjacentTo interval3d(3, 4, 3, 4, 5, 6)))
    assert(interval3dTo(2, 2, 2) isLeftAdjacentTo (intervalFromAfter(2) x intervalTo(2) x intervalTo(2)))
    assert(!(interval3dFrom(2, 2, 2) isLeftAdjacentTo interval3dTo(3, 3, 3)))

    assert(interval3d(3, 4, 1, 2, 5, 6) isRightAdjacentTo interval3d(1, open(3), 1, 2, 5, 6))
    assert(interval3d(3, 4, 1, 2, 5, 6) isAdjacentTo interval3d(1, open(3), 1, 2, 5, 6))
    assert(!(interval3d(1, open(3), 1, 2, 5, 6) isRightAdjacentTo interval3d(3, 4, 1, 2, 5, 6)))
    assert(!(interval3d(2, 4, 1, 2, 5, 6) isRightAdjacentTo interval3d(1, 3, 1, 2, 5, 6)))
    assert(!(interval3d(3, 4, 1, 2, 5, 6) isRightAdjacentTo interval3d(1, 3, 1, 2, 5, 6)))
    assert(interval3dFrom(3, 3, 3) isRightAdjacentTo (intervalToBefore(3) x intervalFrom(3) x intervalFrom(3)))
    assert(!(interval3dTo(3, 3, 3) isRightAdjacentTo interval3dFrom(open(3), open(3), open(3))))

    assert(interval3d(3, 4, 1, 2, 5, 6) isLowerAdjacentTo interval3d(3, 4, open(2), 4, 5, 6))
    assert(interval3d(3, 4, 1, 2, 5, 6) isAdjacentTo interval3d(3, 4, open(2), 4, 5, 6))
    assert(!(interval3d(3, 4, 3, 4, 5, 6) isLowerAdjacentTo interval3d(3, 4, 1, open(4), 5, 6)))
    assert(!(interval3d(3, 4, 1, 3, 5, 6) isLowerAdjacentTo interval3d(3, 4, 2, 4, 5, 6)))
    assert(!(interval3d(3, 4, 1, 3, 5, 6) isLowerAdjacentTo interval3d(3, 4, 3, 4, 5, 6)))
    assert(interval3dTo(2, 2, 2) isLowerAdjacentTo (intervalTo(2) x intervalFromAfter(2) x intervalTo(2)))
    assert(!(interval3dFrom(2, 2, 2) isLowerAdjacentTo interval3dTo(3, 3, 3)))

    assert(interval3d(1, 2, open(2), 4, 5, 6) isUpperAdjacentTo interval3d(1, 2, 1, 2, 5, 6))
    assert(interval3d(1, 2, open(2), 4, 5, 6) isAdjacentTo interval3d(1, 2, 1, 2, 5, 6))
    assert(!(interval3d(1, 2, 1, 2, 5, 6) isUpperAdjacentTo interval3d(1, 2, open(2), 4, 5, 6)))
    assert(!(interval3d(1, 2, 2, 4, 5, 6) isUpperAdjacentTo interval3d(1, 2, 1, 3, 5, 6)))
    assert(!(interval3d(1, 2, 3, 4, 5, 6) isUpperAdjacentTo interval3d(1, 2, 1, 3, 5, 6)))
    assert(interval3dFrom(3, 3, 3) isUpperAdjacentTo (intervalFrom(3) x intervalToBefore(3) x intervalFrom(3)))
    assert(!(interval3dTo(3, 3, 3) isUpperAdjacentTo interval3dFrom(2, 2, 2)))

    assert(interval3d(3, 4, 1, 2, 5, 6) isBackAdjacentTo interval3d(3, 4, 1, 2, open(6), 8))
    assert(interval3d(3, 4, 1, 2, 5, 6) isAdjacentTo interval3d(3, 4, 1, 2, open(6), 8))
    assert(!(interval3d(3, 4, 3, 4, open(4), 6) isBackAdjacentTo interval3d(3, 4, 3, 4, 3, 4)))
    assert(!(interval3d(3, 4, 1, 3, open(4), 6) isBackAdjacentTo interval3d(3, 4, 3, 4, 3, 4)))
    assert(interval3dTo(2, 2, 2) isBackAdjacentTo (intervalTo(2) x intervalTo(2) x intervalFromAfter(2)))
    assert(!(interval3dFrom(2, 2, 2) isBackAdjacentTo interval3dTo(3, 3, 3)))

    assert(interval3d(3, 4, 1, 2, open(4), 6) isFrontAdjacentTo interval3d(3, 4, 1, 2, 3, 4))
    assert(interval3d(3, 4, 1, 2, open(4), 6) isAdjacentTo interval3d(3, 4, 1, 2, 3, 4))
    assert(!(interval3d(3, 4, 3, 4, 5, 6) isFrontAdjacentTo interval3d(3, 4, 3, 4, open(6), 8)))
    assert(!(interval3d(3, 4, 1, 3, 5, 6) isFrontAdjacentTo interval3d(3, 4, 3, 4, 3, open(5))))
    assert(interval3dFrom(2, 2, 2) isFrontAdjacentTo (intervalFrom(2) x intervalFrom(2) x intervalToBefore(2)))
    assert(!(interval3dFrom(2, 2, 2) isFrontAdjacentTo interval3dTo(3, 3, 3)))

    interval3d(1, 2, 3, 4, 5, 6).after shouldBe Some(interval3dFrom(open(2), open(4), open(6)))
    interval3d(1, 2, 3, 4, 5, 6).before shouldBe Some(interval3dTo(open(1), open(3), open(5)))
    interval3dFrom(2, 2, 2).after shouldBe None
    interval3dTo(2, 2, 2).before shouldBe None
    interval3d(1, 2, 1, 2, 1, 2).points.toList shouldBe List.empty

    assert(interval3d(1, 2, 3, 4, 5, 6) hasSameStartAs interval3dFrom(1, 3, 5))
    assert(interval3d(1, 2, 3, 4, 5, 6) hasSameEndAs interval3dTo(2, 4, 6))
    (intervalFrom(0) x intervalTo(0) x intervalAt(0)).toCodeLikeString shouldBe
      "intervalFrom(0) x intervalTo(0) x intervalAt(0)"

    assertResult(interval3d(0, 2, 3, 4, 5, 6))(interval3d(1, 2, 3, 4, 5, 6).withHorizontalUpdate(_.from(0)))
    assertResult(interval3d(1, 2, 0, 4, 5, 6))(interval3d(1, 2, 3, 4, 5, 6).withVerticalUpdate(_.from(0)))
    assertResult(interval3d(1, 2, 3, 4, 0, 6))(interval3d(1, 2, 3, 4, 5, 6).withDepthUpdate(_.from(0)))

    interval3d(1, 2, 3, 4, 5, 6).fromBottom shouldBe interval3dTo(2, 4, 6)
    interval3d(1, 2, 3, 4, 5, 6).toTop shouldBe interval3dFrom(1, 3, 5)
    interval3d(1, 2, 3, 4, 5, 6).from(0, 0, 0) shouldBe interval3d(0, 2, 0, 4, 0, 6)
    interval3d(1, 2, 3, 4, 5, 6).fromAfter(1, 1, 1) shouldBe interval3d(open(1), 2, open(1), 4, open(1), 6)
    interval3d(1, 2, 3, 4, 5, 6).to(5, 5, 5) shouldBe interval3d(1, 5, 3, 5, 5, 5)
    interval3d(1, 2, 3, 4, 5, 6).toBefore(7, 7, 7) shouldBe interval3d(1, open(7), 3, open(7), 5, open(7))
    interval3d(1, 2, 3, 4, 5, 6).atStart shouldBe interval3d(1, 1, 3, 3, 5, 5)
    interval3d(1, 2, 3, 4, 5, 6).atEnd shouldBe interval3d(2, 2, 4, 4, 6, 6)
    interval3d(1, 2, 3, 4, 5, 6).gapWith(interval3d(open(5), 6, open(7), 8, open(9), 10)) shouldBe
      Some(interval3d(open(2), 5, open(4), 7, open(6), 9))
    // even though neither adjacent nor intersecting in 3D, no gap in _all_ dimensions
    interval3d(1, 2, 3, 4, 5, 6).gapWith(interval3d(5, 6, open(4), 6, 8, 9)) shouldBe None

  test("Int interval intersections"):
    assert(!(interval(3, 4) intersects interval(1, 2)))
    assert(interval(1, 3) intersects interval(2, 4))
    assert(interval(1, 3) intersects interval(3, 4))
    assert(!(intervalTo(2) intersects intervalFrom(3)))
    assert(intervalFrom(2) intersects intervalTo(3))
    assert(intervalFrom(2) intersects intervalFrom(3))
    assert(intervalTo(2) intersects intervalTo(3))

    (interval(1, 3) intersectionWith interval(2, 4)) shouldBe Some(interval(2, 3))
    (interval(1, 3) ∩ interval(2, 4)) shouldBe Some(interval(2, 3))
    (intervalTo(3) ∩ intervalFrom(2)) shouldBe Some(interval(2, 3))
    (interval(1, 3) ∩ interval(3, 4)) shouldBe Some(intervalAt(3))
    (intervalTo(2) ∩ intervalFrom(3)) shouldBe None

  test("Int interval gaps and spans"):
    (intervalTo(4) gapWith intervalFrom(8)) shouldBe Some(intervalFromAfter(4).toBefore(8))
    (intervalFrom(3).toBefore(4) gapWith intervalFromAfter(7).to(8)) shouldBe Some(interval(4, 7))
    (intervalTo(4) gapWith intervalFromAfter(4)) shouldBe None // adjacent
    (intervalFrom(4) gapWith intervalTo(8)) shouldBe None // intersects

    (intervalTo(4) joinedWith intervalAt(8)) shouldBe intervalTo(8)
    (intervalTo(4) ∪ interval(5, 8)) shouldBe intervalTo(8)
    (interval(3, 4) ∪ interval(7, 8)) shouldBe interval(3, 8)

  test("Int interval exclusions"):
    import Interval1D.Remainder
    (intervalTo(4) excluding intervalFrom(5)) shouldBe Remainder.Single(intervalTo(4))
    (intervalTo(4) \ intervalFrom(1)) shouldBe Remainder.Single(intervalToBefore(1))
    (intervalTo(4) \ interval(1, 2)) shouldBe Remainder.Split(intervalToBefore(1), intervalFromAfter(2).to(4))
    (intervalTo(4) \ interval(1, 4)) shouldBe Remainder.Single(intervalToBefore(1))
    (intervalTo(4) \ intervalTo(0)) shouldBe Remainder.Single(intervalFromAfter(0).to(4))
    (intervalTo(4) \ intervalTo(4)) shouldBe Remainder.None

    (intervalTo(4) separateUsing intervalFrom(5)) shouldBe Seq(intervalTo(4))
    (intervalTo(4) separateUsing intervalFrom(1)) shouldBe Seq(intervalToBefore(1), intervalFrom(1).to(4))
    (intervalTo(4) separateUsing interval(1, 2)) shouldBe Seq(
      intervalToBefore(1),
      interval(1, 2),
      intervalFromAfter(2).to(4)
    )
    (intervalTo(4) separateUsing interval(1, 4)) shouldBe Seq(intervalToBefore(1), interval(1, 4))
    (intervalTo(4) separateUsing intervalTo(0)) shouldBe Seq(intervalTo(0), intervalFromAfter(0).to(4))
    (intervalTo(4) separateUsing intervalTo(4)) shouldBe Seq(intervalTo(4))

  test("Int interval contains, etc."):
    assert(intervalTo(5) contains 3)
    assert(!(intervalTo(5) contains open(3)))
    assert(Point(3) ∈ intervalTo(5))
    assert(!intervalFrom(5).contains(3))
    assert(interval(3, 5).contains(3))
    assert(!interval(3, 5).contains(interval(4, 6)))
    assert(interval(4, 5) isSubsetOf interval(3, 6))
    assert(interval(4, 5) ⊆ interval(3, 6))
    assert(!interval(3, 5).contains(Bottom))
    assert(Bottom belongsTo unbounded[Int])
    assert(Top ∈ unbounded[Int])

    assert((Point(3) x Point(5)) ∈ (intervalTo(5) x intervalTo(7)))
    assert((Point(3) x Point(5)) belongsTo (intervalTo(5) x intervalTo(7)))
    assert((Point(3) x Point(4) x Point(5)) ∈ (intervalTo(5) x intervalTo(6) x intervalTo(7)))
    assert((Point(3) x Point(4) x Point(5)) belongsTo (intervalTo(5) x intervalTo(6) x intervalTo(7)))

    interval(3, 5).fromBottom shouldBe intervalTo(5)
    interval(3, 5).toTop shouldBe intervalFrom(3)
    interval(3, 5).from(4) shouldBe interval(4, 5)
    interval(3, 5).fromAfter(4) shouldBe intervalFromAfter(4).to(5)
    interval(3, 5).from(5) shouldBe intervalAt(5)
    interval(3, 5).to(4) shouldBe interval(3, 4)
    interval(3, 5).toBefore(4) shouldBe intervalFrom(3).toBefore(4)
    interval(3, 5).to(3) shouldBe intervalAt(3)
    interval(3, 5).atStart shouldBe intervalAt(3)
    interval(3, 5).atEnd shouldBe intervalAt(5)

    (intervalFrom(0) x intervalTo(0) x unbounded[Int]).toCodeLikeString shouldBe
      "intervalFrom(0) x intervalTo(0) x unbounded"

  test("Int interval collection operations"):
    import Interval1D.*

    val intervalsUnsorted1 = List(
      intervalFrom(1).toBefore(11),
      intervalToBefore(1),
      intervalFromAfter(20),
      interval(11, 20)
    )
    val intervals1 = intervalsUnsorted1.sorted

    assert(isDisjoint(intervals1))
    assert(!isCompressible[Int](Nil))
    assert(isCompressible(intervals1))
    assert(
      isCompressible(
        List(
          interval(1, 15),
          interval(11, 20) // overlap
        )
      )
    )
    compress(intervals1) shouldBe List(unbounded[Int])
    compress(intervals1.filterNot(_.start equiv 1)) shouldBe List(intervalToBefore(1), intervalFrom(11))
    assert(isDisjoint(intervals1.filterNot(_.start equiv 1)))
    assert(!isDisjoint((intervalsUnsorted1 :+ interval(5, 15)).sorted))

    uniqueIntervals[Int](Nil) shouldBe Nil

    // | -∞ ........... 5 |        |        | 10 ........................ 20 |          | 30 .. +∞ |
    //           | 3 ........... 7 |                   | 14 .. 16 |
    val intervals2 = List(intervalTo(5), interval(3, 7), interval(10, 20), interval(14, 16), intervalFrom(30))
    val intervals3 = compress(intervals2)
    intervals3.toList shouldBe List(intervalTo(7), interval(10, 20), intervalFrom(30))
    complement(intervals3) shouldBe List(intervalFromAfter(7).toBefore(10), intervalFromAfter(20).toBefore(30))
    complement(intervals3.dropRight(1)) shouldBe List(intervalFromAfter(7).toBefore(10), intervalFromAfter(20))
    complement(intervals3.drop(1)) shouldBe List(intervalToBefore(10), intervalFromAfter(20).toBefore(30))

    // | -∞ .. 2 | 3 .. 5 | 6 .. 7 | 8 .. 9 | 10 .. 13 | 14 .. 16 | 17 .. 20 | 21 .. 29 | 30 .. +∞ |
    val expected2 = List(
      intervalToBefore(3),
      interval(3, 5),
      intervalFromAfter(5).to(7),
      intervalFromAfter(7).toBefore(10),
      intervalFrom(10).toBefore(14),
      interval(14, 16),
      intervalFromAfter(16).to(20),
      intervalFromAfter(20).toBefore(30),
      intervalFrom(30)
    )
    uniqueIntervals(intervals2) shouldBe expected2
    complement(expected2) shouldBe Nil

  test("Int interval 2D collection operations"):
    import Interval2D.*

    val intervalsUnsorted1 = List(
      intervalFrom(1).toBefore(11),
      intervalToBefore(1),
      intervalFromAfter(20),
      interval(11, 20)
    )
    val intervals1 = intervalsUnsorted1.sorted

    assert(!isCompressible[Int, Int](Nil))
    assert(isCompressible(intervals1.map(_ x Interval1D.unbounded[Int])))
    assert(isCompressible(intervals1.map(Interval1D.unbounded[Int] x _)))
    assert(
      isCompressible(
        List(
          interval(1, 15) x Interval1D.unbounded[Int],
          interval(11, 20) x Interval1D.unbounded[Int] // overlap
        )
      )
    )
    assert(
      isCompressible(
        List(
          Interval1D.unbounded[Int] x interval(1, 15),
          Interval1D.unbounded[Int] x interval(11, 20) // overlap
        )
      )
    )

    uniqueIntervals[Int, Int](Nil) shouldBe Nil

    // | -∞ ........... 5 |        |        | 10 ........................ 20 |          | 30 .. +∞ |
    //           | 3 ........... 7 |                   | 14 .. 16 |
    val intervals2 = List(
      intervalTo(0) x intervalTo(5),
      Interval1D.unbounded[Int] x interval(10, 20),
      Interval1D.unbounded[Int] x intervalFrom(30),
      intervalFromAfter(0) x interval(3, 7)
    )
    val intervals3 = compress(intervals2)
    intervals3.toList shouldBe List(
      intervalTo(0) x intervalTo(5),
      Interval1D.unbounded[Int] x interval(10, 20),
      Interval1D.unbounded[Int] x intervalFrom(30),
      intervalFromAfter(0) x interval(3, 7)
    )

    complement(intervals3).toList shouldBe List(
      intervalTo(0) x intervalFromAfter(5).toBefore(10),
      Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      intervalFromAfter(0) x intervalToBefore(3),
      intervalFromAfter(0) x intervalFromAfter(7).toBefore(10)
    )
    complement(intervals3.dropRight(1)).toList shouldBe List(
      Interval1D.unbounded[Int] x intervalFromAfter(5).toBefore(10),
      Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      intervalFromAfter(0) x intervalTo(5)
    )
    complement(intervals3.drop(1)).toList shouldBe List(
      Interval1D.unbounded[Int] x intervalToBefore(3),
      intervalTo(0) x intervalFrom(3).toBefore(10),
      Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      intervalFromAfter(0) x intervalFromAfter(7).toBefore(10)
    )

  test("Int interval 3D collection operations"):
    import Interval3D.*

    val intervalsUnsorted1 = List(
      intervalFrom(1).toBefore(11),
      intervalToBefore(1),
      intervalFromAfter(20),
      interval(11, 20)
    )
    val intervals1 = intervalsUnsorted1.sorted

    assert(!isCompressible[Int, Int, Int](Nil))
    assert(isCompressible(intervals1.map(_ x Interval1D.unbounded[Int] x Interval1D.unbounded[Int])))
    assert(isCompressible(intervals1.map(Interval1D.unbounded[Int] x _ x Interval1D.unbounded[Int])))
    assert(isCompressible(intervals1.map(Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x _)))
    assert(
      isCompressible(
        List(
          interval(1, 15) x Interval1D.unbounded[Int] x Interval1D.unbounded[Int],
          interval(11, 20) x Interval1D.unbounded[Int] x Interval1D.unbounded[Int] // overlap
        )
      )
    )
    assert(
      isCompressible(
        List(
          Interval1D.unbounded[Int] x interval(1, 15) x Interval1D.unbounded[Int],
          Interval1D.unbounded[Int] x interval(11, 20) x Interval1D.unbounded[Int] // overlap
        )
      )
    )
    assert(
      isCompressible(
        List(
          Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x interval(1, 15),
          Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x interval(11, 20) // overlap
        )
      )
    )

    uniqueIntervals[Int, Int, Int](Nil) shouldBe Nil

    // | -∞ ........... 5 |        |        | 10 ........................ 20 |          | 30 .. +∞ |
    //           | 3 ........... 7 |                   | 14 .. 16 |
    val intervals2 = List(
      Interval1D.unbounded[Int] x intervalTo(0) x intervalTo(5),
      intervalTo(0) x Interval1D.unbounded[Int] x interval(10, 20),
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFrom(30),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x interval(3, 7)
    )
    val intervals3 = compress(intervals2)
    intervals3.toList shouldBe List(
      Interval1D.unbounded[Int] x intervalTo(0) x intervalTo(5),
      intervalTo(0) x Interval1D.unbounded[Int] x interval(10, 20),
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFrom(30),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x interval(3, 7)
    )

    complement(intervals3).toList shouldBe List(
      Interval1D.unbounded[Int] x intervalTo(0) x intervalFromAfter(5).toBefore(10),
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x intervalToBefore(3),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x intervalFromAfter(7).toBefore(10),
      intervalFromAfter(0) x Interval1D.unbounded[Int] x interval(10, 20)
    )

    complement(intervals3.dropRight(1)).toList shouldBe List(
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFromAfter(5).toBefore(10),
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x intervalTo(5),
      intervalFromAfter(0) x Interval1D.unbounded[Int] x interval(10, 20)
    )
    complement(intervals3.drop(1)).toList shouldBe List(
      Interval1D.unbounded[Int] x intervalTo(0) x intervalToBefore(10),
      Interval1D.unbounded[Int] x Interval1D.unbounded[Int] x intervalFromAfter(20).toBefore(30),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x intervalToBefore(3),
      Interval1D.unbounded[Int] x intervalFromAfter(0) x intervalFromAfter(7).toBefore(10),
      intervalFromAfter(0) x Interval1D.unbounded[Int] x interval(10, 20)
    )
