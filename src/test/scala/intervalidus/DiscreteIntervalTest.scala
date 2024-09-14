package intervalidus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

class DiscreteIntervalTest extends AnyFunSuite with Matchers:

  import DiscreteDomain1D.{Bottom, Point, Top}
  import DiscreteInterval1D.*

  test("Int interval validity"):
    assertThrows[IllegalArgumentException]:
      val endBeforeStart = interval(2, 1)

    assertThrows[IllegalArgumentException]:
      val endBeforeStart = DiscreteInterval1D[Int](Top, Bottom)

  test("Int interval adjacency, etc."):
    assert(interval(1, 2) isLeftAdjacentTo interval(3, 4))
    assert(!(interval(3, 4) isLeftAdjacentTo interval(1, 2)))
    assert(!(interval(1, 3) isLeftAdjacentTo interval(2, 4)))
    assert(!(interval(1, 3) isLeftAdjacentTo interval(3, 4)))
    assert(intervalTo(2) isLeftAdjacentTo intervalFrom(3))
    assert(!(intervalFrom(2) isLeftAdjacentTo intervalTo(3)))

    assert(interval(3, 4) isRightAdjacentTo interval(1, 2))
    assert(!(interval(1, 2) isRightAdjacentTo interval(3, 4)))
    assert(!(interval(2, 4) isRightAdjacentTo interval(1, 3)))
    assert(!(interval(3, 4) isRightAdjacentTo interval(1, 3)))
    assert(intervalFrom(3) isRightAdjacentTo intervalTo(2))
    assert(!(intervalTo(3) isRightAdjacentTo intervalFrom(2)))

    assert(interval(1, 2) isAdjacentTo interval(3, 4))
    assert(interval(3, 4) isAdjacentTo interval(1, 2))
    assert(intervalTo(2) isAdjacentTo intervalFrom(3))
    assert(intervalFrom(3) isAdjacentTo intervalTo(2))
    assert(!(intervalFrom(2) isAdjacentTo intervalTo(3)))
    assert(!(intervalTo(3) isAdjacentTo intervalFrom(2)))

    interval(1, 2).after shouldBe Some(intervalFrom(3))
    interval(1, 2).before shouldBe Some(intervalTo(0))
    intervalFrom(2).after shouldBe None
    intervalTo(2).before shouldBe None
    interval(1, 3).points.toList shouldBe List(1, 2, 3).map(Point(_))
    intervalFrom(Int.MaxValue - 1).points.toList shouldBe List(Int.MaxValue - 1, Int.MaxValue).map(Point(_))
    intervalTo(Int.MinValue + 1).points.toList shouldBe List(Int.MinValue, Int.MinValue + 1).map(Point(_))

    intervalFrom(0).toCodeLikeString shouldBe "intervalFrom(0)"
    intervalTo(0).toCodeLikeString shouldBe "intervalTo(0)"
    intervalAt(0).toCodeLikeString shouldBe "intervalAt(0)"
    interval(0, 1).toCodeLikeString shouldBe "interval(0, 1)"
    unbounded[Int].toCodeLikeString shouldBe "unbounded"

  test("Int 2D interval adjacency, etc."):
    val now = LocalDate.now
    val d: DiscreteInterval2D[LocalDate, Int] = intervalTo(now) x intervalFrom(0)
    d.flip shouldBe (intervalFrom(0) x intervalTo(now))

    def interval2d(hs: Int, he: Int, vs: Int, ve: Int) = interval(hs, he) x interval(vs, ve)

    def interval2dFrom(hs: Int, vs: Int) = intervalFrom(hs) x intervalFrom(vs)

    def interval2dTo(he: Int, ve: Int) = intervalTo(he) x intervalTo(ve)

    interval2d(1, 2, 3, 4).toString shouldBe "{[1..2], [3..4]}"

    interval2d(1, 2, 3, 4) intersectionWith interval2d(2, 3, 4, 5) shouldBe Some(interval2d(2, 2, 4, 4))
    interval2d(1, 2, 3, 4) ∩ interval2d(2, 3, 4, 5) shouldBe Some(interval2d(2, 2, 4, 4))
    assert(interval2d(1, 2, 3, 4) contains interval2d(2, 2, 4, 4))
    assert(interval2d(2, 2, 4, 4) isSubsetOf interval2d(1, 2, 3, 4))
    assert(interval2d(2, 2, 4, 4) ⊆ interval2d(1, 2, 3, 4))

    assert(interval2d(1, 2, 3, 4) isLeftAdjacentTo interval2d(3, 4, 3, 4))
    assert(interval2d(1, 2, 3, 4) isAdjacentTo interval2d(3, 4, 3, 4))
    assert(!(interval2d(3, 4, 3, 4) isLeftAdjacentTo interval2d(1, 2, 3, 4)))
    assert(!(interval2d(1, 3, 3, 4) isLeftAdjacentTo interval2d(2, 4, 3, 4)))
    assert(!(interval2d(1, 3, 3, 4) isLeftAdjacentTo interval2d(3, 4, 3, 4)))
    assert(interval2dTo(2, 2) isLeftAdjacentTo (intervalFrom(3) x intervalTo(2)))
    assert(!(interval2dFrom(2, 2) isLeftAdjacentTo interval2dTo(3, 3)))

    assert(interval2d(3, 4, 1, 2) isRightAdjacentTo interval2d(1, 2, 1, 2))
    assert(interval2d(3, 4, 1, 2) isAdjacentTo interval2d(1, 2, 1, 2))
    assert(!(interval2d(1, 2, 1, 2) isRightAdjacentTo interval2d(3, 4, 1, 2)))
    assert(!(interval2d(2, 4, 1, 2) isRightAdjacentTo interval2d(1, 3, 1, 2)))
    assert(!(interval2d(3, 4, 1, 2) isRightAdjacentTo interval2d(1, 3, 1, 2)))
    assert(interval2dFrom(3, 3) isRightAdjacentTo (intervalTo(2) x intervalFrom(3)))
    assert(!(interval2dTo(3, 3) isRightAdjacentTo interval2dFrom(2, 2)))

    assert(interval2d(3, 4, 1, 2) isLowerAdjacentTo interval2d(3, 4, 3, 4))
    assert(interval2d(3, 4, 1, 2) isAdjacentTo interval2d(3, 4, 3, 4))
    assert(!(interval2d(3, 4, 3, 4) isLowerAdjacentTo interval2d(3, 4, 1, 2)))
    assert(!(interval2d(3, 4, 1, 3) isLowerAdjacentTo interval2d(3, 4, 2, 4)))
    assert(!(interval2d(3, 4, 1, 3) isLowerAdjacentTo interval2d(3, 4, 3, 4)))
    assert(interval2dTo(2, 2) isLowerAdjacentTo (intervalTo(2) x intervalFrom(3)))
    assert(!(interval2dFrom(2, 2) isLowerAdjacentTo interval2dTo(3, 3)))

    assert(interval2d(1, 2, 3, 4) isUpperAdjacentTo interval2d(1, 2, 1, 2))
    assert(interval2d(1, 2, 3, 4) isAdjacentTo interval2d(1, 2, 1, 2))
    assert(!(interval2d(1, 2, 1, 2) isUpperAdjacentTo interval2d(1, 2, 3, 4)))
    assert(!(interval2d(1, 2, 2, 4) isUpperAdjacentTo interval2d(1, 2, 1, 3)))
    assert(!(interval2d(1, 2, 3, 4) isUpperAdjacentTo interval2d(1, 2, 1, 3)))
    assert(interval2dFrom(3, 3) isUpperAdjacentTo (intervalFrom(3) x intervalTo(2)))
    assert(!(interval2dTo(3, 3) isUpperAdjacentTo interval2dFrom(2, 2)))

    interval2d(1, 2, 3, 4).after shouldBe Some(interval2dFrom(3, 5))
    interval2d(1, 2, 3, 4).before shouldBe Some(interval2dTo(0, 2))
    interval2dFrom(2, 2).after shouldBe None
    interval2dTo(2, 2).before shouldBe None
    interval2d(1, 3, 1, 3).points.toList shouldBe List(
      (1, 1),
      (1, 2),
      (1, 3),
      (2, 1),
      (2, 2),
      (2, 3),
      (3, 1),
      (3, 2),
      (3, 3)
    ).map((t1, t2) => Point(t1) x Point(t2))

    assert(interval2d(1, 2, 3, 4) hasSameStartAs interval2dFrom(1, 3))
    assert(interval2d(1, 2, 3, 4) hasSameEndAs interval2dTo(2, 4))
    (intervalFrom(0) x intervalTo(0)).toCodeLikeString shouldBe
      "intervalFrom(0) x intervalTo(0)"

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
    (intervalTo(4) gapWith intervalFrom(8)) shouldBe Some(interval(5, 7))
    (interval(3, 4) gapWith interval(7, 8)) shouldBe Some(interval(5, 6))
    (intervalTo(4) gapWith intervalFrom(5)) shouldBe None // adjacent
    (intervalFrom(4) gapWith intervalTo(8)) shouldBe None // intersects

    (intervalTo(4) joinedWith intervalAt(8)) shouldBe intervalTo(8)
    (intervalTo(4) ∪ interval(5, 8)) shouldBe intervalTo(8)
    (interval(3, 4) ∪ interval(7, 8)) shouldBe interval(3, 8)

  test("Int interval exclusions"):
    import DiscreteInterval1D.Remainder
    (intervalTo(4) excluding intervalFrom(5)) shouldBe Remainder.Single(intervalTo(4))
    (intervalTo(4) \ intervalFrom(1)) shouldBe Remainder.Single(intervalTo(0))
    (intervalTo(4) \ interval(1, 2)) shouldBe Remainder.Split(intervalTo(0), interval(3, 4))
    (intervalTo(4) \ interval(1, 4)) shouldBe Remainder.Single(intervalTo(0))
    (intervalTo(4) \ intervalTo(0)) shouldBe Remainder.Single(interval(1, 4))
    (intervalTo(4) \ intervalTo(4)) shouldBe Remainder.None

  test("Int interval contains, etc."):
    assert(intervalTo(5) contains 3)
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

    interval(Some(3), Some(5)).fromBottom shouldBe interval(None, Some(5))
    interval(Some(3), Some(5)).fromBottom shouldBe intervalTo(5)
    interval(Some(3), Some(5)).toTop shouldBe interval(Some(3), None)
    interval(3, 5).toTop shouldBe intervalFrom(3)
    interval(3, 5).startingWith(4) shouldBe interval(4, 5)
    interval(3, 5).startingAfter(4) shouldBe intervalAt(5)
    interval(3, 5).endingWith(4) shouldBe interval(3, 4)
    interval(3, 5).endingBefore(4) shouldBe intervalAt(3)
    interval(3, 5).atStart shouldBe intervalAt(3)
    interval(3, 5).atEnd shouldBe intervalAt(5)

  test("Int interval collection operations"):
    import DiscreteInterval1D.*

    val intervalsUnsorted1 = List(
      interval(1, 10),
      intervalTo(0),
      intervalFrom(21),
      interval(11, 20)
    )
    val intervals1 = intervalsUnsorted1.sortBy(_.start)

    sort(intervalsUnsorted1) shouldBe intervals1
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
    compress(intervals1.filterNot(_.start equiv 1)) shouldBe List(intervalTo(0), intervalFrom(11))
    assert(isDisjoint(intervals1.filterNot(_.start equiv 1)))
    assert(!isDisjoint(sort(intervalsUnsorted1 :+ interval(5, 15))))

    uniqueIntervals[Int](Nil) shouldBe Nil

    // | -∞ ........... 5 |        |        | 10 ........................ 20 |          | 30 .. +∞ |
    //           | 3 ........... 7 |                   | 14 .. 16 |
    val intervals2 = List(intervalTo(5), interval(3, 7), interval(10, 20), interval(14, 16), intervalFrom(30))
    val intervals3 = compress(intervals2)
    intervals3.toList shouldBe List(intervalTo(7), interval(10, 20), intervalFrom(30))
    complement(intervals3) shouldBe List(interval(8, 9), interval(21, 29))
    complement(intervals3.dropRight(1)) shouldBe List(interval(8, 9), intervalFrom(21))
    complement(intervals3.drop(1)) shouldBe List(intervalTo(9), interval(21, 29))

    // | -∞ .. 2 | 3 .. 5 | 6 .. 7 | 8 .. 9 | 10 .. 13 | 14 .. 16 | 17 .. 20 | 21 .. 29 | 30 .. +∞ |
    val expected2 = List(
      intervalTo(2),
      interval(3, 5),
      interval(6, 7),
      interval(8, 9),
      interval(10, 13),
      interval(14, 16),
      interval(17, 20),
      interval(21, 29),
      intervalFrom(30)
    )
    uniqueIntervals(intervals2) shouldBe expected2
    complement(expected2) shouldBe Nil

  test("Int interval 2D collection operations"):
    import DiscreteInterval2D.*

    val intervalsUnsorted1 = List(
      interval(1, 10),
      intervalTo(0),
      intervalFrom(21),
      interval(11, 20)
    )
    val intervals1 = intervalsUnsorted1.sortBy(_.start)

    assert(!isCompressible[Int, Int](Nil))
    assert(isCompressible(intervals1.map(_ x DiscreteInterval1D.unbounded[Int])))
    assert(isCompressible(intervals1.map(DiscreteInterval1D.unbounded[Int] x _)))
    assert(
      isCompressible(
        List(
          interval(1, 15) x DiscreteInterval1D.unbounded[Int],
          interval(11, 20) x DiscreteInterval1D.unbounded[Int] // overlap
        )
      )
    )
    assert(
      isCompressible(
        List(
          DiscreteInterval1D.unbounded[Int] x interval(1, 15),
          DiscreteInterval1D.unbounded[Int] x interval(11, 20) // overlap
        )
      )
    )
