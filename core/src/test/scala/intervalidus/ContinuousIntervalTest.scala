package intervalidus

import intervalidus.ContinuousValue.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class ContinuousIntervalTest extends AnyFunSuite with Matchers with IntervalCommonBehaviors:

  import Domain1D.open
  import Interval1D.*

  testsFor(commonBehaviors("Continuous"))

  test("Continuous: Int continuous-specific interval behaviors"):
    // String representation
    interval2d(1, open(2), open(3), 4).toString shouldBe "{[1, 2), (3, 4]}"
    interval3d(1, 2, open(3), open(4), 5, 6).toString shouldBe "{[1, 2], (3, 4), [5, 6]}"
    interval4d(1, 2, 3, 4, 5, 6).toString shouldBe "{[1, 2], [3, 4], [5, 6], (-∞, +∞)}"

    // Points
    assert(interval(1, 3).points.isEmpty)
    assert(interval2d(1, 3, 1, 3).points.isEmpty)
    assert(interval3d(1, 2, 1, 2, 1, 2).points.isEmpty)
    assert(interval4d(1, 2, 1, 2, 1, 2, 1, 2).points.isEmpty)

    // Other
    assert(!(intervalTo(5) contains open(3)))
    intervalFromAfter(LocalDate.of(2025, 1, 10).atTime(1, 2, 3, 4)).toCodeLikeString shouldBe
      "intervalFromAfter(LocalDate.of(2025,1,10).atTime(1,2,3,4))"
    intervalFromAfter(0).toBefore(1).toCodeLikeString shouldBe "interval(open(0), open(1))"
    intervalAt(open(LocalDate.of(2025, 1, 10))).toCodeLikeString shouldBe "intervalAt(LocalDate.of(2025,1,10))"

    val originClosed = Domain.in2D[Int, Int](0, 0)
    val originOpen = Domain.in2D[Int, Int](open(0), open(0))

    assertThrows[IllegalArgumentException]:
      val _ = Interval[Domain.In2D[Int, Int]](originClosed, originOpen) // empty interval

    assertThrows[IllegalArgumentException]:
      val _ = Interval[Domain.In2D[Int, Int]](originOpen, originClosed) // empty interval

    assertThrows[IllegalArgumentException]:
      val _ = Interval[Domain.In2D[Int, Int]](originOpen, originOpen) // empty interval
