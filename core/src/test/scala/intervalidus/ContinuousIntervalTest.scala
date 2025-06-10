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
    interval(1, 3).points.toList shouldBe List.empty
    interval2d(1, 3, 1, 3).points.toList shouldBe List.empty
    interval3d(1, 2, 1, 2, 1, 2).points.toList shouldBe List.empty
    interval4d(1, 2, 1, 2, 1, 2, 1, 2).points.toList shouldBe List.empty

    // Other
    assert(!(intervalTo(5) contains open(3)))
    intervalFromAfter(LocalDate.of(2025, 1, 10).atTime(1, 2, 3, 4)).toCodeLikeString shouldBe
      "intervalFromAfter(LocalDate.of(2025,1,10).atTime(1,2,3,4))"
    intervalFromAfter(0).toBefore(1).toCodeLikeString shouldBe "interval(open(0), open(1))"
    intervalAt(open(LocalDate.of(2025, 1, 10))).toCodeLikeString shouldBe "intervalAt(LocalDate.of(2025,1,10))"
