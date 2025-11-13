package intervalidus

import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DiscreteIntervalTest extends AnyFunSuite with Matchers with IntervalCommonBehaviors:

  import Domain1D.domain
  import Interval1D.*

  testsFor(commonBehaviors("Discrete"))

  test("Discrete: Int discrete-specific interval behaviors"):
    // String representation
    interval2d(1, 2, 3, 4).toString shouldBe "{[1..2], [3..4]}"
    interval3d(1, 2, 3, 4, 5, 6).toString shouldBe "{[1..2], [3..4], [5..6]}"
    interval4d(1, 2, 3, 4, 5, 6).toString shouldBe "{[1..2], [3..4], [5..6], (-∞..+∞)}"

    // Points
    intervalFrom(Int.MaxValue - 1).points shouldBe Iterable(Int.MaxValue - 1, Int.MaxValue).map(domain(_))
    intervalTo(Int.MinValue + 1).points shouldBe Iterable(Int.MinValue, Int.MinValue + 1).map(domain(_))
    interval(1, 3).points shouldBe Iterable(1, 2, 3).map(domain(_))
    interval2d(1, 3, 1, 3).points shouldBe Iterable(
      (1, 1),
      (1, 2),
      (1, 3),
      (2, 1),
      (2, 2),
      (2, 3),
      (3, 1),
      (3, 2),
      (3, 3)
    ).map((t1, t2) => domain(t1) x domain(t2))
    interval3d(1, 2, 1, 2, 1, 2).points shouldBe Iterable(
      (1, 1, 1),
      (1, 1, 2),
      (1, 2, 1),
      (1, 2, 2),
      (2, 1, 1),
      (2, 1, 2),
      (2, 2, 1),
      (2, 2, 2)
    ).map((t1, t2, t3) => domain(t1) x domain(t2) x domain(t3))
    interval4d(1, 2, 1, 2, 1, 2, 1, 2).points shouldBe Iterable(
      (1, 1, 1, 1),
      (1, 1, 1, 2),
      (1, 1, 2, 1),
      (1, 1, 2, 2),
      (1, 2, 1, 1),
      (1, 2, 1, 2),
      (1, 2, 2, 1),
      (1, 2, 2, 2),
      (2, 1, 1, 1),
      (2, 1, 1, 2),
      (2, 1, 2, 1),
      (2, 1, 2, 2),
      (2, 2, 1, 1),
      (2, 2, 1, 2),
      (2, 2, 2, 1),
      (2, 2, 2, 2)
    ).map((t1, t2, t3, t4) => domain(t1) x domain(t2) x domain(t3) x domain(t4))
