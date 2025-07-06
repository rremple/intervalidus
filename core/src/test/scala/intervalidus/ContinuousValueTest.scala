package intervalidus

import intervalidus.*
import intervalidus.DomainLike.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import scala.math.Ordering.Implicits.infixOrderingOps

class ContinuousValueTest extends AnyFunSuite:

  test("Ops on Ints"):
    import intervalidus.ContinuousValue.IntContinuousValue
    assert(3 equiv 3)
    assert(3 <= 3)
    assert(3 >= 3)
    assert(0 < 4)
    assert(4 > 0)

  test("Ops on Int Domain"):
    import intervalidus.ContinuousValue.IntContinuousValue
    import intervalidus.ContinuousValue.IntContinuousValue.{maxValue, minValue}
    import intervalidus.Domain1D.*

    def top: Domain1D[Int] = Top
    def bottom: Domain1D[Int] = Bottom

    assert(domain(3).rightAdjacent equiv open(3))
    assert(domain(3) isRightAdjacentTo open(3))
    assert(domain(3) isLeftAdjacentTo open(3))
    assert(domain(3) isAdjacentTo open(3))
    assert(domain(maxValue).leftAdjacent equiv open(maxValue))
    assert(domain(minValue).rightAdjacent equiv open(minValue))
    assert(bottom.leftAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)
    assert(domain(3) equiv domain(3))
    assert(domain(3) <= domain(3))
    assert(domain(3) >= domain(3))
    assert(domain(0) < domain(4))
    assert(domain(4) > domain(0))
    assert(bottom < domain(4))
    assert(domain(4) < top)
    assert(open(3) equiv open(3))
    assert(open(3) <= open(3))
    assert(open(3) >= open(3))
    assert(open(0) < open(4))
    assert(open(4) > open(0))
    assert(bottom < open(4))
    assert(open(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(domain(3).orderedHash == open(3).orderedHash)
    assert(domain(0).orderedHash <= open(4).orderedHash)
    assert(bottom.orderedHash <= open(4).orderedHash)
    assert(domain(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assertResult("OpenPoint(3) x Bottom")((open(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((domain(3) x Bottom x Top).toCodeLikeString)

  test("Ops on Enum and Enum Domain"):
    // Can't be empty
    assertThrows[IllegalArgumentException]:
      val _ = ContinuousValue.fromSeq(IndexedSeq())

    // Must be distinct
    assertThrows[IllegalArgumentException]:
      val _ = ContinuousValue.fromSeq(IndexedSeq(1, 2, 1))

    enum Color:
      case Red, Yellow, Green, Cyan, Blue, Magenta
    given discreteColor: ContinuousValue[Color] = ContinuousValue.fromSeq(Color.values.toIndexedSeq)

    import Color.*
    import Domain1D.*

    def top: Domain1D[Color] = Top
    def bottom: Domain1D[Color] = Bottom

    // For some reason ScalaTest is intervening here and applying its own comparison and getting the wrong answer.
    // So we wrap these assertions to prevent its tinkering...
    def assertCompare(b: => Boolean): Assertion = assert(b)

    assert(Cyan equiv Cyan)
    assertCompare(Cyan <= Cyan)
    assertCompare(Cyan >= Cyan)
    assertCompare(Red < Blue)
    assertCompare(Blue > Red)

    assert(domain(Cyan).rightAdjacent equiv open(Cyan))
    assert(open(Cyan).leftAdjacent equiv domain(Cyan))
    assert(domain(discreteColor.maxValue).rightAdjacent equiv open(discreteColor.maxValue))
    assert(open(discreteColor.minValue).leftAdjacent equiv domain(discreteColor.minValue))
    assert(bottom.leftAdjacent equiv bottom)
    assert(bottom.rightAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)
    assert(top.rightAdjacent equiv top)
    assert(domain(Cyan) equiv domain(Cyan))

    assertCompare(domain(Cyan) <= domain(Cyan))
    assertCompare(domain(Cyan) >= domain(Cyan))
    assertCompare(domain(Red) < domain(Blue))
    assertCompare(domain(Blue) > domain(Red))
    assertCompare(bottom < domain(Blue))
    assertCompare(domain(Blue) < top)
    assertCompare(bottom < top)
    assertCompare(bottom <= top)
    assertCompare(!(bottom > bottom))
    assertCompare(!(bottom equiv top))

    assertCompare(domain(Cyan).orderedHash == domain(Cyan).orderedHash)
    assertCompare(domain(Red).orderedHash <= domain(Blue).orderedHash)
    assertCompare(bottom.orderedHash <= domain(Blue).orderedHash)
    assertCompare(domain(Blue).orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)
    assertResult("Point(Cyan) x Bottom")((domain(Cyan) x Bottom).toCodeLikeString)
    assertResult("Point(Cyan) x Bottom x Top")((domain(Cyan) x Bottom x Top).toCodeLikeString)

  test("Ops on Longs"):
    import ContinuousValue.LongContinuousValue
    assert(3L equiv 3L)
    assert(3L <= 3L)
    assert(3L >= 3L)
    assert(0L < 4L)
    assert(4L > 0L)

  test("Ops on Long Domain"):
    import ContinuousValue.LongContinuousValue
    import ContinuousValue.LongContinuousValue.{maxValue, minValue}
    import Domain1D.*

    def pointClosed(v: Long): Domain1D[Long] = domain(v)
    def pointOpen(v: Long): Domain1D[Long] = open(v)
    def top: Domain1D[Long] = Top
    def bottom: Domain1D[Long] = Bottom

    assert(pointClosed(3).rightAdjacent equiv pointOpen(3))
    assert(pointOpen(3).leftAdjacent equiv pointClosed(3))
    assert(pointClosed(maxValue).rightAdjacent equiv pointOpen(maxValue))
    assert(pointOpen(minValue).leftAdjacent equiv pointClosed(minValue))
    assert(bottom.leftAdjacent equiv bottom)
    assert(top.rightAdjacent equiv top)

    assert(pointClosed(3) equiv pointClosed(3))
    assert(pointClosed(3) <= pointClosed(3))
    assert(pointClosed(3) >= pointClosed(3))
    assert(pointClosed(0) < pointClosed(4))
    assert(pointClosed(4) > pointClosed(0))
    assert(bottom < pointClosed(4))
    assert(pointClosed(4) < top)
    assert(pointOpen(3) equiv pointOpen(3))
    assert(pointOpen(3) <= pointOpen(3))
    assert(pointOpen(3) >= pointOpen(3))
    assert(pointOpen(0) < pointOpen(4))
    assert(pointOpen(4) > pointOpen(0))
    assert(bottom < pointOpen(4))
    assert(pointOpen(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(pointClosed(3).orderedHash == pointClosed(3).orderedHash)
    assert(pointClosed(0).orderedHash <= pointClosed(4).orderedHash)
    assert(bottom.orderedHash <= pointClosed(4).orderedHash)
    assert(pointClosed(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assertResult("Point(3) x Bottom")((pointClosed(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((pointClosed(3) x Bottom x Top).toCodeLikeString)

  test("Ops on Doubles"):
    import ContinuousValue.DoubleContinuousValue

    def valueOf(d: Double): Double = d

    assert(valueOf(3) equiv valueOf(3))
    assert(valueOf(3) <= valueOf(3))
    assert(valueOf(3) >= valueOf(3))
    assert(valueOf(0) < valueOf(4))
    assert(valueOf(4) > valueOf(0))

  test("Ops on Double Domain"):
    import ContinuousValue.DoubleContinuousValue
    import ContinuousValue.DoubleContinuousValue.{maxValue, minValue}
    import Domain1D.*

    def pointClosed(v: Double): Domain1D[Double] = domain(v)
    def pointOpen(v: Double): Domain1D[Double] = open(v)
    def top: Domain1D[Double] = Top
    def bottom: Domain1D[Double] = Bottom

    assert(pointClosed(3).rightAdjacent equiv pointOpen(3))
    assert(pointOpen(3).leftAdjacent equiv pointClosed(3))
    assert(domain(maxValue).rightAdjacent equiv open(maxValue))
    assert(domain(minValue).leftAdjacent equiv open(minValue))
    assert(bottom.rightAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)

    assert(pointClosed(3) equiv pointClosed(3))
    assert(pointClosed(3) <= pointClosed(3))
    assert(pointClosed(3) >= pointClosed(3))
    assert(pointClosed(0) < pointClosed(4))
    assert(pointClosed(4) > pointClosed(0))
    assert(bottom < pointClosed(4))
    assert(pointClosed(4) < top)
    assert(pointOpen(3) equiv pointOpen(3))
    assert(pointOpen(3) <= pointOpen(3))
    assert(pointOpen(3) >= pointOpen(3))
    assert(pointOpen(0) < pointOpen(4))
    assert(pointOpen(4) > pointOpen(0))
    assert(bottom < pointOpen(4))
    assert(pointOpen(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(pointClosed(3).orderedHash == pointClosed(3).orderedHash)
    assert(pointClosed(0).orderedHash <= pointClosed(4).orderedHash)
    assert(bottom.orderedHash <= pointClosed(4).orderedHash)
    assert(pointClosed(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assertResult("Point(3.0) x Bottom")((pointClosed(3) x Bottom).toCodeLikeString)
    assertResult("Point(3.0) x Bottom x Top")((pointClosed(3) x Bottom x Top).toCodeLikeString)

  test("Ops on LocalDates"):
    import ContinuousValue.LocalDateContinuousValue

    val date0 = LocalDate.of(2024, 5, 31)
    // val date1 = LocalDate.of(2024, 6, 1)
    // val date2 = LocalDate.of(2024, 6, 2)
    val date3 = LocalDate.of(2024, 6, 3)
    val date4 = LocalDate.of(2024, 6, 4)
    assert(date3 equiv date3)
    assert(date3 <= date3)
    assert(date3 >= date3)
    assert(date0 < date4)
    assert(date4 > date0)

  test("Ops on LocalDate Domain"):
    import ContinuousValue.LocalDateContinuousValue
    import ContinuousValue.LocalDateContinuousValue.{maxValue, minValue}
    import Domain1D.*

    def top: Domain1D[LocalDate] = Top
    def bottom: Domain1D[LocalDate] = Bottom

    val date0 = LocalDate.of(2024, 5, 31)
    val date3 = LocalDate.of(2024, 6, 3)
    val date4 = LocalDate.of(2024, 6, 4)
    assert(domain(date3) equiv domain(date3))
    assert(domain(date3) <= domain(date3))
    assert(domain(date3) >= domain(date3))
    assert(domain(date0) < domain(date4))
    assert(domain(date4) > domain(date0))
    assert(bottom < domain(date4))
    assert(domain(date4) < top)
    assert(open(date3) equiv open(date3))
    assert(open(date3) <= open(date3))
    assert(open(date3) >= open(date3))
    assert(open(date0) < open(date4))
    assert(open(date4) > open(date0))
    assert(bottom < open(date4))
    assert(open(date4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > top))
    assert(!(bottom equiv top))

    assert(domain(date3).orderedHash == domain(date3).orderedHash)
    assert(domain(date0).orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash >= domain(date0).orderedHash)
    assert(bottom.orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

    assert(domain(maxValue).leftAdjacent equiv open(maxValue))
    assert(domain(minValue).leftAdjacent equiv open(minValue))
    assertResult(
      "Point(LocalDate.of(2024,5,31)) x Point(LocalDate.of(2024,6,3)) x Point(LocalDate.of(2024,6,4))"
    )((Point(date0) x Point(date3) x Point(date4)).toCodeLikeString)

  test("Ops on LocalDateTimes"):
    import ContinuousValue.LocalDateTimeContinuousValue

    val date0 = LocalDate.of(2024, 5, 31).atTime(1, 2, 3, 4)
    // val date1 = LocalDate.of(2024, 6, 1).atTime(1, 2, 3, 4)
    // val date2 = LocalDate.of(2024, 6, 2).atTime(1, 2, 3, 4)
    val date3 = LocalDate.of(2024, 6, 3).atTime(1, 2, 3, 4)
    val date4 = LocalDate.of(2024, 6, 4).atTime(1, 2, 3, 4)
    assert(date3 equiv date3)
    assert(date3 <= date3)
    assert(date3 >= date3)
    assert(date0 < date4)
    assert(date4 > date0)

  test("Ops on LocalDateTime Domain"):
    import ContinuousValue.LocalDateTimeContinuousValue
    import ContinuousValue.LocalDateTimeContinuousValue.{maxValue, minValue}
    import Domain1D.*

    def top: Domain1D[LocalDateTime] = Top
    def bottom: Domain1D[LocalDateTime] = Bottom

    val date0 = LocalDate.of(2024, 5, 31).atTime(1, 2, 3, 4)
    val date3 = LocalDate.of(2024, 6, 3).atTime(1, 2, 3, 4)
    val date4 = LocalDate.of(2024, 6, 4).atTime(1, 2, 3, 4)
    assert(domain(date3) equiv domain(date3))
    assert(domain(date3) <= domain(date3))
    assert(domain(date3) >= domain(date3))
    assert(domain(date0) < domain(date4))
    assert(domain(date4) > domain(date0))
    assert(bottom < domain(date4))
    assert(domain(date4) < top)
    assert(open(date3) equiv open(date3))
    assert(open(date3) <= open(date3))
    assert(open(date3) >= open(date3))
    assert(open(date0) < open(date4))
    assert(open(date4) > open(date0))
    assert(bottom < open(date4))
    assert(open(date4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > top))
    assert(!(bottom equiv top))

    assert(domain(date3).orderedHash == domain(date3).orderedHash)
    assert(domain(date0).orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash >= domain(date0).orderedHash)
    assert(bottom.orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    val belowUniqueHash = LocalDateTime.ofInstant(Instant.ofEpochMilli(Long.MinValue), ZoneOffset.UTC)
    val aboveUniqueHash = LocalDateTime.ofInstant(Instant.ofEpochMilli(Long.MaxValue), ZoneOffset.UTC)
    assert(domain(belowUniqueHash).orderedHash == bottom.orderedHash)
    assert(domain(aboveUniqueHash).orderedHash == top.orderedHash)

    assert(domain(maxValue).rightAdjacent equiv open(maxValue))
    assert(domain(minValue).leftAdjacent equiv open(minValue))
    assertResult(
      "Point(LocalDate.of(2024,5,31).atTime(1,2,3,4)) x " +
        "Point(LocalDate.of(2024,6,3).atTime(1,2,3,4)) x " +
        "Point(LocalDate.of(2024,6,4).atTime(1,2,3,4))"
    )((Point(date0) x Point(date3) x Point(date4)).toCodeLikeString)
