package intervalidus

import intervalidus.DomainLike.given
import intervalidus.collection.{Coordinate, CoordinateFixed}
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalDate
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

class DiscreteValueTest extends AnyFunSuite:
  test("Auto-derive Discrete Value"):
    enum Color derives DiscreteValue:
      case Red, Yellow, Green, Cyan, Blue, Magenta

    assertResult(Some(Color.Yellow))(Color.Red.successorValue)

  test("Ops on Ints"):
    import DiscreteValue.IntDiscreteValue
    assert(1.predecessorValue equiv Some(0))
    assert(Int.MinValue.predecessorValue equiv None)
    assert(1.successorValue equiv Some(2))
    assert(Int.MaxValue.successorValue equiv None)
    assert(3 equiv 3)
    assert(3 <= 3)
    assert(3 >= 3)
    assert(0 < 4)
    assert(4 > 0)

  test("Ops on Int Domain"):
    import DiscreteValue.IntDiscreteValue
    import DiscreteValue.IntDiscreteValue.{maxValue, minValue}
    import Domain1D.*

    def top: Domain1D[Int] = Top
    def bottom: Domain1D[Int] = Bottom

    // Can't be open
    assertThrows[IllegalArgumentException]:
      val _ = open(0)

    assert(domain(3).rightAdjacent equiv domain(4))
    assert(domain(4) isRightAdjacentTo domain(3))
    assert(domain(4) isAdjacentTo domain(3))
    assert(domain(3).leftAdjacent equiv domain(2))
    assert(domain(2) isLeftAdjacentTo domain(3))
    assert(domain(4) ~ domain(3))
    assert(domain(2) ~> domain(3))
    assert(domain(maxValue).rightAdjacent equiv top)
    assert(domain(minValue).leftAdjacent equiv bottom)
    assert(bottom.leftAdjacent equiv bottom)
    assert(bottom.rightAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)
    assert(top.rightAdjacent equiv top)
    assert(domain(3) equiv domain(3))
    assert(domain(3) <= domain(3))
    assert(domain(3) >= domain(3))
    assert(domain(0) < domain(4))
    assert(domain(4) > domain(0))
    assert(bottom < domain(4))
    assert(domain(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(domain(3).orderedHashFixed == domain(3).orderedHashFixed)
    assert(domain(0).orderedHashFixed <= domain(4).orderedHashFixed)
    assert(bottom.orderedHashFixed <= domain(4).orderedHashFixed)
    assert(domain(4).orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)

    assert(Domain.in1D(4) afterStart Domain.in1D(3))
    assert(!(Domain.in1D(4) afterStart Domain.in1D(4)))
    assert(Domain.in1D(4) afterOrAtStart Domain.in1D(4))
    assert(Domain.in1D(4) beforeOrAtEnd Domain.in1D(4))
    assert(!(Domain.in1D(4) beforeEnd Domain.in1D(4)))
    assert(Domain.in1D(3) beforeEnd Domain.in1D(4))

    assertResult("3")(Domain.in1D(3).asString)
    assertResult("{3, -∞}")((domain(3) x Bottom).asString)
    assertResult("Point(3) x Bottom")((domain(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((domain(3) x Bottom x Top).toCodeLikeString)

    assertResult(Coordinate(Some(3), None))((domain(3) x Bottom).asCoordinateUnfixed)
    assertResult(CoordinateFixed(3, Int.MinValue))((domain(3) x Bottom).asCoordinateFixed)

  test("Ops on Enum and Enum Domain"):
    // Can't be empty
    assertThrows[IllegalArgumentException]:
      val _ = DiscreteValue.fromSeq(IndexedSeq())

    // Must be distinct
    assertThrows[IllegalArgumentException]:
      val _ = DiscreteValue.fromSeq(IndexedSeq(1, 2, 1))

    enum Color:
      case Red, Yellow, Green, Cyan, Blue, Magenta
    given discreteColor: DiscreteValue[Color] = DiscreteValue.fromSeq(Color.values.toIndexedSeq)

    import Color.*
    import Domain1D.*

    def top: Domain1D[Color] = Top
    def bottom: Domain1D[Color] = Bottom

    // For some reason ScalaTest is intervening here and applying its own comparison and getting the wrong answer.
    // So we wrap these assertions to prevent its tinkering...
    def assertCompare(b: => Boolean): Assertion = assert(b)

    assert(Yellow.predecessorValue equiv Some(Red))
    assert(Red.predecessorValue equiv None)
    assert(Blue.successorValue equiv Some(Magenta))
    assert(Magenta.successorValue equiv None)
    assert(Cyan equiv Cyan)
    assertCompare(Cyan <= Cyan)
    assertCompare(Cyan >= Cyan)
    assertCompare(Red < Blue)
    assertCompare(Blue > Red)

    assert(domain(Cyan).rightAdjacent equiv domain(Blue))
    assert(domain(Cyan).leftAdjacent equiv domain(Green))
    assert(domain(discreteColor.maxValue).rightAdjacent equiv top)
    assert(domain(discreteColor.minValue).leftAdjacent equiv bottom)
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

    assertCompare(domain(Cyan).orderedHashFixed == domain(Cyan).orderedHashFixed)
    assertCompare(domain(Red).orderedHashFixed <= domain(Blue).orderedHashFixed)
    assertCompare(bottom.orderedHashFixed <= domain(Blue).orderedHashFixed)
    assertCompare(domain(Blue).orderedHashFixed <= top.orderedHashFixed)
    assertCompare(bottom.orderedHashFixed <= top.orderedHashFixed)
    assertCompare(bottom.orderedHashFixed <= top.orderedHashFixed)

    assertResult("Point(Cyan) x Bottom")((domain(Cyan) x Bottom).toCodeLikeString)
    assertResult("Point(Cyan) x Bottom x Top")((domain(Cyan) x Bottom x Top).toCodeLikeString)

  test("Ops on Longs"):
    import DiscreteValue.LongDiscreteValue
    assert(1.predecessorValue equiv Some(0))
    assert(Long.MinValue.predecessorValue equiv None)
    assert(1.successorValue equiv Some(2))
    assert(Long.MaxValue.successorValue equiv None)
    assert(3 equiv 3)
    assert(3 <= 3)
    assert(3 >= 3)
    assert(0 < 4)
    assert(4 > 0)

  test("Ops on Long Domain"):
    import DiscreteValue.LongDiscreteValue
    import DiscreteValue.LongDiscreteValue.{maxValue, minValue}
    import Domain1D.*

    def point(v: Long): Domain1D[Long] = domain(v)
    def top: Domain1D[Long] = Top
    def bottom: Domain1D[Long] = Bottom

    assert(point(3).rightAdjacent equiv point(4))
    assert(point(3).leftAdjacent equiv point(2))
    assert(point(maxValue).rightAdjacent equiv top)
    assert(point(minValue).leftAdjacent equiv bottom)
    assert(bottom.leftAdjacent equiv bottom)
    assert(bottom.rightAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)
    assert(top.rightAdjacent equiv top)

    assert(point(3) equiv point(3))
    assert(point(3) <= point(3))
    assert(point(3) >= point(3))
    assert(point(0) < point(4))
    assert(point(4) > point(0))
    assert(bottom < point(4))
    assert(point(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(point(3).orderedHashFixed == point(3).orderedHashFixed)
    assert(point(0).orderedHashFixed <= point(4).orderedHashFixed)
    assert(bottom.orderedHashFixed <= point(4).orderedHashFixed)
    assert(point(4).orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)

    assertResult("Point(3) x Bottom")((point(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((point(3) x Bottom x Top).toCodeLikeString)

  test("Ops on BigIntegers"):
    import DiscreteValue.BigIntegerDiscreteValue

    import java.math.BigInteger.valueOf
    assert(valueOf(1).predecessorValue equiv Some(valueOf(0)))
    assert(BigIntegerDiscreteValue.minValue.predecessorValue equiv None)
    assert(valueOf(1).successorValue equiv Some(valueOf(2)))
    assert(BigIntegerDiscreteValue.maxValue.successorValue equiv None)
    assert(valueOf(3) equiv valueOf(3))
    assert(valueOf(3) <= valueOf(3))
    assert(valueOf(3) >= valueOf(3))
    assert(valueOf(0) < valueOf(4))
    assert(valueOf(4) > valueOf(0))

  test("Ops on BigInteger Domain"):
    import DiscreteValue.BigIntegerDiscreteValue
    import DiscreteValue.BigIntegerDiscreteValue.{maxValue, minValue}
    import Domain1D.*

    import java.math.BigInteger
    import java.math.BigInteger.valueOf

    def point(v: Int): Domain1D[BigInteger] = domain(valueOf(v))
    def top: Domain1D[BigInteger] = Top
    def bottom: Domain1D[BigInteger] = Bottom

    assert(point(3).rightAdjacent equiv point(4))
    assert(point(3).leftAdjacent equiv point(2))
    assert(domain(maxValue).rightAdjacent equiv top)
    assert(domain(minValue).leftAdjacent equiv bottom)
    assert(bottom.leftAdjacent equiv bottom)
    assert(bottom.rightAdjacent equiv bottom)
    assert(top.leftAdjacent equiv top)
    assert(top.rightAdjacent equiv top)

    assert(point(3) equiv point(3))
    assert(point(3) <= point(3))
    assert(point(3) >= point(3))
    assert(point(0) < point(4))
    assert(point(4) > point(0))
    assert(bottom < point(4))
    assert(point(4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > bottom))
    assert(!(bottom equiv top))

    assert(point(3).orderedHashFixed == point(3).orderedHashFixed)
    assert(point(0).orderedHashFixed <= point(4).orderedHashFixed)
    assert(bottom.orderedHashFixed <= point(4).orderedHashFixed)
    assert(point(4).orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)

    assertResult("Point(3) x Bottom")((point(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((point(3) x Bottom x Top).toCodeLikeString)

  test("Ops on LocalDates"):
    import DiscreteValue.LocalDateDiscreteValue

    val date0 = LocalDate.of(2024, 5, 31)
    val date1 = LocalDate.of(2024, 6, 1)
    val date2 = LocalDate.of(2024, 6, 2)
    val date3 = LocalDate.of(2024, 6, 3)
    val date4 = LocalDate.of(2024, 6, 4)
    assert(date1.predecessorValue equiv Some(date0))
    assert(date1.successorValue equiv Some(date2))
    assert(date3 equiv date3)
    assert(date3 <= date3)
    assert(date3 >= date3)
    assert(date0 < date4)
    assert(date4 > date0)

  test("Ops on LocalDate Domain"):
    import DiscreteValue.LocalDateDiscreteValue
    import DiscreteValue.LocalDateDiscreteValue.{maxValue, minValue}
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
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > top))
    assert(!(bottom equiv top))

    assert(domain(date3).orderedHashFixed == domain(date3).orderedHashFixed)
    assert(domain(date0).orderedHashFixed <= domain(date4).orderedHashFixed)
    assert(domain(date4).orderedHashFixed >= domain(date0).orderedHashFixed)
    assert(bottom.orderedHashFixed <= domain(date4).orderedHashFixed)
    assert(domain(date4).orderedHashFixed <= top.orderedHashFixed)
    assert(bottom.orderedHashFixed <= top.orderedHashFixed)

    assert(domain(maxValue).rightAdjacent equiv top)
    assert(domain(minValue).leftAdjacent equiv bottom)

    assertResult("Point(LocalDate.of(2024,5,31)) x Point(LocalDate.of(2024,6,3)) x Point(LocalDate.of(2024,6,4))")(
      (Point(date0) x Point(date3) x Point(date4)).toCodeLikeString
    )
