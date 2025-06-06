package intervalidus

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalDate
import scala.math.Ordering.Implicits.infixOrderingOps

class DiscreteValueTest extends AnyFunSuite:

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
    import Domain1D.*
    import DiscreteValue.IntDiscreteValue
    import DiscreteValue.IntDiscreteValue.{maxValue, minValue}

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

    assert(domain(3).orderedHash == domain(3).orderedHash)
    assert(domain(0).orderedHash <= domain(4).orderedHash)
    assert(bottom.orderedHash <= domain(4).orderedHash)
    assert(domain(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

    assertResult("Point(3) x Bottom")((domain(3) x Bottom).toCodeLikeString)
    assertResult("Point(3) x Bottom x Top")((domain(3) x Bottom x Top).toCodeLikeString)

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

    assertCompare(domain(Cyan).orderedHash == domain(Cyan).orderedHash)
    assertCompare(domain(Red).orderedHash <= domain(Blue).orderedHash)
    assertCompare(bottom.orderedHash <= domain(Blue).orderedHash)
    assertCompare(domain(Blue).orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)

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
    import Domain1D.*
    import DiscreteValue.LongDiscreteValue
    import DiscreteValue.LongDiscreteValue.{maxValue, minValue}

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

    assert(point(3).orderedHash == point(3).orderedHash)
    assert(point(0).orderedHash <= point(4).orderedHash)
    assert(bottom.orderedHash <= point(4).orderedHash)
    assert(point(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

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
    import Domain1D.*
    import DiscreteValue.BigIntegerDiscreteValue
    import DiscreteValue.BigIntegerDiscreteValue.{maxValue, minValue}
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

    assert(point(3).orderedHash == point(3).orderedHash)
    assert(point(0).orderedHash <= point(4).orderedHash)
    assert(bottom.orderedHash <= point(4).orderedHash)
    assert(point(4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

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
    import Domain1D.*
    import DiscreteValue.LocalDateDiscreteValue
    import DiscreteValue.LocalDateDiscreteValue.{maxValue, minValue}

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

    assert(domain(date3).orderedHash == domain(date3).orderedHash)
    assert(domain(date0).orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash >= domain(date0).orderedHash)
    assert(bottom.orderedHash <= domain(date4).orderedHash)
    assert(domain(date4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

    assert(domain(maxValue).rightAdjacent equiv top)
    assert(domain(minValue).leftAdjacent equiv bottom)

    assertResult("Point(LocalDate.of(2024,5,31)) x Point(LocalDate.of(2024,6,3)) x Point(LocalDate.of(2024,6,4))")(
      (Point(date0) x Point(date3) x Point(date4)).toCodeLikeString
    )
