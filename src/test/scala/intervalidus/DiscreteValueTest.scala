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
    import DiscreteDomain1D.*
    import DiscreteValue.IntDiscreteValue
    import DiscreteValue.IntDiscreteValue.{maxValue, minValue}

    def point(v: Int): DiscreteDomain1D[Int] = Point(v)
    def top: DiscreteDomain1D[Int] = Top
    def bottom: DiscreteDomain1D[Int] = Bottom

    assert(point(3).successor equiv point(4))
    assert(point(3).predecessor equiv point(2))
    assert(point(maxValue).successor equiv top)
    assert(point(minValue).predecessor equiv bottom)
    assert(bottom.predecessor equiv bottom)
    assert(bottom.successor equiv bottom)
    assert(top.predecessor equiv top)
    assert(top.successor equiv top)
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
    import DiscreteDomain1D.*

    def point(v: Color): DiscreteDomain1D[Color] = Point(v)

    def top: DiscreteDomain1D[Color] = Top

    def bottom: DiscreteDomain1D[Color] = Bottom

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

    assert(point(Cyan).successor equiv point(Blue))
    assert(point(Cyan).predecessor equiv point(Green))
    assert(point(discreteColor.maxValue).successor equiv top)
    assert(point(discreteColor.minValue).predecessor equiv bottom)
    assert(bottom.predecessor equiv bottom)
    assert(bottom.successor equiv bottom)
    assert(top.predecessor equiv top)
    assert(top.successor equiv top)
    assert(point(Cyan) equiv point(Cyan))

    assertCompare(point(Cyan) <= point(Cyan))
    assertCompare(point(Cyan) >= point(Cyan))
    assertCompare(point(Red) < point(Blue))
    assertCompare(point(Blue) > point(Red))
    assertCompare(bottom < point(Blue))
    assertCompare(point(Blue) < top)
    assertCompare(bottom < top)
    assertCompare(bottom <= top)
    assertCompare(!(bottom > bottom))
    assertCompare(!(bottom equiv top))

    assertCompare(point(Cyan).orderedHash == point(Cyan).orderedHash)
    assertCompare(point(Red).orderedHash <= point(Blue).orderedHash)
    assertCompare(bottom.orderedHash <= point(Blue).orderedHash)
    assertCompare(point(Blue).orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)
    assertCompare(bottom.orderedHash <= top.orderedHash)

    assertResult("Point(Cyan) x Bottom")((point(Cyan) x Bottom).toCodeLikeString)
    assertResult("Point(Cyan) x Bottom x Top")((point(Cyan) x Bottom x Top).toCodeLikeString)

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
    import DiscreteDomain1D.*
    import DiscreteValue.LongDiscreteValue
    import DiscreteValue.LongDiscreteValue.{maxValue, minValue}

    def point(v: Long): DiscreteDomain1D[Long] = Point(v)
    def top: DiscreteDomain1D[Long] = Top
    def bottom: DiscreteDomain1D[Long] = Bottom

    assert(point(3).successor equiv point(4))
    assert(point(3).predecessor equiv point(2))
    assert(point(maxValue).successor equiv top)
    assert(point(minValue).predecessor equiv bottom)
    assert(bottom.predecessor equiv bottom)
    assert(bottom.successor equiv bottom)
    assert(top.predecessor equiv top)
    assert(top.successor equiv top)

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
    import DiscreteDomain1D.*
    import DiscreteValue.BigIntegerDiscreteValue
    import DiscreteValue.BigIntegerDiscreteValue.{maxValue, minValue}
    import java.math.BigInteger
    import java.math.BigInteger.valueOf

    def point(v: Int): DiscreteDomain1D[BigInteger] = Point(valueOf(v))
    def pointBigInteger(v: BigInteger): DiscreteDomain1D[BigInteger] = Point(v)
    def top: DiscreteDomain1D[BigInteger] = Top
    def bottom: DiscreteDomain1D[BigInteger] = Bottom

    assert(point(3).successor equiv point(4))
    assert(point(3).predecessor equiv point(2))
    assert(pointBigInteger(maxValue).successor equiv top)
    assert(pointBigInteger(minValue).predecessor equiv bottom)
    assert(bottom.predecessor equiv bottom)
    assert(bottom.successor equiv bottom)
    assert(top.predecessor equiv top)
    assert(top.successor equiv top)

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
    import DiscreteDomain1D.*
    import DiscreteValue.LocalDateDiscreteValue
    import DiscreteValue.LocalDateDiscreteValue.{maxValue, minValue}

    def point(v: LocalDate): DiscreteDomain1D[LocalDate] = Point(v)
    def top: DiscreteDomain1D[LocalDate] = Top
    def bottom: DiscreteDomain1D[LocalDate] = Bottom

    val date0 = LocalDate.of(2024, 5, 31)
    val date3 = LocalDate.of(2024, 6, 3)
    val date4 = LocalDate.of(2024, 6, 4)
    assert(point(date3) equiv point(date3))
    assert(point(date3) <= point(date3))
    assert(point(date3) >= point(date3))
    assert(point(date0) < point(date4))
    assert(point(date4) > point(date0))
    assert(bottom < point(date4))
    assert(point(date4) < top)
    assert(bottom < top)
    assert(bottom <= top)
    assert(!(bottom > top))
    assert(!(bottom equiv top))

    assert(point(date3).orderedHash == point(date3).orderedHash)
    assert(point(date0).orderedHash <= point(date4).orderedHash)
    assert(point(date4).orderedHash >= point(date0).orderedHash)
    assert(bottom.orderedHash <= point(date4).orderedHash)
    assert(point(date4).orderedHash <= top.orderedHash)
    assert(bottom.orderedHash <= top.orderedHash)

    assert(point(maxValue).successor equiv top)
    assert(point(minValue).predecessor equiv bottom)

    assertResult("Point(2024-05-31) x Point(2024-06-03) x Point(2024-06-04)")(
      (Point(date0) x Point(date3) x Point(date4)).toCodeLikeString
    )
