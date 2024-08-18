package intervalidus

import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalDate
import scala.math.Ordering.Implicits.infixOrderingOps

class DiscreteValueTest extends AnyFunSuite:

  test("Ops on Ints"):
    import DiscreteValue.IntDiscreteValue
    assert(1.predecessor equiv Some(0))
    assert(Int.MinValue.predecessor equiv None)
    assert(1.successor equiv Some(2))
    assert(Int.MaxValue.successor equiv None)
    assert(3 equiv 3)
    assert(3 <= 3)
    assert(3 >= 3)
    assert(0 < 4)
    assert(4 > 0)

  test("Ops on Int Data"):
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

  test("Ops on Longs"):
    import DiscreteValue.LongDiscreteValue
    assert(1.predecessor equiv Some(0))
    assert(Long.MinValue.predecessor equiv None)
    assert(1.successor equiv Some(2))
    assert(Long.MaxValue.successor equiv None)
    assert(3 equiv 3)
    assert(3 <= 3)
    assert(3 >= 3)
    assert(0 < 4)
    assert(4 > 0)

  test("Ops on Long Data"):
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

  test("Ops on BigIntegers"):
    import DiscreteValue.BigIntegerDiscreteValue
    import java.math.BigInteger.valueOf
    assert(valueOf(1).predecessor equiv Some(valueOf(0)))
    assert(BigIntegerDiscreteValue.minValue.predecessor equiv None)
    assert(valueOf(1).successor equiv Some(valueOf(2)))
    assert(BigIntegerDiscreteValue.maxValue.successor equiv None)
    assert(valueOf(3) equiv valueOf(3))
    assert(valueOf(3) <= valueOf(3))
    assert(valueOf(3) >= valueOf(3))
    assert(valueOf(0) < valueOf(4))
    assert(valueOf(4) > valueOf(0))

  test("Ops on BigInteger Data"):
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

  test("Ops on LocalDates"):
    import DiscreteValue.LocalDateDiscreteValue

    val date0 = LocalDate.of(2024, 5, 31)
    val date1 = LocalDate.of(2024, 6, 1)
    val date2 = LocalDate.of(2024, 6, 2)
    val date3 = LocalDate.of(2024, 6, 3)
    val date4 = LocalDate.of(2024, 6, 4)
    assert(date1.predecessor equiv Some(date0))
    assert(date1.successor equiv Some(date2))
    assert(date3 equiv date3)
    assert(date3 <= date3)
    assert(date3 >= date3)
    assert(date0 < date4)
    assert(date4 > date0)

  test("Ops on LocalDate Data"):
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

    assert(point(maxValue).successor equiv top)
    assert(point(minValue).predecessor equiv bottom)
