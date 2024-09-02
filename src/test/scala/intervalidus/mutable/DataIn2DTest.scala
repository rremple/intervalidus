package intervalidus.mutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DTest extends AnyFunSuite with Matchers with DataIn2DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn2D(_), DataIn2D.of(_)))

  def vertical2D[T: DiscreteValue](interval2: DiscreteInterval1D[T]): DiscreteInterval2D[LocalDate, T] =
    unbounded[LocalDate] x interval2

  test("Mutable: Looking up data in intervals - bounded r1"):
    val empty: DataIn2D[String, Int, Int] = DataIn2D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData =
      testData(("Hello", intervalTo(day(14)), interval(0, 10)), ("World", intervalFrom(day(1)), intervalFrom(11)))
    // even though naive validity check fails (since intervalTo(day(14)) overlaps intervalFrom(day(1))), fixture is
    // still valid
    val fixture = immutable.DataIn2D(allData).toImmutable.toMutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")

    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Mutable: Adding and removing data in intervals - unbounded r1"):
    val allData = testData(("Hello", unbounded, interval(0, 10)), ("World", unbounded, intervalFrom(11)))
    val fixture = DataIn2D(allData)

    fixture.set(vertical2D(interval(5, 15)) -> "to")
    val expectedData1 = testData(
      ("Hello", unbounded, interval(0, 4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, intervalFrom(16))
    )
    fixture.getAll.toList shouldBe expectedData1

    fixture.set(vertical2D(interval(20, 25)) -> "!") // split
    val expectedData2 = testData(
      ("Hello", unbounded, interval(0, 4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, interval(16, 19)),
      ("!", unbounded, interval(20, 25)),
      ("World", unbounded, intervalFrom(26))
    )
    fixture.getAll.toList shouldBe expectedData2

    val copyFixture2 = fixture.copy

    assert(!fixture.setIfNoConflict(vertical2D(intervalTo(4)) -> "Hey"))
    assert(fixture.setIfNoConflict(vertical2D(intervalTo(-1)) -> "Hey"))

    fixture.set(vertical2D(intervalTo(4)) -> "Hey")
    fixture.remove(vertical2D(intervalFrom(21)))
    val expectedData3 = testData(
      ("Hey", unbounded, intervalTo(4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, interval(16, 19)),
      ("!", unbounded, intervalAt(20))
    )
    fixture.getAll.toList shouldBe expectedData3

    val fixture3a = fixture.copy
    fixture3a.replaceByKey(
      (unbounded[LocalDate] x intervalTo(4)).start,
      (unbounded[LocalDate] x intervalTo(3)) -> "Hello"
    )
    fixture3a.replace(
      (unbounded[LocalDate] x interval(16, 19)) -> "World",
      (unbounded[LocalDate] x interval(15, 20)) -> "World!"
    )
    val expectedData3a = testData(
      ("Hello", unbounded, intervalTo(3)),
      ("to", unbounded, interval(5, 14)),
      ("World!", unbounded, interval(15, 20))
    )
    fixture3a.getAll.toList shouldBe expectedData3a

    fixture.set(vertical2D(intervalFrom(20)) -> "World")
    val expectedData4 = testData(
      ("Hey", unbounded, intervalTo(4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, intervalFrom(16))
    )
    fixture.getAll.toList shouldBe expectedData4

    val copyFixture4 = fixture.copy

    fixture.remove(vertical2D(interval(5, 15)))
    val expectedData5 = testData(("Hey", unbounded, intervalTo(4)), ("World", unbounded, intervalFrom(16)))
    fixture.getAll.toList shouldBe expectedData5

    fixture.set((intervalFrom(day(1)) x intervalFrom(1)) -> "remove me")
    fixture.remove(intervalFrom(day(1)) x intervalFrom(1))
    val expectedData6 = testData(
      ("Hey", unbounded, intervalTo(0)),
      ("Hey", intervalTo(day(0)), interval(1, 4)),
      ("World", intervalTo(day(0)), intervalFrom(16))
    )
    fixture.getAll.toList shouldBe expectedData6

    val copyFixture6 = fixture.copy

    import DiffAction2D.*
    import DiscreteDomain1D.{Bottom, Point}

    val actionsFrom2To4 = copyFixture4.diffActionsFrom(copyFixture2)
    actionsFrom2To4.toList shouldBe List(
      Create(vertical2D(intervalTo(4)) -> "Hey"),
      Delete(DiscreteDomain2D(Bottom, 0)),
      Update(vertical2D(intervalFrom(16)) -> "World"),
      Delete(DiscreteDomain2D(Bottom, 20)),
      Delete(DiscreteDomain2D(Bottom, 26))
    )
    val actionsFrom4To6 = copyFixture6.diffActionsFrom(copyFixture4)
    actionsFrom4To6.toList shouldBe List(
      Update(vertical2D(intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x interval(1, 4)) -> "Hey"),
      Delete(DiscreteDomain2D(Bottom, Point(5))),
      Update((intervalTo(day(0)) x intervalFrom(16)) -> "World")
    )
    copyFixture2.applyDiffActions(actionsFrom2To4)
    copyFixture2.getAll.toList shouldBe expectedData4

    copyFixture2.syncWith(copyFixture6)
    copyFixture2.getAll.toList shouldBe expectedData6

  test("Mutable: Mapping, flatmapping, etc."):
    val allData = testData(("Hey", unbounded, intervalTo(4)), ("World", unbounded, intervalFrom(16)))

    val fixture = DataIn2D(allData)
    fixture.copy.getAll.toList shouldBe fixture.getAll.toList

    val concat = fixture.foldLeft(StringBuilder()): (b, d) =>
      b.append(d.value).append("->").append(d.interval.vertical.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.withVerticalUpdate(_.endingWith(d.interval.vertical.end.successor))
      )
    )
    val expectedData2 = testData(("Hey!", unbounded, intervalTo(5)), ("World!", unbounded, intervalFrom(16)))
    fixture.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = testData(("Hey!!!", unbounded, intervalTo(5)), ("World!!!", unbounded, intervalFrom(16)))
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap(d => DataIn2D(Seq(d)))
    val expectedData4 = testData(("Hey!!!", unbounded, intervalTo(5)), ("World!!!", unbounded, intervalFrom(16)))
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.filter(_.interval.vertical ⊆ intervalTo(10))
    val expectedData5 = testData(("Hey!!!", unbounded, intervalTo(5)))
    fixture.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataIn2D.of[String, LocalDate, Int](d.value))
    val expectedData6 = testData(("Hey!!!", unbounded, unbounded))
    fixture.getAll.toList shouldBe expectedData6
    fixture.get shouldBe "Hey!!!"

    fixture.filter(_.value == "Planet")
    assert(fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

  test("Mutable: Compressing data in intervals - unbounded r1"):
    val allData = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, intervalAt(5)),
      ("World", unbounded, intervalAt(6)),
      ("Hello", unbounded, intervalAt(7)),
      ("Hello", unbounded, interval(8, 9)),
      ("Hello", unbounded, intervalFrom(10))
    )

    val fixture1 = DataIn2D(allData)
    fixture1.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])
    fixture1.compress("Hello")
    val expectedData1 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, intervalAt(5)),
      ("World", unbounded, intervalAt(6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    fixture1.getAll.toList shouldBe expectedData1
    fixture1.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

    val fixture2 = DataIn2D(allData)
    fixture2.compressAll()
    val expectedData2 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, interval(5, 6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

  test("Mutable: Updating data in intervals - unbounded r1"):
    val allData = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, interval(5, 6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    val fixture = DataIn2D(allData)

    fixture.update(vertical2D(interval(5, 7)) -> "World!")
    val expectedData1 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World!", unbounded, interval(5, 7)),
      ("Hello", unbounded, intervalFrom(8))
    )
    fixture.getAll.toList shouldBe expectedData1

    fixture.remove(vertical2D(interval(3, 5)))
    fixture.update(vertical2D(interval(2, 9)) -> "to")
    val expectedData2 = testData(
      ("Hello", unbounded, intervalTo(1)),
      ("to", unbounded, intervalAt(2)),
      ("to", unbounded, interval(6, 9)),
      ("Hello", unbounded, intervalFrom(10))
    )
    fixture.getAll.toList shouldBe expectedData2
    fixture.domain.toList shouldBe List(
      unbounded[Int] x intervalTo(2),
      unbounded[Int] x intervalFrom(6)
    )

    fixture.remove(vertical2D(interval(2, 9)))
    fixture.update(vertical2D(interval(-5, -2)) -> "World!")
    val expectedData3 = testData(
      ("Hello", unbounded, intervalTo(-6)),
      ("World!", unbounded, interval(-5, -2)),
      ("Hello", unbounded, interval(-1, 1)),
      ("Hello", unbounded, intervalFrom(10))
    )
    fixture.getAll.toList shouldBe expectedData3
    fixture.domain.toList shouldBe List(
      unbounded[Int] x intervalTo(1),
      unbounded[Int] x intervalFrom(10)
    )

    fixture.set((intervalFrom(day(0)) x intervalFrom(1)) -> "update me")
    fixture.update((intervalFrom(day(1)) x intervalFrom(0)) -> "update me")
    fixture.compressAll()
    val expectedData4 = testData(
      ("Hello", unbounded, intervalTo(-6)),
      ("World!", unbounded, interval(-5, -2)),
      ("Hello", unbounded, intervalAt(-1)),
      ("Hello", intervalTo(day(0)), intervalAt(0)),
      ("Hello", intervalTo(day(-1)), intervalAt(1)),
      ("Hello", intervalTo(day(-1)), intervalFrom(10)),
      ("update me", intervalFrom(day(0)), intervalFrom(1)),
      ("update me", intervalFrom(day(1)), intervalAt(0))
    )

    fixture.getAll.toList shouldBe expectedData4
    fixture.domain.toList shouldBe List(
      unbounded[LocalDate] x intervalTo(0), // first four
      intervalTo(day(-1)) x intervalAt(1),
      intervalTo(day(-1)) x intervalFrom(10),
      intervalFrom(day(0)) x intervalFrom(1) // last two
    )

  /*
   * Two dimensional exclusions and updates can have 3 x 3 = 9 cases. But there is symmetry for 3 of
   * them, so logically only 6:
   *  (1) simple + simple = simple (1)
   *  (2) simple + partial or partial + simple = edge (2), each with or without a common start
   *  (3) simple + split or split + simple = slice (2), vertical or horizontal
   *  (4) partial + partial = corner (1)
   *  (5) partial + split or split + partial = bite (2)
   *  (6) split + split = hole (1)
   */
  test("Mutable: All remove by interval - (1) simple + simple = simple"):
    // the same or larger
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(-14), day(14)) x interval(4, 7))
    val expectedData1 = testData()
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn2D(allData)
    fixture2.remove(interval(day(-15), day(15)) x interval(3, 8))
    val expectedData2 = testData()
    fixture2.getAll.toList shouldBe expectedData2

  test("Mutable: All remove by interval - (2) simple + partial or partial + simple = edge (2)"):
    // each with or without a common start
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // partial + simple, to the right (remainder with common start)
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(8), day(14)) x interval(3, 8))
    val expectedData1 = testData(
      ("World", interval(day(-14), day(7)), interval(4, 7))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // partial + simple, to the left (remainder does not have a common start)
    val fixture2 = DataIn2D(allData)
    fixture2.remove(interval(day(-15), day(-1)) x interval(3, 8))
    val expectedData2 = testData(
      ("World", interval(day(0), day(14)), interval(4, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // simple + partial, above (remainder with common start)
    val fixture3 = DataIn2D(allData)
    fixture3.remove(interval(day(-15), day(15)) x interval(6, 8))
    val expectedData3 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // simple + partial, below (remainder does not have a common start)
    val fixture4 = DataIn2D(allData)
    fixture4.remove(interval(day(-15), day(15)) x interval(1, 5))
    val expectedData4 = testData(
      ("World", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All remove by interval - (3) simple + split or split + simple = slice (2)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // vertical slice, resulting in a left and right elements
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(-1), day(1)) x interval(3, 8))
    val expectedData1 = testData(
      ("World", interval(day(-14), day(-2)), interval(4, 7)),
      ("World", interval(day(2), day(14)), interval(4, 7))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // horizontal slice, resulting in a lower and upper elements
    val fixture2 = DataIn2D(allData)
    fixture2.remove(interval(day(-15), day(15)) x intervalAt(5))
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

  test("Mutable: All remove by interval - (4) partial + partial = corner (1)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // lower left
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(-15), day(-8)) x interval(3, 5))
    val expectedData1 = testData(
      ("World", interval(day(-14), day(14)), interval(6, 7)),
      ("World", interval(day(-7), day(14)), interval(4, 5))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // upper left
    val fixture2 = DataIn2D(allData)
    fixture2.remove(interval(day(-15), day(-8)) x interval(6, 8))
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5)),
      ("World", interval(day(-7), day(14)), interval(6, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // lower right
    val fixture3 = DataIn2D(allData)
    fixture3.remove(interval(day(8), day(15)) x interval(3, 5))
    val expectedData3 = testData(
      ("World", interval(day(-14), day(7)), interval(4, 5)),
      ("World", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // upper right
    val fixture4 = DataIn2D(allData)
    fixture4.remove(interval(day(8), day(15)) x interval(6, 8))
    val expectedData4 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5)),
      ("World", interval(day(-14), day(7)), interval(6, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All remove by interval - (5) partial + split or split + partial = bite (2)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // bite left
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(-15), day(-8)) x interval(5, 6))
    val expectedData1 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(14)), intervalAt(7)),
      ("World", interval(day(-7), day(14)), interval(5, 6))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // bite right
    val fixture2 = DataIn2D(allData)
    fixture2.remove(interval(day(8), day(15)) x interval(5, 6))
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(7)), interval(5, 6)),
      ("World", interval(day(-14), day(14)), intervalAt(7))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // bite below
    val fixture3 = DataIn2D(allData)
    fixture3.remove(interval(day(-6), day(6)) x interval(3, 5))

    val expectedData3 = testData(
      ("World", interval(day(-14), day(-7)), interval(4, 7)),
      ("World", interval(day(-6), day(6)), interval(6, 7)),
      ("World", interval(day(7), day(14)), interval(4, 7))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // bite above
    val fixture4 = DataIn2D(allData)
    fixture4.remove(interval(day(-6), day(6)) x interval(6, 8))
    val expectedData4 = testData(
      ("World", interval(day(-14), day(-7)), interval(4, 7)),
      ("World", interval(day(-6), day(6)), interval(4, 5)),
      ("World", interval(day(7), day(14)), interval(4, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All remove by interval - (6) split + split = hole (1)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )
    val fixture1 = DataIn2D(allData)
    fixture1.remove(interval(day(-6), day(6)) x interval(5, 6))
    val expectedData1 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(-7)), interval(5, 6)),
      ("World", interval(day(-14), day(14)), intervalAt(7)),
      ("World", interval(day(7), day(14)), interval(5, 6))
    )
    fixture1.getAll.toList shouldBe expectedData1

  // same, but for updates

  test("Mutable: All update by interval - (1) simple + simple = simple"):
    // the same or larger
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(-14), day(14)) x interval(4, 7)) -> "update")
    val expectedData1 = testData(("update", interval(day(-14), day(14)), interval(4, 7)))
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = DataIn2D(allData)
    fixture2.update((interval(day(-15), day(15)) x interval(3, 8)) -> "update")
    val expectedData2 = testData(("update", interval(day(-14), day(14)), interval(4, 7)))
    fixture2.getAll.toList shouldBe expectedData2

  test("Mutable: All update by interval - (2) simple + partial or partial + simple = edge (2)"):
    // each with or without a common start
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // partial + simple, to the right (remainder with common start)
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(8), day(14)) x interval(3, 8)) -> "update")
    val expectedData1 = testData(
      ("World", interval(day(-14), day(7)), interval(4, 7)),
      ("update", interval(day(8), day(14)), interval(4, 7))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // partial + simple, to the left (remainder does not have a common start)
    val fixture2 = DataIn2D(allData)
    fixture2.update((interval(day(-15), day(-1)) x interval(3, 8)) -> "update")
    val expectedData2 = testData(
      ("update", interval(day(-14), day(-1)), interval(4, 7)),
      ("World", interval(day(0), day(14)), interval(4, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // simple + partial, above (remainder with common start)
    val fixture3 = DataIn2D(allData)
    fixture3.update((interval(day(-14), day(14)) x interval(6, 8)) -> "update")
    val expectedData3 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5)),
      ("update", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // simple + partial, below (remainder does not have a common start)
    val fixture4 = DataIn2D(allData)
    fixture4.update((interval(day(-15), day(15)) x interval(1, 5)) -> "update")
    val expectedData4 = testData(
      ("update", interval(day(-14), day(14)), interval(4, 5)),
      ("World", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All update by interval - (3) simple + split or split + simple = slice (2)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // vertical slice, resulting in a left and right elements
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(-1), day(1)) x interval(3, 8)) -> "update")
    val expectedData1 = testData(
      ("World", interval(day(-14), day(-2)), interval(4, 7)),
      ("update", interval(day(-1), day(1)), interval(4, 7)),
      ("World", interval(day(2), day(14)), interval(4, 7))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // horizontal slice, resulting in a lower and upper elements
    val fixture2 = DataIn2D(allData)
    fixture2.update((interval(day(-15), day(15)) x intervalAt(5)) -> "update")
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("update", interval(day(-14), day(14)), intervalAt(5)),
      ("World", interval(day(-14), day(14)), interval(6, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

  test("Mutable: All update by interval - (4) partial + partial = corner (1)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // lower left
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(-15), day(-8)) x interval(3, 5)) -> "update")
    val expectedData1 = testData(
      ("update", interval(day(-14), day(-8)), interval(4, 5)),
      ("World", interval(day(-14), day(14)), interval(6, 7)),
      ("World", interval(day(-7), day(14)), interval(4, 5))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // upper left
    val fixture2 = DataIn2D(allData)
    fixture2.update((interval(day(-15), day(-8)) x interval(6, 8)) -> "update")
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5)),
      ("update", interval(day(-14), day(-8)), interval(6, 7)),
      ("World", interval(day(-7), day(14)), interval(6, 7))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // lower right
    val fixture3 = DataIn2D(allData)
    fixture3.update((interval(day(8), day(15)) x interval(3, 5)) -> "update")
    val expectedData3 = testData(
      ("World", interval(day(-14), day(7)), interval(4, 5)),
      ("World", interval(day(-14), day(14)), interval(6, 7)),
      ("update", interval(day(8), day(14)), interval(4, 5))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // upper right
    val fixture4 = DataIn2D(allData)
    fixture4.update((interval(day(8), day(15)) x interval(6, 8)) -> "update")
    val expectedData4 = testData(
      ("World", interval(day(-14), day(14)), interval(4, 5)),
      ("World", interval(day(-14), day(7)), interval(6, 7)),
      ("update", interval(day(8), day(14)), interval(6, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All update by interval - (5) partial + split or split + partial = bite (2)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )

    // bite left
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(-15), day(-8)) x interval(5, 6)) -> "update")
    val expectedData1 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("update", interval(day(-14), day(-8)), interval(5, 6)),
      ("World", interval(day(-14), day(14)), intervalAt(7)),
      ("World", interval(day(-7), day(14)), interval(5, 6))
    )
    fixture1.getAll.toList shouldBe expectedData1

    // bite right
    val fixture2 = DataIn2D(allData)
    fixture2.update((interval(day(8), day(15)) x interval(5, 6)) -> "update")
    val expectedData2 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(7)), interval(5, 6)),
      ("World", interval(day(-14), day(14)), intervalAt(7)),
      ("update", interval(day(8), day(14)), interval(5, 6))
    )
    fixture2.getAll.toList shouldBe expectedData2

    // bite below
    val fixture3 = DataIn2D(allData)
    fixture3.update((interval(day(-6), day(6)) x interval(3, 5)) -> "update")

    val expectedData3 = testData(
      ("World", interval(day(-14), day(-7)), interval(4, 7)),
      ("update", interval(day(-6), day(6)), interval(4, 5)),
      ("World", interval(day(-6), day(6)), interval(6, 7)),
      ("World", interval(day(7), day(14)), interval(4, 7))
    )
    fixture3.getAll.toList shouldBe expectedData3

    // bite above
    val fixture4 = DataIn2D(allData)
    fixture4.update((interval(day(-6), day(6)) x interval(6, 8)) -> "update")
    val expectedData4 = testData(
      ("World", interval(day(-14), day(-7)), interval(4, 7)),
      ("World", interval(day(-6), day(6)), interval(4, 5)),
      ("update", interval(day(-6), day(6)), interval(6, 7)),
      ("World", interval(day(7), day(14)), interval(4, 7))
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Mutable: All update by interval - (6) split + split = hole (1)"):
    val allData: Seq[ValidData2D[String, LocalDate, Int]] = testData(
      ("World", interval(day(-14), day(14)), interval(4, 7))
    )
    val fixture1 = DataIn2D(allData)
    fixture1.update((interval(day(-6), day(6)) x interval(5, 6)) -> "update")
    val expectedData1 = testData(
      ("World", interval(day(-14), day(14)), intervalAt(4)),
      ("World", interval(day(-14), day(-7)), interval(5, 6)),
      ("World", interval(day(-14), day(14)), intervalAt(7)),
      ("update", interval(day(-6), day(6)), interval(5, 6)),
      ("World", interval(day(7), day(14)), interval(5, 6))
    )
    fixture1.getAll.toList shouldBe expectedData1

  test("Mutable: Simple toString"):
    val fixturePadData = DataIn2D.of[String, LocalDate, Int]("H")
    fixturePadData.set((intervalFrom(day(0)) x unbounded[Int]) -> "W")
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 2024-07-14 | 2024-07-15 .. +∞ |
         || H (-∞..+∞)       |
         |                   | W (-∞..+∞)       |
         |""".stripMargin.replaceAll("\r", "")

    val fixturePadLabel = DataIn2D.of[String, LocalDate, Int]("Helloooooooooo")
    fixturePadLabel.set((intervalFrom(day(0)) x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 2024-07-14        | 2024-07-15 .. +∞        |
         || Helloooooooooo (-∞..+∞) |
         |                          | Wooooooorld (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")
