package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DTest extends AnyFunSuite with Matchers with DataIn2DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn2D(_), DataIn2D.of(_)))
  testsFor(
    stringLookupTests("Mutable [experimental noSearchTree]", DataIn2D(_), DataIn2D.of(_))(using
      Experimental("noSearchTree")
    )
  )

  protected def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData2D[String, LocalDate, Int]*
  )(
    removeOrUpdateInterval: DiscreteInterval2D[LocalDate, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val rectangle = interval(day(-14), day(14)) x interval(4, 7)
    val removeFixture = DataIn2D.of(rectangle -> "World")
    val updateFixture = DataIn2D.of(rectangle -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ rectangle match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the rectangle")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
    removeFixture.remove(removeOrUpdateInterval)
    removeFixture.recompressAll()
    updateFixture.update(removeOrUpdateInterval -> updateValue)
    updateFixture.recompressAll()
    try assertResult(removeExpected)(removeFixture.getAll.toList)
    catch
      case ex: Exception =>
        println("Test failed (in remove), here's the actual remove data:")
        println(removeFixture.getAll.map(_.toCodeLikeString).mkString(",\n"))
        throw ex
    try assertResult(updateExpected)(updateFixture.getAll.toList)
    catch
      case ex: Exception =>
        println("Test failed (in update), here's the actual update data:")
        println(updateFixture.getAll.map(_.toCodeLikeString).mkString(",\n"))
        throw ex

  testsFor(removeOrUpdateTests("Mutable"))
  testsFor(removeOrUpdateTests("Mutable [experimental noSearchTree]")(using Experimental("noSearchTree")))
  testsFor(removeOrUpdateTests("Mutable [experimental bruteForceUpdate]")(using Experimental("bruteForceUpdate")))

  def vertical2D[T: DiscreteValue](interval2: DiscreteInterval1D[T]): DiscreteInterval2D[LocalDate, T] =
    unbounded[LocalDate] x interval2

  test("Mutable: Looking up data in intervals - bounded r1"):
    val empty: DataIn2D[String, Int, Int] = DataIn2D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData =
      testData(("Hello", intervalTo(day(14)), interval(0, 10)), ("World", intervalFrom(day(1)), intervalFrom(11)))
    val fixture = immutable.DataIn2D(allData).toImmutable.toMutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")

    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Mutable [experimental noSearchTree]: Looking up data in intervals - bounded r1"):
    given Experimental = Experimental("noSearchTree")

    val allData =
      testData(("Hello", intervalTo(day(14)), interval(0, 10)), ("World", intervalFrom(day(1)), intervalFrom(11)))
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
      vertical2D(intervalTo(4)).start,
      vertical2D(intervalTo(3)) -> "Hello"
    )
    fixture3a.replace(
      vertical2D(interval(16, 19)) -> "World",
      vertical2D(interval(15, 20)) -> "World!"
    )
    val expectedData3a = testData(
      ("Hello", unbounded, intervalTo(3)),
      ("to", unbounded, interval(5, 14)),
      ("World!", unbounded, interval(15, 20))
    )
    fixture3a.getAll.toList shouldBe expectedData3a

    fixture.set(vertical2D(intervalFrom(20)) -> "World")
    // needed? fixture.recompressAll()
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
    // needed? fixture.recompressAll()
    val expectedData6 = testData(
      ("Hey", unbounded, intervalTo(0)),
      ("Hey", intervalTo(day(0)), interval(1, 4)),
      ("World", intervalTo(day(0)), intervalFrom(16))
    )
    fixture.getAll.toList shouldBe expectedData6

    val copyFixture6 = fixture.copy

    fixture.fill(DiscreteInterval2D.unbounded -> "Filled")
    val expectedFilled = testData(
      ("Hey", unbounded, intervalTo(0)),
      ("Hey", intervalTo(day(0)), interval(1, 4)),
      ("Filled", unbounded, interval(5, 15)),
      ("World", intervalTo(day(0)), intervalFrom(16)),
      ("Filled", intervalFrom(day(1)), interval(1, 4)),
      ("Filled", intervalFrom(day(1)), intervalFrom(16))
    )
    fixture.getAll.toList shouldBe expectedFilled

    import DiffAction2D.*
    import DiscreteDomain1D.{Bottom, Point}

    val actionsFrom2To4 = copyFixture4.diffActionsFrom(copyFixture2)
    actionsFrom2To4.toList shouldBe List(
      Create(vertical2D(intervalTo(4)) -> "Hey"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 0)),
      Update(vertical2D(intervalFrom(16)) -> "World"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 20)),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 26))
    )
    actionsFrom2To4.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction2D.Create((unbounded x intervalTo(4)) -> \"Hey\")",
      "DiffAction2D.Delete(Bottom x Point(0))",
      "DiffAction2D.Update((unbounded x intervalFrom(16)) -> \"World\")",
      "DiffAction2D.Delete(Bottom x Point(20))",
      "DiffAction2D.Delete(Bottom x Point(26))"
    )

    val actionsFrom4To6 = copyFixture6.diffActionsFrom(copyFixture4)
    actionsFrom4To6.toList shouldBe List(
      Update(vertical2D(intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x interval(1, 4)) -> "Hey"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, Point(5))),
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
    // needed? fixture.recompressAll()
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
      vertical2D(intervalTo(1)),
      vertical2D(intervalFrom(10)),
      intervalFrom(day(0)) x interval(2, 9)
    )

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
