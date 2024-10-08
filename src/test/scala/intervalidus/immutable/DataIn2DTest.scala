package intervalidus.immutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn2DTest extends AnyFunSuite with Matchers with DataIn2DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Immutable", DataIn2D(_), DataIn2D.of(_)))
  testsFor(
    stringLookupTests("Immutable [experimental noSearchTree]", DataIn2D(_), DataIn2D.of(_))(using
      Experimental("noSearchTree")
    )
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData2D[String, LocalDate, Int]*
  )(
    removeOrUpdateInterval: DiscreteInterval2D[LocalDate, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val rectangle = interval(day(-14), day(14)) x interval(4, 7)
    val fixture = DataIn2D.of(rectangle -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ rectangle match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the rectangle")

    val removeExpected = removeExpectedUnsorted.toList.sortBy(_.key)
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sortBy(_.key)
    val removeFixture = fixture.remove(removeOrUpdateInterval).recompressAll()
    val updateFixture = fixture.update(removeOrUpdateInterval -> updateValue).recompressAll()
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

  testsFor(removeOrUpdateTests("Immutable"))
  testsFor(removeOrUpdateTests("Immutable [experimental noSearchTree]")(using Experimental("noSearchTree")))
  testsFor(removeOrUpdateTests("Immutable [experimental bruteForceUpdate]")(using Experimental("bruteForceUpdate")))

  def vertical2D[T: DiscreteValue](interval2: DiscreteInterval1D[T]): DiscreteInterval2D[LocalDate, T] =
    unbounded[LocalDate] x interval2

  test("Immutable: Looking up data in intervals - bounded r1"):
    val empty: DataIn2D[String, Int, Int] = DataIn2D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData =
      testData(("Hello", intervalTo(day(14)), interval(0, 10)), ("World", intervalFrom(day(1)), intervalFrom(11)))
    val fixture = mutable.DataIn2D(allData).toMutable.toImmutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")

    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Immutable [experimental noSearchTree]: Looking up data in intervals - bounded r1"):
    given Experimental = Experimental("noSearchTree")

    val allData =
      testData(("Hello", intervalTo(day(14)), interval(0, 10)), ("World", intervalFrom(day(1)), intervalFrom(11)))
    val fixture = mutable.DataIn2D(allData).toMutable.toImmutable

    fixture.getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")

    fixture.getByVerticalIndex(11).getAt(day(1)) shouldBe Some("World")

  test("Immutable: Adding and removing data in intervals - unbounded r1"):
    val allData = testData(("Hello", unbounded, interval(0, 10)), ("World", unbounded, intervalFrom(11)))
    val fixture0 = DataIn2D(allData)

    val fixture1 = fixture0.set(vertical2D(interval(5, 15)) -> "to")
    val expectedData1 = testData(
      ("Hello", unbounded, interval(0, 4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, intervalFrom(16))
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2a = fixture1.set(vertical2D(interval(20, 25)) -> "!") // split
    val expectedData2 = testData(
      ("Hello", unbounded, interval(0, 4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, interval(16, 19)),
      ("!", unbounded, interval(20, 25)),
      ("World", unbounded, intervalFrom(26))
    )
    fixture2a.getAll.toList shouldBe expectedData2

    val fixture2b = fixture2a.setIfNoConflict(vertical2D(intervalTo(-1)) -> "Hey") match
      case Some(f) =>
        f.getAll.toList shouldBe (vertical2D(intervalTo(-1)) -> "Hey") :: expectedData2
        f.setIfNoConflict(vertical2D(intervalTo(-1)) -> "Hey") shouldBe None
        f
      case None => fail("unexpected conflict")

    val fixture3 = fixture2b
      .set(vertical2D(intervalTo(4)) -> "Hey")
      .remove(vertical2D(intervalFrom(21)))
    val expectedData3 = testData(
      ("Hey", unbounded, intervalTo(4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, interval(16, 19)),
      ("!", unbounded, intervalAt(20))
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture3a = fixture3
      .replaceByKey(
        vertical2D(intervalTo(4)).start,
        vertical2D(intervalTo(3)) -> "Hello"
      )
      .replace(
        vertical2D(interval(16, 19)) -> "World",
        vertical2D(interval(15, 20)) -> "World!"
      )
    val expectedData3a = testData(
      ("Hello", unbounded, intervalTo(3)),
      ("to", unbounded, interval(5, 14)),
      ("World!", unbounded, interval(15, 20))
    )
    fixture3a.getAll.toList shouldBe expectedData3a

    val fixture4 = fixture3
      .set(vertical2D(intervalFrom(20)) -> "World")
    // needed? .recompressAll()
    val expectedData4 = testData(
      ("Hey", unbounded, intervalTo(4)),
      ("to", unbounded, interval(5, 15)),
      ("World", unbounded, intervalFrom(16))
    )
    fixture4.getAll.toList shouldBe expectedData4

    val fixture5 = fixture4.remove(vertical2D(interval(5, 15)))
    val expectedData5 = testData(("Hey", unbounded, intervalTo(4)), ("World", unbounded, intervalFrom(16)))
    fixture5.getAll.toList shouldBe expectedData5

    val fixture6 = fixture5
      .set((intervalFrom(day(1)) x intervalFrom(1)) -> "remove me")
      .remove(intervalFrom(day(1)) x intervalFrom(1))
    // needed? .recompressAll()
    val expectedData6 = testData(
      ("Hey", unbounded, intervalTo(0)),
      ("Hey", intervalTo(day(0)), interval(1, 4)),
      ("World", intervalTo(day(0)), intervalFrom(16))
    )
    fixture6.getAll.toList shouldBe expectedData6

    import DiffAction2D.*
    import DiscreteDomain1D.{Bottom, Point}

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2a)
    actionsFrom2To4.toList shouldBe List(
      Create(vertical2D(intervalTo(4)) -> "Hey"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 0)),
      Update(vertical2D(intervalFrom(16)) -> "World"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 20)),
      Delete(DiscreteDomain2D[Int, Int](Bottom, 26))
    )
    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6.toList shouldBe List(
      Update(vertical2D(intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x interval(1, 4)) -> "Hey"),
      Delete(DiscreteDomain2D[Int, Int](Bottom, Point(5))),
      Update((intervalTo(day(0)) x intervalFrom(16)) -> "World")
    )

    val fixture2AppDiff = fixture2a.applyDiffActions(actionsFrom2To4)
    fixture2AppDiff.getAll.toList shouldBe expectedData4

    val fixture2Synced = fixture2a.syncWith(fixture6)
    fixture2Synced.getAll.toList shouldBe expectedData6

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = testData(("Hey", unbounded, intervalTo(4)), ("World", unbounded, intervalFrom(16)))

    val fixture1 = DataIn2D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.vertical.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 = fixture1.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.withVerticalUpdate(_.endingWith(d.interval.vertical.end.successor))
      )
    )
    val expectedData2 = testData(("Hey!", unbounded, intervalTo(5)), ("World!", unbounded, intervalFrom(16)))
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = testData(("Hey!!!", unbounded, intervalTo(5)), ("World!!!", unbounded, intervalFrom(16)))
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 =
      fixture3.flatMap(d => DataIn2D.of[String, LocalDate, Int](d.value).map(x => d.interval -> x.value))
    val expectedData4 = testData(("Hey!!!", unbounded, intervalTo(5)), ("World!!!", unbounded, intervalFrom(16)))
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.interval.vertical ⊆ intervalTo(10))
    val expectedData5 = testData(("Hey!!!", unbounded, intervalTo(5)))
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataIn2D.of[String, LocalDate, Int](d.value))
    val expectedData6 = testData(("Hey!!!", unbounded, unbounded))
    fixture6.getAll.toList shouldBe expectedData6
    fixture6.get shouldBe "Hey!!!"

    val fixture7 = fixture6.filter(_.value == "Planet")
    assert(fixture7.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture7.get

  test("Immutable: Compressing data in intervals - unbounded r1"):
    val allData = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, intervalAt(5)),
      ("World", unbounded, intervalAt(6)),
      ("Hello", unbounded, intervalAt(7)),
      ("Hello", unbounded, interval(8, 9)),
      ("Hello", unbounded, intervalFrom(10))
    )

    val fixture0 = DataIn2D(allData)
    fixture0.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])
    val fixture1 = fixture0.compress("Hello")
    val expectedData1 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, intervalAt(5)),
      ("World", unbounded, intervalAt(6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    fixture1.getAll.toList shouldBe expectedData1
    fixture1.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

    val fixture2 = DataIn2D(allData)
      .compressAll()
    val expectedData2 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, interval(5, 6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(DiscreteInterval2D.unbounded[Int, Int])

  test("Immutable: Updating data in intervals - unbounded r1"):
    val allData = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World", unbounded, interval(5, 6)),
      ("Hello", unbounded, intervalFrom(7))
    )
    val fixture0 = DataIn2D(allData)

    val fixture1 = fixture0.update(vertical2D(interval(5, 7)) -> "World!")
    val expectedData1 = testData(
      ("Hello", unbounded, intervalTo(4)),
      ("World!", unbounded, interval(5, 7)),
      ("Hello", unbounded, intervalFrom(8))
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .remove(vertical2D(interval(3, 5)))
      .update(vertical2D(interval(2, 9)) -> "to")
    val expectedData2 = testData(
      ("Hello", unbounded, intervalTo(1)),
      ("to", unbounded, intervalAt(2)),
      ("to", unbounded, interval(6, 9)),
      ("Hello", unbounded, intervalFrom(10))
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(
      unbounded[Int] x intervalTo(2),
      unbounded[Int] x intervalFrom(6)
    )

    val fixture3 = fixture2
      .remove(vertical2D(interval(2, 9)))
      .update(vertical2D(interval(-5, -2)) -> "World!")
    val expectedData3 = testData(
      ("Hello", unbounded, intervalTo(-6)),
      ("World!", unbounded, interval(-5, -2)),
      ("Hello", unbounded, interval(-1, 1)),
      ("Hello", unbounded, intervalFrom(10))
    )
    fixture3.getAll.toList shouldBe expectedData3
    fixture3.domain.toList shouldBe List(
      unbounded[Int] x intervalTo(1),
      unbounded[Int] x intervalFrom(10)
    )

    val fixture4 = fixture3
      .set((intervalFrom(day(0)) x intervalFrom(1)) -> "update me")
      .update((intervalFrom(day(1)) x intervalFrom(0)) -> "updated me")
    // needed? .recompressAll()
    val expectedData4 = testData(
      ("Hello", unbounded, intervalTo(-6)),
      ("World!", unbounded, interval(-5, -2)),
      ("Hello", unbounded, intervalAt(-1)),
      ("Hello", intervalTo(day(0)), intervalAt(0)),
      ("Hello", intervalTo(day(-1)), intervalAt(1)),
      ("Hello", intervalTo(day(-1)), intervalFrom(10)),
      ("update me", intervalAt(day(0)), intervalFrom(1)),
      ("updated me", intervalFrom(day(1)), intervalFrom(0))
    )
    fixture4.getAll.toList shouldBe expectedData4
    fixture4.domain.toList shouldBe List(
      vertical2D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical2D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x interval(2, 9) // the remaining bits of the 7th and 8th
    )

  test("Immutable: Simple toString"):
    val fixturePadData = DataIn2D
      .of[String, LocalDate, Int]("H")
      .set((intervalFrom(day(0)) x unbounded[Int]) -> "W")
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 2024-07-14 | 2024-07-15 .. +∞ |
         || H (-∞..+∞)       |
         |                   | W (-∞..+∞)       |
         |""".stripMargin.replaceAll("\r", "")

    val fixturePadLabel = DataIn2D
      .of[String, LocalDate, Int]("Helloooooooooo")
      .set((intervalFrom(day(0)) x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 2024-07-14        | 2024-07-15 .. +∞        |
         || Helloooooooooo (-∞..+∞) |
         |                          | Wooooooorld (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")
