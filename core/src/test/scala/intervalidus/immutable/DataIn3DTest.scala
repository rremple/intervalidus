package intervalidus.immutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn3DTest extends AnyFunSuite with Matchers with DataIn3DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Immutable", DataIn3D(_), DataIn3D.of(_)))
  testsFor(
    stringLookupTests("Immutable [experimental]", DataIn3D(_), DataIn3D.of(_))(using Experimental("noSearchTree"))
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData3D[String, Int, Int, Int]*
  )(
    removeOrUpdateInterval: DiscreteInterval3D[Int, Int, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val cube = interval(-9, 9) x interval(-9, 9) x interval(-9, 9)
    val fixture = DataIn3D.of(cube -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ cube match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the cube")

    val removeExpected = removeExpectedUnsorted.toList.sorted
    val updateExpected = (removeExpectedUnsorted :+ (expectedUpdateInterval -> updateValue)).toList.sorted
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

  def vertical3D[T2: DiscreteValue](
    interval3: DiscreteInterval1D[T2]
  ): DiscreteInterval3D[LocalDate, LocalDate, T2] = unbounded[LocalDate] x unbounded[LocalDate] x interval3

  test("Immutable: Looking up data in intervals - bounded r1"):
    val empty: DataIn3D[String, Int, Int, Int] = DataIn3D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = mutable.DataIn3D(allData).toMutable.toImmutable

    fixture.getByHorizontalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByVerticalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByDepthIndex(11).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")

  test("Immutable: Simple toString"):
    val fixturePadData = DataIn3D
      .of[String, Int, Int, Int]("H")
      .set((intervalFrom(1) x intervalTo(0) x interval(1, 9)) -> "W")
      .recompressAll()
    // println(fixturePadData.toString)
    fixturePadData.toString shouldBe
      """|| -∞ .. 0               | 1 .. +∞               |
         |                        | W (-∞..0] x [1..9]    |
         || H (-∞..+∞) x (-∞..0]                          |
         || H (-∞..+∞) x [1..9]   |
         || H (-∞..+∞) x [10..+∞)                         |
         |                        | H [1..+∞) x [1..9]    |
         |""".stripMargin.replaceAll("\r", "")

    val fixturePadLabel = DataIn3D
      .of[String, Int, Int, Int]("Helloooooooooo")
      .set((intervalFrom(1) x unbounded[Int] x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 0                            | 1 .. +∞                            |
         || Helloooooooooo (-∞..+∞) x (-∞..+∞) |
         |                                     | Wooooooorld (-∞..+∞) x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

  test("Immutable: Adding and removing data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x interval(0, 10)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(11)) -> "World"
    )
    val fixture0 = DataIn3D(allData)

    val fixture1 = fixture0.set(vertical3D(interval(5, 15)) -> "to")
    val expectedData1 = List(
      (unboundedDate x unboundedDate x interval(0, 4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2a = fixture1.set(vertical3D(interval(20, 25)) -> "!") // split
    val expectedData2 = List(
      (unboundedDate x unboundedDate x interval(0, 4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19)) -> "World",
      (unboundedDate x unboundedDate x interval(20, 25)) -> "!",
      (unboundedDate x unboundedDate x intervalFrom(26)) -> "World"
    )
    fixture2a.getAll.toList shouldBe expectedData2

    val fixture2b = fixture2a.setIfNoConflict(vertical3D(intervalTo(-1)) -> "Hey") match
      case Some(f) =>
        f.getAll.toList shouldBe (vertical3D(intervalTo(-1)) -> "Hey") :: expectedData2
        f.setIfNoConflict(vertical3D(intervalTo(-1)) -> "Hey") shouldBe None
        f
      case None => fail("unexpected conflict")

    val fixture3 = fixture2b
      .set(vertical3D(intervalTo(4)) -> "Hey")
      .remove(vertical3D(intervalFrom(21)))
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(20)) -> "!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture3a = fixture3
      .replaceByKey(
        vertical3D(intervalTo(4)).start,
        vertical3D(intervalTo(3)) -> "Hello"
      )
      .replace(
        vertical3D(interval(16, 19)) -> "World",
        vertical3D(interval(15, 20)) -> "World!"
      )
    val expectedData3a = List(
      (unboundedDate x unboundedDate x intervalTo(3)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 14)) -> "to",
      (unboundedDate x unboundedDate x interval(15, 20)) -> "World!"
    )
    fixture3a.getAll.toList shouldBe expectedData3a

    val fixture4 = fixture3
      .set(vertical3D(intervalFrom(20)) -> "World")
    // needed? .recompressAll()
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture4.getAll.toList shouldBe expectedData4

    val fixture5 = fixture4.remove(vertical3D(interval(5, 15)))
    val expectedData5 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture5.getAll.toList shouldBe expectedData5

    val fixture6 = fixture5
      .set((intervalFrom(day(1)) x unboundedDate x intervalFrom(1)) -> "remove me")
      .remove(intervalFrom(day(1)) x unboundedDate x intervalFrom(1))
      .recompressAll()
    val expectedData6 = List(
      (unboundedDate x unboundedDate x intervalTo(0)) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x interval(1, 4)) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture6.getAll.toList shouldBe expectedData6

    import DiffAction3D.*
    import DiscreteDomain1D.{Bottom, Point}

    val actionsFrom2To4 = fixture4.diffActionsFrom(fixture2a)
    actionsFrom2To4.toList shouldBe List(
      Create(vertical3D(intervalTo(4)) -> "Hey"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 0)),
      Update(vertical3D(intervalFrom(16)) -> "World"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 20)),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 26))
    )
    val actionsFrom4To6 = fixture6.diffActionsFrom(fixture4)
    actionsFrom4To6.toList shouldBe List(
      Update((unboundedDate x unboundedDate x intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x unboundedDate x interval(1, 4)) -> "Hey"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, Point(5))),
      Update((intervalTo(day(0)) x unboundedDate x intervalFrom(16)) -> "World")
    )
    val fixture2AppDiff = fixture2a.applyDiffActions(actionsFrom2To4)
    fixture2AppDiff.getAll.toList shouldBe expectedData4

    val fixture2Synced = fixture2a.syncWith(fixture6)
    fixture2Synced.getAll.toList shouldBe expectedData6

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )

    val fixture1 = DataIn3D(allData)
    fixture1.copy.getAll.toList shouldBe fixture1.getAll.toList

    val concat = fixture1.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.depth.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    val fixture2 = fixture1.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.withDepthUpdate(_.endingWith(d.interval.depth.end.successor))
      )
    )
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!"
    )
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_ + "!!")
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!!!"
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 =
      fixture3.flatMap: d =>
        DataIn3D.of[String, LocalDate, LocalDate, Int](d.value).map(x => d.interval -> x.value)
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!!!"
    )
    fixture4.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture4.get

    val fixture5 = fixture4.filter(_.interval.depth ⊆ intervalTo(10))
    val expectedData5 = List((unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!")
    fixture5.getAll.toList shouldBe expectedData5
    assert(!fixture5.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture5.get

    val fixture6 = fixture5.flatMap(d => DataIn3D.of[String, LocalDate, LocalDate, Int](d.value))
    val expectedData6 = List((unboundedDate x unboundedDate x unbounded[Int]) -> "Hey!!!")
    fixture6.getAll.toList shouldBe expectedData6
    fixture6.get shouldBe "Hey!!!"

    val fixture7 = fixture6.filter(_.value == "Planet")
    assert(fixture7.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture7.get

  test("Immutable: Compressing data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x intervalAt(5)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(6)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(7)) -> "Hello",
      (unboundedDate x unboundedDate x interval(8, 9)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )

    val fixture0 = DataIn3D(allData)
    fixture0.domain.toList shouldBe List(DiscreteInterval3D.unbounded[Int, Int, Int])
    val fixture1 = fixture0.compress("Hello")
    val expectedData1 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x intervalAt(5)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(6)) -> "World",
      (unboundedDate x unboundedDate x intervalFrom(7)) -> "Hello"
    )
    fixture1.getAll.toList shouldBe expectedData1
    fixture1.domain.toList shouldBe List(DiscreteInterval3D.unbounded[Int, Int, Int])

    DiscreteInterval3D.isCompressible(allData.filter(_.value == "World").map(_.interval)) shouldBe true
    val fixture2 = DataIn3D(allData)
      .compressAll()
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 6)) -> "World",
      (unboundedDate x unboundedDate x intervalFrom(7)) -> "Hello"
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(DiscreteInterval3D.unbounded[Int, Int, Int])
    DiscreteInterval3D.isCompressible(fixture2.getAll.filter(_.value == "World").map(_.interval)) shouldBe false

  test("Immutable: Updating data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 6)) -> "World",
      (unboundedDate x unboundedDate x intervalFrom(7)) -> "Hello"
    )
    val fixture0 = DataIn3D(allData)

    val fixture1 = fixture0.update(vertical3D(interval(5, 7)) -> "World!")
    val expectedData1 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 7)) -> "World!",
      (unboundedDate x unboundedDate x intervalFrom(8)) -> "Hello"
    )
    fixture1.getAll.toList shouldBe expectedData1

    val fixture2 = fixture1
      .remove(vertical3D(interval(3, 5)))
      .update(vertical3D(interval(2, 9)) -> "to")
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(1)) -> "Hello",
      (unboundedDate x unboundedDate x intervalAt(2)) -> "to",
      (unboundedDate x unboundedDate x interval(6, 9)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(
      unboundedDate x unboundedDate x intervalTo(2),
      unboundedDate x unboundedDate x intervalFrom(6)
    )

    val fixture3 = fixture2
      .remove(vertical3D(interval(2, 9)))
      .update(vertical3D(interval(-5, -2)) -> "World!")
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(-6)) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2)) -> "World!",
      (unboundedDate x unboundedDate x interval(-1, 1)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )
    fixture3.getAll.toList shouldBe expectedData3
    fixture3.domain.toList shouldBe List(
      unboundedDate x unboundedDate x intervalTo(1),
      unboundedDate x unboundedDate x intervalFrom(10)
    )

    val fixture4 = fixture3
      .set((intervalFrom(day(0)) x unboundedDate x intervalFrom(1)) -> "update me")
      .update((intervalFrom(day(1)) x unboundedDate x intervalFrom(0)) -> "updated me")
      .recompressAll()
//    println("before")
//    println(fixture3)
//    println("after")
//    println(fixture4)
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(-6)) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2)) -> "World!",
      (unboundedDate x unboundedDate x intervalAt(-1)) -> "Hello",
      (intervalTo(day(0)) x unboundedDate x intervalAt(0)) -> "Hello",
      (intervalTo(day(-1)) x unboundedDate x intervalAt(1)) -> "Hello",
      (intervalTo(day(-1)) x unboundedDate x intervalFrom(10)) -> "Hello",
      (intervalAt(day(0)) x unboundedDate x intervalFrom(1)) -> "update me",
      (intervalFrom(day(1)) x unboundedDate x intervalFrom(0)) -> "updated me"
    )
    fixture4.getAll.toList shouldBe expectedData4
    fixture4.domain.toList shouldBe List(
      vertical3D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical3D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x unboundedDate x interval(2, 9) // the remaining bits of the 7th and 8th
    )
