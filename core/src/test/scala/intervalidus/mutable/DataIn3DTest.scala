package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataIn3DTest extends AnyFunSuite with Matchers with DataIn3DBaseBehaviors:

  import DiscreteInterval1D.*

  // shared
  testsFor(stringLookupTests("Mutable", DataIn3D(_), DataIn3D.of(_)))
  testsFor(
    stringLookupTests("Mutable [experimental noSearchTree]", DataIn3D(_), DataIn3D.of(_))(using
      Experimental("noSearchTree")
    )
  )

  override def assertRemoveOrUpdateResult(
    removeExpectedUnsorted: ValidData3D[String, Int, Int, Int]*
  )(
    removeOrUpdateInterval: DiscreteInterval3D[Int, Int, Int],
    updateValue: String = "update"
  )(using Experimental): Assertion =
    val cube = interval(-9, 9) x interval(-9, 9) x interval(-9, 9)
    val removeFixture = DataIn3D.of(cube -> "World")
    val updateFixture = DataIn3D.of(cube -> "World")
    val expectedUpdateInterval = removeOrUpdateInterval ∩ cube match
      case Some(intersection) => intersection
      case None               => fail("Test failed, no intersection with the cube")

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

  def vertical3D[T2: DiscreteValue](
    interval3: DiscreteInterval1D[T2]
  ): DiscreteInterval3D[LocalDate, LocalDate, T2] = unbounded[LocalDate] x unbounded[LocalDate] x interval3

  test("Mutable: Looking up data in intervals - bounded r1"):
    val empty: DataIn3D[String, Int, Int, Int] = DataIn3D()
    assert(empty.getAll.isEmpty)
    assert(empty.domain.isEmpty)

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = immutable.DataIn3D(allData).toImmutable.toMutable

    fixture.getByHorizontalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByVerticalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByDepthIndex(11).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")

  test("Mutable [experimental noSearchTree]: Looking up data in intervals - bounded r1"):
    given Experimental = Experimental("noSearchTree")

    val allData = List(
      (intervalTo(day(14)) x intervalTo(day(14)) x interval(0, 10)) -> "Hello",
      (intervalFrom(day(1)) x intervalFrom(day(1)) x intervalFrom(11)) -> "World"
    )
    val fixture = DataIn3D(allData)

    fixture.getByHorizontalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByVerticalIndex(dayZero).getByHorizontalIndex(dayZero).getAt(0) shouldBe Some("Hello")
    fixture.getByDepthIndex(11).getByHorizontalIndex(day(1)).getAt(day(1)) shouldBe Some("World")

    fixture.domain.toList.sorted

  test("Mutable: Simple toString"):
    val fixturePadData = DataIn3D
      .of[String, Int, Int, Int]("H")
    fixturePadData.set((intervalFrom(1) x intervalTo(0) x interval(1, 9)) -> "W")
    fixturePadData.recompressAll()
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
    fixturePadLabel.set((intervalFrom(1) x unbounded[Int] x unbounded[Int]) -> "Wooooooorld")
    // println(fixturePadLabel.toString)
    fixturePadLabel.toString shouldBe
      """|| -∞ .. 0                            | 1 .. +∞                            |
         || Helloooooooooo (-∞..+∞) x (-∞..+∞) |
         |                                     | Wooooooorld (-∞..+∞) x (-∞..+∞)    |
         |""".stripMargin.replaceAll("\r", "")

  test("Mutable: Adding and removing data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x interval(0, 10)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(11)) -> "World"
    )
    val fixture = DataIn3D(allData)

    fixture.set(vertical3D(interval(5, 15)) -> "to")
    val expectedData1 = List(
      (unboundedDate x unboundedDate x interval(0, 4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData1

    fixture.set(vertical3D(interval(20, 25)) -> "!") // split
    val expectedData2 = List(
      (unboundedDate x unboundedDate x interval(0, 4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19)) -> "World",
      (unboundedDate x unboundedDate x interval(20, 25)) -> "!",
      (unboundedDate x unboundedDate x intervalFrom(26)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData2

    val copyFixture2 = fixture.copy

    assert(!fixture.setIfNoConflict(vertical3D(intervalTo(4)) -> "Hey"))
    assert(fixture.setIfNoConflict(vertical3D(intervalTo(-1)) -> "Hey"))
    fixture.getAll.toList shouldBe (vertical3D(intervalTo(-1)) -> "Hey") :: expectedData2

    fixture.set(vertical3D(intervalTo(4)) -> "Hey")
    fixture.remove(vertical3D(intervalFrom(21)))
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x interval(16, 19)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(20)) -> "!"
    )
    fixture.getAll.toList shouldBe expectedData3

    val fixture3a = fixture.copy
    fixture3a.replaceByKey(
      vertical3D(intervalTo(4)).start,
      vertical3D(intervalTo(3)) -> "Hello"
    )
    fixture3a.replace(
      vertical3D(interval(16, 19)) -> "World",
      vertical3D(interval(15, 20)) -> "World!"
    )
    val expectedData3a = List(
      (unboundedDate x unboundedDate x intervalTo(3)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 14)) -> "to",
      (unboundedDate x unboundedDate x interval(15, 20)) -> "World!"
    )
    fixture3a.getAll.toList shouldBe expectedData3a

    fixture.set(vertical3D(intervalFrom(20)) -> "World")
    fixture.recompressAll()
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData4

    val copyFixture4 = fixture.copy

    fixture.remove(vertical3D(interval(5, 15)))
    val expectedData5 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData5

    fixture.set((intervalFrom(day(1)) x unboundedDate x intervalFrom(1)) -> "remove me")
    fixture.remove(intervalFrom(day(1)) x unboundedDate x intervalFrom(1))
    fixture.recompressAll()
    val expectedData6 = List(
      (unboundedDate x unboundedDate x intervalTo(0)) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x interval(1, 4)) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x intervalFrom(16)) -> "World"
    )
    fixture.getAll.toList shouldBe expectedData6

    val copyFixture6 = fixture.copy

    fixture.fill(DiscreteInterval3D.unbounded -> "Filled")
    val expectedFilled = List(
      (unboundedDate x unboundedDate x intervalTo(0)) -> "Hey",
      (intervalTo(day(0)) x unboundedDate x interval(1, 4)) -> "Hey",
      (unboundedDate x unboundedDate x interval(5, 15)) -> "Filled",
      (intervalTo(day(0)) x unboundedDate x intervalFrom(16)) -> "World",
      (intervalFrom(day(1)) x unboundedDate x interval(1, 4)) -> "Filled",
      (intervalFrom(day(1)) x unboundedDate x intervalFrom(16)) -> "Filled"
    )
    fixture.getAll.toList shouldBe expectedFilled

    import DiffAction3D.*
    import DiscreteDomain1D.{Bottom, Point}

    val actionsFrom2To4 = copyFixture4.diffActionsFrom(copyFixture2)
    actionsFrom2To4.toList shouldBe List(
      Create(vertical3D(intervalTo(4)) -> "Hey"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 0)),
      Update(vertical3D(intervalFrom(16)) -> "World"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 20)),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, 26))
    )
    actionsFrom2To4.toList.map(_.toCodeLikeString) shouldBe List(
      "DiffAction3D.Create((unbounded x unbounded x intervalTo(4)) -> \"Hey\")",
      "DiffAction3D.Delete(Bottom x Bottom x Point(0))",
      "DiffAction3D.Update((unbounded x unbounded x intervalFrom(16)) -> \"World\")",
      "DiffAction3D.Delete(Bottom x Bottom x Point(20))",
      "DiffAction3D.Delete(Bottom x Bottom x Point(26))"
    )

    val actionsFrom4To6 = copyFixture6.diffActionsFrom(copyFixture4)
    actionsFrom4To6.toList shouldBe List(
      Update((unboundedDate x unboundedDate x intervalTo(0)) -> "Hey"),
      Create((intervalTo(day(0)) x unboundedDate x interval(1, 4)) -> "Hey"),
      Delete(DiscreteDomain3D[Int, Int, Int](Bottom, Bottom, Point(5))),
      Update((intervalTo(day(0)) x unboundedDate x intervalFrom(16)) -> "World")
    )
    copyFixture2.applyDiffActions(actionsFrom2To4)
    copyFixture2.getAll.toList shouldBe expectedData4

    copyFixture2.syncWith(copyFixture6)
    copyFixture2.getAll.toList shouldBe expectedData6

  test("Mutable: Mapping, flatmapping, etc."):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hey",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World"
    )

    val fixture = DataIn3D(allData)
    fixture.copy.getAll.toList shouldBe fixture.getAll.toList

    val concat = fixture.foldLeft(StringBuilder()): (a, d) =>
      a.append(d.value).append("->").append(d.interval.depth.toString).append(" ")
    concat.result() shouldBe "Hey->(-∞..4] World->[16..+∞) "

    fixture.map(d =>
      d.copy(
        value = d.value + "!",
        interval = d.interval.withDepthUpdate(_.endingWith(d.interval.depth.end.successor))
      )
    )
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!"
    )
    fixture.getAll.toList shouldBe expectedData2

    fixture.mapValues(_ + "!!")
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!!!"
    )
    fixture.getAll.toList shouldBe expectedData3

    fixture.flatMap: d =>
      val result = DataIn3D.of[String, LocalDate, LocalDate, Int](d.value)
      result.map(x => d.interval -> x.value)
      result
    val expectedData4 = List(
      (unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!",
      (unboundedDate x unboundedDate x intervalFrom(16)) -> "World!!!"
    )
    fixture.getAll.toList shouldBe expectedData4
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.filter(_.interval.depth ⊆ intervalTo(10))
    val expectedData5 = List((unboundedDate x unboundedDate x intervalTo(5)) -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData5
    assert(!fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

    fixture.flatMap(d => DataIn3D.of[String, LocalDate, LocalDate, Int](d.value))
    val expectedData6 = List((unboundedDate x unboundedDate x unbounded[Int]) -> "Hey!!!")
    fixture.getAll.toList shouldBe expectedData6
    fixture.get shouldBe "Hey!!!"

    fixture.filter(_.value == "Planet")
    assert(fixture.isEmpty)
    assertThrows[NoSuchElementException]:
      fixture.get

  test("Mutable: Compressing data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x intervalAt(5)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(6)) -> "World",
      (unboundedDate x unboundedDate x intervalAt(7)) -> "Hello",
      (unboundedDate x unboundedDate x interval(8, 9)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )

    val fixture1 = DataIn3D(allData)
    fixture1.domain.toList shouldBe List(DiscreteInterval3D.unbounded[Int, Int, Int])
    fixture1.compress("Hello")
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
    fixture2.compressAll()
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 6)) -> "World",
      (unboundedDate x unboundedDate x intervalFrom(7)) -> "Hello"
    )
    fixture2.getAll.toList shouldBe expectedData2
    fixture2.domain.toList shouldBe List(DiscreteInterval3D.unbounded[Int, Int, Int])
    DiscreteInterval3D.isCompressible(fixture2.getAll.filter(_.value == "World").map(_.interval)) shouldBe false

  test("Mutable: Updating data in intervals - unbounded r1"):
    val allData = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 6)) -> "World",
      (unboundedDate x unboundedDate x intervalFrom(7)) -> "Hello"
    )
    val fixture = DataIn3D(allData)

    fixture.update(vertical3D(interval(5, 7)) -> "World!")
    val expectedData1 = List(
      (unboundedDate x unboundedDate x intervalTo(4)) -> "Hello",
      (unboundedDate x unboundedDate x interval(5, 7)) -> "World!",
      (unboundedDate x unboundedDate x intervalFrom(8)) -> "Hello"
    )
    fixture.getAll.toList shouldBe expectedData1

    fixture.remove(vertical3D(interval(3, 5)))
    fixture.update(vertical3D(interval(2, 9)) -> "to")
    val expectedData2 = List(
      (unboundedDate x unboundedDate x intervalTo(1)) -> "Hello",
      (unboundedDate x unboundedDate x intervalAt(2)) -> "to",
      (unboundedDate x unboundedDate x interval(6, 9)) -> "to",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )
    fixture.getAll.toList shouldBe expectedData2
    fixture.domain.toList shouldBe List(
      unboundedDate x unboundedDate x intervalTo(2),
      unboundedDate x unboundedDate x intervalFrom(6)
    )

    fixture.remove(vertical3D(interval(2, 9)))
    fixture.update(vertical3D(interval(-5, -2)) -> "World!")
    val expectedData3 = List(
      (unboundedDate x unboundedDate x intervalTo(-6)) -> "Hello",
      (unboundedDate x unboundedDate x interval(-5, -2)) -> "World!",
      (unboundedDate x unboundedDate x interval(-1, 1)) -> "Hello",
      (unboundedDate x unboundedDate x intervalFrom(10)) -> "Hello"
    )
    fixture.getAll.toList shouldBe expectedData3
    fixture.domain.toList shouldBe List(
      unboundedDate x unboundedDate x intervalTo(1),
      unboundedDate x unboundedDate x intervalFrom(10)
    )

    fixture.set((intervalFrom(day(0)) x unboundedDate x intervalFrom(1)) -> "update me")
    fixture.update((intervalFrom(day(1)) x unboundedDate x intervalFrom(0)) -> "updated me")
    fixture.recompressAll()
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
    fixture.getAll.toList shouldBe expectedData4
    fixture.domain.toList shouldBe List(
      vertical3D(intervalTo(1)), // the first 5 plus a bit of the 7th and 8th
      vertical3D(intervalFrom(10)), // the 6th plus a bit of the 7th and 8th
      intervalFrom(day(0)) x unboundedDate x interval(2, 9) // the remaining bits of the 7th and 8th
    )
