package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/**
  * Behaviors that only depend on the mutable base trait methods (do not differ in 1D, 2D, or 3D).
  *
  * Methods tested here: set, setIfNoConflict, replace, replaceByKey, remove, fill, update, filter, compress,
  * compressAll, map, mapValues, flatMap
  *
  * Methods not tested here: applyDiffActions, syncWith, recompressAll
  */
trait MutableBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import DiscreteInterval1D.*

  def mutableBaseTests[
    D: DiscreteDomainLike,
    I <: DiscreteIntervalLike[D, I],
    ValidData <: ValidDataLike[String, D, I, ValidData],
    DiffAction: DiffActionLike,
    S <: MutableBase[String, D, I, ValidData, DiffAction, S] & DimensionalBase[String, D, I, ValidData, DiffAction, ?]
  ](
    mutableFrom: Experimental ?=> Iterable[ValidData] => S,
    intervalFrom1D: DiscreteInterval1D[Int] => I,
    dataFrom1D: (DiscreteInterval1D[Int], String) => ValidData,
    mapF: ValidData => ValidData,
    fullyUnbound: I
  )(using Experimental): Unit =
    test("Mutable: Adding and removing data in intervals"):
      val allData = List(
        dataFrom1D(interval(0, 10), "Hello"),
        dataFrom1D(intervalFrom(11), "World")
      )
      val fixture: S = mutableFrom(allData)

      fixture.set(dataFrom1D(interval(5, 15), "to"))
      val expectedData1 = List(
        dataFrom1D(interval(0, 4), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture.getAll.toList shouldBe expectedData1

      fixture.set(dataFrom1D(interval(20, 25), "!")) // split
      val expectedData2 = List(
        dataFrom1D(interval(0, 4), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(interval(16, 19), "World"),
        dataFrom1D(interval(20, 25), "!"),
        dataFrom1D(intervalFrom(26), "World")
      )
      fixture.getAll.toList shouldBe expectedData2

      fixture.setIfNoConflict(dataFrom1D(intervalTo(4), "Hey")) shouldBe false
      fixture.setIfNoConflict(dataFrom1D(intervalTo(-1), "Hey")) shouldBe true
      fixture.getAll.toList shouldBe (dataFrom1D(intervalTo(-1), "Hey") :: expectedData2)

      fixture.set(dataFrom1D(intervalTo(4), "Hey"))
      fixture.remove(intervalFrom1D(intervalFrom(21)))
      val expectedData3 = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(interval(16, 19), "World"),
        dataFrom1D(intervalAt(20), "!")
      )
      fixture.getAll.toList shouldBe expectedData3
      val fixture3a = mutableFrom(fixture.getAll)

      fixture3a
        .replaceByKey(
          intervalFrom1D(intervalTo(4)).start,
          dataFrom1D(intervalTo(3), "Hello")
        )
      fixture3a.replace(
        dataFrom1D(interval(16, 19), "World"),
        dataFrom1D(interval(15, 20), "World!")
      )
      val expectedData3a = List(
        dataFrom1D(intervalTo(3), "Hello"),
        dataFrom1D(interval(5, 14), "to"),
        dataFrom1D(interval(15, 20), "World!")
      )
      fixture3a.getAll.toList shouldBe expectedData3a

      fixture.set(dataFrom1D(intervalFrom(20), "World"))
      // needed? .recompressAll()
      val expectedData4 = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture.getAll.toList shouldBe expectedData4
      fixture.remove(intervalFrom1D(interval(5, 15)))
      val expectedData5 = List(dataFrom1D(intervalTo(4), "Hey"), dataFrom1D(intervalFrom(16), "World"))
      fixture.getAll.toList shouldBe expectedData5

      fixture.fill(dataFrom1D(unbounded, "Filled"))
      val expectedFilled = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "Filled"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture.getAll.toList shouldBe expectedFilled

      val data1 = DataIn1D[String, Int](Seq(intervalFrom(1).to(3) -> "A", intervalFrom(5) -> "B"))
      val data2 = DataIn1D[String, Int](Seq(intervalFrom(2).to(4) -> "C", intervalFrom(6) -> "D"))
      // Default merge operation will "prioritize left"
      val defaultMerge = data1.copy
      defaultMerge.merge(data2)
      defaultMerge.getAll.toList shouldBe List(
        intervalFrom(1).to(3) -> "A",
        intervalAt(4) -> "C",
        intervalFrom(5) -> "B"
      )
      // Custom merge operation will combine overlapping elements
      val customMerge = data1.copy
      customMerge.merge(data2, _ + _)
      customMerge.getAll.toList shouldBe List(
        intervalAt(1) -> "A",
        intervalFrom(2).to(3) -> "AC",
        intervalAt(4) -> "C",
        intervalAt(5) -> "B",
        intervalFrom(6) -> "BD"
      )

    test("Mutable: Filter, mapping, flatmapping, etc."):
      val allData = List(dataFrom1D(intervalTo(4), "Hey"), dataFrom1D(intervalFrom(16), "World"))

      val fixture: S = mutableFrom(allData)
      fixture.copy.getAll.toList shouldBe fixture.getAll.toList

      fixture.map(mapF)
      val expectedData2 = List(dataFrom1D(intervalTo(5), "Hey!"), dataFrom1D(intervalFrom(16), "World!"))
      fixture.getAll.toList shouldBe expectedData2

      fixture.mapValues(_ + "!!")
      val expectedData3 = List(dataFrom1D(intervalTo(5), "Hey!!!"), dataFrom1D(intervalFrom(16), "World!!!"))
      fixture.getAll.toList shouldBe expectedData3

      fixture.flatMap(d => mutableFrom(Seq(d)))
      val expectedData4 = List(dataFrom1D(intervalTo(5), "Hey!!!"), dataFrom1D(intervalFrom(16), "World!!!"))
      fixture.getAll.toList shouldBe expectedData4
      assertThrows[NoSuchElementException]:
        fixture.get

      fixture.filter(_.interval ⊆ intervalFrom1D(intervalTo(10)))
      val expectedData5 = List(dataFrom1D(intervalTo(5), "Hey!!!"))
      fixture.getAll.toList shouldBe expectedData5
      assert(!fixture.isEmpty)
      assertThrows[NoSuchElementException]:
        fixture.get

      fixture.flatMap(d => mutableFrom(Seq(dataFrom1D(unbounded, d.value))))
      val expectedData6 = List(dataFrom1D(unbounded, "Hey!!!"))
      fixture.getAll.toList shouldBe expectedData6
      fixture.get shouldBe "Hey!!!"

      fixture.filter(_.value == "Planet")
      assert(fixture.isEmpty)
      assertThrows[NoSuchElementException]:
        fixture.get

    test("Mutable: Compressing data in intervals"):
      val allData = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalAt(6), "World"),
        dataFrom1D(intervalAt(7), "Hello"),
        dataFrom1D(interval(8, 9), "Hello"),
        dataFrom1D(intervalFrom(10), "Hello")
      )

      val fixture1: S = mutableFrom(allData)
      fixture1.domain.toList shouldBe List(fullyUnbound)
      fixture1.compress("Hello")
      val expectedData1 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalAt(6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture1.getAll.toList shouldBe expectedData1
      fixture1.domain.toList shouldBe List(fullyUnbound)

      val fixture2 = mutableFrom(allData)
      fixture2.compressAll()
      val expectedData2 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture2.getAll.toList shouldBe expectedData2
      fixture2.domain.toList shouldBe List(fullyUnbound)

    test("Mutable: Updating data in intervals"):
      val allData = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      val fixture: S = mutableFrom(allData)

      fixture.update(dataFrom1D(interval(5, 7), "World!"))
      val expectedData1 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 7), "World!"),
        dataFrom1D(intervalFrom(8), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData1

      fixture
        .remove(intervalFrom1D(interval(3, 5)))
      fixture.update(dataFrom1D(interval(2, 9), "to"))
      val expectedData2 = List(
        dataFrom1D(intervalTo(1), "Hello"),
        dataFrom1D(intervalAt(2), "to"),
        dataFrom1D(interval(6, 9), "to"),
        dataFrom1D(intervalFrom(10), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData2
      fixture.domain.toList shouldBe List(
        intervalFrom1D(intervalTo(2)),
        intervalFrom1D(intervalFrom(6))
      )

      fixture.remove(intervalFrom1D(interval(2, 9)))
      fixture.update(dataFrom1D(interval(-5, -2), "World!"))
      val expectedData3 = List(
        dataFrom1D(intervalTo(-6), "Hello"),
        dataFrom1D(interval(-5, -2), "World!"),
        dataFrom1D(interval(-1, 1), "Hello"),
        dataFrom1D(intervalFrom(10), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData3
      fixture.domain.toList shouldBe List(
        intervalFrom1D(intervalTo(1)),
        intervalFrom1D(intervalFrom(10))
      )
