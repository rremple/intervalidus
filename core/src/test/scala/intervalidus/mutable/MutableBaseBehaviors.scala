package intervalidus.mutable

import intervalidus.*
import intervalidus.DomainLike.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/**
  * Behaviors that only depend on the mutable base trait methods (common in all dimensions).
  *
  * Methods tested here: set, setIfNoConflict, replace, replaceByKey, remove, fill, update, filter, compress,
  * compressAll, map, mapValues, flatMap
  *
  * Methods not tested here: applyDiffActions, syncWith, recompressAll
  */
trait MutableBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  def mutableBaseTests[
    D <: NonEmptyTuple: DomainLike,
    S <: Data[String, D]
  ](
    mutableFrom: Experimental ?=> Iterable[ValidData[String, D]] => S,
    intervalFrom1D: Interval1D[Int] => Interval[D],
    mapF: ValidData[String, D] => ValidData[String, D]
  )(using Experimental, DomainValueLike[Int]): Unit =
    def dataFrom1D(interval1D: Interval1D[Int], v: String): ValidData[String, D] =
      intervalFrom1D(interval1D) -> v

    test("Mutable: Adding and removing data in intervals"):
      val allData = List(
        dataFrom1D(interval(0, 10), "Hello"),
        dataFrom1D(intervalFrom(11), "World")
      )
      val fixture: S = mutableFrom(allData)

      fixture.set(dataFrom1D(interval(5, 15), "to"))
      val expectedData1 = List(
        dataFrom1D(intervalFrom(0).toBefore(5), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFromAfter(15), "World")
      )
      fixture.getAll.toList shouldBe expectedData1

      fixture.set(dataFrom1D(interval(20, 25), "!")) // split
      val expectedData2 = List(
        dataFrom1D(intervalFrom(0).toBefore(5), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFromAfter(15).toBefore(20), "World"),
        dataFrom1D(interval(20, 25), "!"),
        dataFrom1D(intervalFromAfter(25), "World")
      )
      fixture.getAll.toList shouldBe expectedData2

      fixture.setIfNoConflict(dataFrom1D(intervalTo(4), "Hey")) shouldBe false
      fixture.setIfNoConflict(dataFrom1D(intervalTo(-1), "Hey")) shouldBe true
      fixture.getAll.toList shouldBe (dataFrom1D(intervalTo(-1), "Hey") :: expectedData2)

      fixture.set(dataFrom1D(intervalTo(5), "Hey"))
      fixture.remove(intervalFrom1D(intervalFromAfter(20)))
      val expectedData3 = List(
        dataFrom1D(intervalTo(5), "Hey"),
        dataFrom1D(intervalFromAfter(5).to(15), "to"),
        dataFrom1D(intervalFromAfter(15).toBefore(20), "World"),
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
        dataFrom1D(intervalFromAfter(5).toBefore(15), "to"),
        dataFrom1D(interval(15, 20), "World!")
      )
      fixture3a.getAll.toList shouldBe expectedData3a

      fixture.set(dataFrom1D(intervalFrom(20), "World"))
      // if needed: .recompressAll()
      val expectedData4 = List(
        dataFrom1D(intervalTo(5), "Hey"),
        dataFrom1D(intervalFromAfter(5).to(15), "to"),
        dataFrom1D(intervalFromAfter(15), "World")
      )
      fixture.getAll.toList shouldBe expectedData4
      fixture.remove(intervalFrom1D(interval(5, 15)))
      val expectedData5 = List(dataFrom1D(intervalToBefore(5), "Hey"), dataFrom1D(intervalFromAfter(15), "World"))
      fixture.getAll.toList shouldBe expectedData5

      fixture.fill(dataFrom1D(unbounded, "Filled"))
      val expectedFilled = List(
        dataFrom1D(intervalToBefore(5), "Hey"),
        dataFrom1D(interval(5, 15), "Filled"),
        dataFrom1D(intervalFromAfter(15), "World")
      )
      fixture.getAll.toList shouldBe expectedFilled

      val data1 = Data(Seq(intervalFrom(1).to(3) -> "A", intervalFrom(5) -> "B"))
      val data2 = Data(Seq(intervalFrom(2).to(4) -> "C", intervalFrom(6) -> "D"))
      // Default merge operation will "prioritize left"
      val defaultMerge = data1.copy
      defaultMerge.merge(data2)
      defaultMerge.getAll.toList shouldBe List(
        intervalFrom(1).to(3) -> "A",
        intervalFromAfter(3).to(4) -> "C",
        intervalFrom(5) -> "B"
      )
      // Custom merge operation will combine overlapping elements
      val customMerge = data1.copy
      customMerge.merge(data2, _ + _)
      customMerge.getAll.toList shouldBe List(
        intervalFrom(1).toBefore(2) -> "A",
        intervalFrom(2).to(3) -> "AC",
        intervalFromAfter(3).to(4) -> "C",
        intervalFrom(5).toBefore(6) -> "B",
        intervalFrom(6) -> "BD"
      )

    test("Mutable: Filter, mapping, flatmapping, etc."):
      val allData = List(dataFrom1D(intervalTo(4), "Hey"), dataFrom1D(intervalFrom(16), "World"))

      val fixture: S = mutableFrom(allData)
      fixture.copy.getAll.toList shouldBe fixture.getAll.toList

      fixture.map(mapF)
      val expectedData2 = List(dataFrom1D(intervalToBefore(4), "Hey!"), dataFrom1D(intervalFrom(16), "World!"))
      fixture.getAll.toList shouldBe expectedData2

      fixture.mapValues(_ + "!!")
      val expectedData3 = List(dataFrom1D(intervalToBefore(4), "Hey!!!"), dataFrom1D(intervalFrom(16), "World!!!"))
      fixture.getAll.toList shouldBe expectedData3

      fixture.flatMap(d => mutableFrom(Seq(d)))
      val expectedData4 = List(dataFrom1D(intervalToBefore(4), "Hey!!!"), dataFrom1D(intervalFrom(16), "World!!!"))
      fixture.getAll.toList shouldBe expectedData4
      assertThrows[NoSuchElementException]:
        fixture.get

      val fixtureCollect = fixture.copy
      fixtureCollect.collect:
        case d if d.interval ⊆ intervalFrom1D(intervalTo(10)) => d
      fixture.filter(_.interval ⊆ intervalFrom1D(intervalTo(10)))
      val expectedData5 = List(dataFrom1D(intervalToBefore(4), "Hey!!!"))
      fixture.getAll.toList shouldBe expectedData5
      fixtureCollect.getAll.toList shouldBe expectedData5
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
        dataFrom1D(intervalToBefore(5), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalFromAfter(5).toBefore(7), "World"),
        dataFrom1D(intervalAt(7), "Hello"),
        dataFrom1D(intervalFromAfter(7).toBefore(10), "Hello"),
        dataFrom1D(intervalFrom(10), "Hello")
      )

      val fixture1: S = mutableFrom(allData)
      fixture1.domain.toList shouldBe List(Interval.unbounded[D])
      fixture1.domainComplement.toList shouldBe List.empty
      fixture1.compress("Hello")
      val expectedData1 = List(
        dataFrom1D(intervalToBefore(5), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalFromAfter(5).toBefore(7), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture1.getAll.toList shouldBe expectedData1
      fixture1.domain.toList shouldBe List(Interval.unbounded[D])
      fixture1.domainComplement.toList shouldBe List.empty

      val fixture2 = mutableFrom(allData)
      fixture2.compressAll()
      val expectedData2 = List(
        dataFrom1D(intervalToBefore(5), "Hello"),
        dataFrom1D(intervalFrom(5).toBefore(7), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture2.getAll.toList shouldBe expectedData2
      fixture2.domain.toList shouldBe List(Interval.unbounded[D])
      fixture2.domainComplement.toList shouldBe List.empty

    test("Mutable: Updating data in intervals"):
      val allData = List(
        dataFrom1D(intervalToBefore(5), "Hello"),
        dataFrom1D(interval(5, 6), "World"),
        dataFrom1D(intervalFromAfter(6), "Hello")
      )
      val fixture: S = mutableFrom(allData)

      fixture.update(dataFrom1D(interval(5, 7), "World!"))
      val expectedData1 = List(
        dataFrom1D(intervalToBefore(5), "Hello"),
        dataFrom1D(interval(5, 7), "World!"),
        dataFrom1D(intervalFromAfter(7), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData1

      fixture
        .remove(intervalFrom1D(interval(3, 5)))
      fixture.update(dataFrom1D(interval(2, 9), "to"))
      val expectedData2 = List(
        dataFrom1D(intervalToBefore(2), "Hello"),
        dataFrom1D(intervalFrom(2).toBefore(3), "to"),
        dataFrom1D(intervalFromAfter(5).to(9), "to"),
        dataFrom1D(intervalFromAfter(9), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData2
      fixture.domain.toList shouldBe List(
        intervalFrom1D(intervalToBefore(3)),
        intervalFrom1D(intervalFromAfter(5))
      )
      fixture.domainComplement.toList shouldBe List(
        intervalFrom1D(interval(3, 5))
      )
      Interval.compress(fixture.domain ++ fixture.domainComplement).toList shouldBe List(
        intervalFrom1D(unbounded[Int])
      )

      fixture.remove(intervalFrom1D(interval(2, 9)))
      fixture.update(dataFrom1D(interval(-5, -2), "World!"))
      val expectedData3 = List(
        dataFrom1D(intervalToBefore(-5), "Hello"),
        dataFrom1D(interval(-5, -2), "World!"),
        dataFrom1D(intervalFromAfter(-2).toBefore(2), "Hello"),
        dataFrom1D(intervalFromAfter(9), "Hello")
      )
      fixture.getAll.toList shouldBe expectedData3
      fixture.domain.toList shouldBe List(
        intervalFrom1D(intervalToBefore(2)),
        intervalFrom1D(intervalFromAfter(9))
      )
      fixture.domainComplement.toList shouldBe List(
        intervalFrom1D(interval(2, 9))
      )
