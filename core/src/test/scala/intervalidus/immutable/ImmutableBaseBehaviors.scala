package intervalidus.immutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/**
  * Behaviors that only depend on the immutable base trait methods (do not differ in 1D, 2D, or 3D).
  *
  * Methods tested here: set, setIfNoConflict, replace, replaceByKey, remove, fill, update, filter, compress,
  * compressAll
  *
  * Methods not tested here: applyDiffActions, syncWith, recompressAll
  */
trait ImmutableBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import DiscreteInterval1D.*

  def immutableBaseTests[
    D <: DiscreteDomainLike[D],
    I <: DiscreteIntervalLike[D, I],
    ValidData <: ValidDataLike[String, D, I, ValidData],
    DiffAction <: DiffActionLike[String, D, I, ValidData, DiffAction],
    S <: ImmutableBase[String, D, I, ValidData, DiffAction, S]
  ](
    immutableFrom: Experimental ?=> Iterable[ValidData] => S,
    intervalFrom1D: DiscreteInterval1D[Int] => I,
    dataFrom1D: (DiscreteInterval1D[Int], String) => ValidData,
    fullyUnbound: I
  )(using Experimental): Unit =
    test("Immutable: Adding and removing data in intervals"):
      val allData = List(
        dataFrom1D(interval(0, 10), "Hello"),
        dataFrom1D(intervalFrom(11), "World")
      )
      val fixture0: S = immutableFrom(allData)

      val fixture1 = fixture0.set(dataFrom1D(interval(5, 15), "to"))
      val expectedData1 = List(
        dataFrom1D(interval(0, 4), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture1.getAll.toList shouldBe expectedData1

      val fixture2a: S = fixture1.set(dataFrom1D(interval(20, 25), "!")) // split
      val expectedData2 = List(
        dataFrom1D(interval(0, 4), "Hello"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(interval(16, 19), "World"),
        dataFrom1D(interval(20, 25), "!"),
        dataFrom1D(intervalFrom(26), "World")
      )
      fixture2a.getAll.toList shouldBe expectedData2

      val fixture2b: S = fixture2a.setIfNoConflict(dataFrom1D(intervalTo(-1), "Hey")) match
        case Some(f) =>
          f.getAll.toList shouldBe (dataFrom1D(intervalTo(-1), "Hey") :: expectedData2)
          f.setIfNoConflict(dataFrom1D(intervalTo(-1), "Hey")) shouldBe None
          f
        case None => fail("unexpected conflict")

      val fixture3 = fixture2b
        .set(dataFrom1D(intervalTo(4), "Hey"))
        .remove(intervalFrom1D(intervalFrom(21)))
      val expectedData3 = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(interval(16, 19), "World"),
        dataFrom1D(intervalAt(20), "!")
      )
      fixture3.getAll.toList shouldBe expectedData3

      val fixture3a = fixture3
        .replaceByKey(
          intervalFrom1D(intervalTo(4)).start,
          dataFrom1D(intervalTo(3), "Hello")
        )
        .replace(
          dataFrom1D(interval(16, 19), "World"),
          dataFrom1D(interval(15, 20), "World!")
        )
      val expectedData3a = List(
        dataFrom1D(intervalTo(3), "Hello"),
        dataFrom1D(interval(5, 14), "to"),
        dataFrom1D(interval(15, 20), "World!")
      )
      fixture3a.getAll.toList shouldBe expectedData3a

      val fixture4: S = fixture3
        .set(dataFrom1D(intervalFrom(20), "World"))
      // needed? .recompressAll()
      val expectedData4 = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "to"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture4.getAll.toList shouldBe expectedData4
      val fixture5 = fixture4.remove(intervalFrom1D(interval(5, 15)))
      val expectedData5 = List(dataFrom1D(intervalTo(4), "Hey"), dataFrom1D(intervalFrom(16), "World"))
      fixture5.getAll.toList shouldBe expectedData5

      val fixture6 = fixture5.fill(dataFrom1D(unbounded, "Filled"))
      val expectedFilled = List(
        dataFrom1D(intervalTo(4), "Hey"),
        dataFrom1D(interval(5, 15), "Filled"),
        dataFrom1D(intervalFrom(16), "World")
      )
      fixture6.getAll.toList shouldBe expectedFilled

    test("Immutable: Filter"):
      val expectedData4 = List(dataFrom1D(intervalTo(5), "Hey!!!"), dataFrom1D(intervalFrom(16), "World!!!"))
      val fixture4 = immutableFrom(expectedData4)
      val fixture5 = fixture4.filter(_.interval ⊆ intervalFrom1D(intervalTo(10)))
      val expectedData5 = List(dataFrom1D(intervalTo(5), "Hey!!!"))
      fixture5.getAll.toList shouldBe expectedData5
      assert(!fixture5.isEmpty)
      assertThrows[NoSuchElementException]:
        fixture5.get
      val fixture7 = fixture5.filter(_.value == "Planet")
      assert(fixture7.isEmpty)
      assertThrows[NoSuchElementException]:
        fixture7.get

    test("Immutable: Compressing data in intervals"):
      val allData = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalAt(6), "World"),
        dataFrom1D(intervalAt(7), "Hello"),
        dataFrom1D(interval(8, 9), "Hello"),
        dataFrom1D(intervalFrom(10), "Hello")
      )

      val fixture0: S = immutableFrom(allData)
      fixture0.domain.toList shouldBe List(fullyUnbound)
      val fixture1 = fixture0.compress("Hello")
      val expectedData1 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(intervalAt(5), "World"),
        dataFrom1D(intervalAt(6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture1.getAll.toList shouldBe expectedData1
      fixture1.domain.toList shouldBe List(fullyUnbound)

      val fixture2 = immutableFrom(allData)
        .compressAll()
      val expectedData2 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      fixture2.getAll.toList shouldBe expectedData2
      fixture2.domain.toList shouldBe List(fullyUnbound)

    test("Immutable: Updating data in intervals"):
      val allData = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 6), "World"),
        dataFrom1D(intervalFrom(7), "Hello")
      )
      val fixture0: S = immutableFrom(allData)

      val fixture1 = fixture0.update(dataFrom1D(interval(5, 7), "World!"))
      val expectedData1 = List(
        dataFrom1D(intervalTo(4), "Hello"),
        dataFrom1D(interval(5, 7), "World!"),
        dataFrom1D(intervalFrom(8), "Hello")
      )
      fixture1.getAll.toList shouldBe expectedData1

      val fixture2 = fixture1
        .remove(intervalFrom1D(interval(3, 5)))
        .update(dataFrom1D(interval(2, 9), "to"))
      val expectedData2 = List(
        dataFrom1D(intervalTo(1), "Hello"),
        dataFrom1D(intervalAt(2), "to"),
        dataFrom1D(interval(6, 9), "to"),
        dataFrom1D(intervalFrom(10), "Hello")
      )
      fixture2.getAll.toList shouldBe expectedData2
      fixture2.domain.toList shouldBe List(
        intervalFrom1D(intervalTo(2)),
        intervalFrom1D(intervalFrom(6))
      )

      val fixture3 = fixture2
        .remove(intervalFrom1D(interval(2, 9)))
        .update(dataFrom1D(interval(-5, -2), "World!"))
      val expectedData3 = List(
        dataFrom1D(intervalTo(-6), "Hello"),
        dataFrom1D(interval(-5, -2), "World!"),
        dataFrom1D(interval(-1, 1), "Hello"),
        dataFrom1D(intervalFrom(10), "Hello")
      )
      fixture3.getAll.toList shouldBe expectedData3
      fixture3.domain.toList shouldBe List(
        intervalFrom1D(intervalTo(1)),
        intervalFrom1D(intervalFrom(10))
      )