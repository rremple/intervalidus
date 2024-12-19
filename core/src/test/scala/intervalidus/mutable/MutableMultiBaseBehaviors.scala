package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on the mutable base trait methods (do not differ in 1D, 2D, or 3D).
 */
trait MutableMultiBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import DiscreteInterval1D.*

  def addAndRemoveTests[
    D: DiscreteDomainLike,
    I <: DiscreteIntervalLike[D, I],
    ValidDataOne <: ValidDataLike[String, D, I, ValidDataOne],
    ValidData <: ValidDataLike[Set[String], D, I, ValidData],
    DiffAction: DiffActionLike,
    S <: MutableMultiBase[String, D, I, ValidDataOne, ValidData, DiffAction, S] &
      DimensionalBase[Set[String], D, I, ValidData, DiffAction, ?]
  ](
    multiFrom: Experimental ?=> Iterable[ValidDataOne] => S,
    withHorizontalOne: (DiscreteInterval1D[Int], String) => ValidDataOne,
    withHorizontal: (DiscreteInterval1D[Int], Set[String]) => ValidData
  )(using Experimental): Unit =
    test("Mutable: Element-wise adding and removing data in intervals"):
      val allData = List(
        withHorizontalOne(interval(0, 20), "A"),
        withHorizontalOne(interval(5, 25), "B"),
        withHorizontalOne(interval(10, 30), "C"),
        withHorizontalOne(interval(15, 35), "D")
      )
      val f0: S = multiFrom(allData)
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(0, 4), Set("A")),
        withHorizontal(interval(5, 9), Set("A", "B")),
        withHorizontal(interval(10, 14), Set("A", "B", "C")),
        withHorizontal(interval(15, 20), Set("A", "B", "C", "D")),
        withHorizontal(interval(21, 25), Set("B", "C", "D")),
        withHorizontal(interval(26, 30), Set("C", "D")),
        withHorizontal(interval(31, 35), Set("D"))
      )

      f0.removeOne(withHorizontalOne(intervalTo(18), "A"))
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(5, 9), Set("B")),
        withHorizontal(interval(10, 14), Set("B", "C")),
        withHorizontal(interval(15, 18), Set("B", "C", "D")),
        withHorizontal(interval(19, 20), Set("A", "B", "C", "D")),
        withHorizontal(interval(21, 25), Set("B", "C", "D")),
        withHorizontal(interval(26, 30), Set("C", "D")),
        withHorizontal(interval(31, 35), Set("D"))
      )

      f0.addOne(withHorizontalOne(intervalFrom(17), "A"))
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(5, 9), Set("B")),
        withHorizontal(interval(10, 14), Set("B", "C")),
        withHorizontal(interval(15, 16), Set("B", "C", "D")),
        withHorizontal(interval(17, 25), Set("A", "B", "C", "D")),
        withHorizontal(interval(26, 30), Set("A", "C", "D")),
        withHorizontal(interval(31, 35), Set("A", "D")),
        withHorizontal(intervalFrom(36), Set("A"))
      )

      f0.set(withHorizontal(interval(15, 20), Set("A", "B", "C")))
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(5, 9), Set("B")),
        withHorizontal(interval(10, 14), Set("B", "C")),
        withHorizontal(interval(15, 20), Set("A", "B", "C")),
        withHorizontal(interval(21, 25), Set("A", "B", "C", "D")),
        withHorizontal(interval(26, 30), Set("A", "C", "D")),
        withHorizontal(interval(31, 35), Set("A", "D")),
        withHorizontal(intervalFrom(36), Set("A"))
      )

      val mergeData = List(
        withHorizontalOne(interval(0, 14), "X"),
        withHorizontalOne(interval(5, 16), "Y"),
        withHorizontalOne(intervalFrom(26), "Z")
      )
      f0.merge(multiFrom(mergeData))
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(0, 4), Set("X")),
        withHorizontal(interval(5, 9), Set("B", "X", "Y")),
        withHorizontal(interval(10, 14), Set("B", "C", "X", "Y")),
        withHorizontal(interval(15, 16), Set("A", "B", "C", "Y")),
        withHorizontal(interval(17, 20), Set("A", "B", "C")),
        withHorizontal(interval(21, 25), Set("A", "B", "C", "D")),
        withHorizontal(interval(26, 30), Set("A", "C", "D", "Z")),
        withHorizontal(interval(31, 35), Set("A", "D", "Z")),
        withHorizontal(intervalFrom(36), Set("A", "Z"))
      )

      f0.removeAll(
        Seq(
          withHorizontalOne(intervalTo(22), "A"),
          withHorizontalOne(intervalTo(22), "B"),
          withHorizontalOne(intervalTo(22), "C")
        )
      )
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(0, 4), Set("X")),
        withHorizontal(interval(5, 14), Set("X", "Y")),
        withHorizontal(interval(15, 16), Set("Y")),
        withHorizontal(interval(21, 22), Set("D")),
        withHorizontal(interval(23, 25), Set("A", "B", "C", "D")),
        withHorizontal(interval(26, 30), Set("A", "C", "D", "Z")),
        withHorizontal(interval(31, 35), Set("A", "D", "Z")),
        withHorizontal(intervalFrom(36), Set("A", "Z"))
      )

  def mapAndFlatmapTests[
    D: DiscreteDomainLike,
    I <: DiscreteIntervalLike[D, I],
    ValidDataOne <: ValidDataLike[String, D, I, ValidDataOne],
    ValidData <: ValidDataLike[Set[String], D, I, ValidData],
    DiffAction: DiffActionLike,
    S <: MutableMultiBase[String, D, I, ValidDataOne, ValidData, DiffAction, S] &
      DimensionalBase[Set[String], D, I, ValidData, DiffAction, ?]
  ](
    multiFrom: Experimental ?=> Iterable[ValidDataOne] => S,
    withHorizontalOne: (DiscreteInterval1D[Int], String) => ValidDataOne,
    withHorizontal: (DiscreteInterval1D[Int], Set[String]) => ValidData,
    mapF: ValidData => ValidData,
    flatMapF: ValidData => S
  )(using Experimental): Unit =
    test("Mutable: Mapping, flatmapping, etc."):
      val allData = List(
        withHorizontalOne(intervalTo(4), "Hey"),
        withHorizontalOne(intervalFrom(16), "World")
      )

      val fixture: S = multiFrom(allData)
      fixture.map(mapF)
      val expectedData2 = List(
        withHorizontal(intervalTo(5), Set("Hey!")),
        withHorizontal(intervalFrom(16), Set("World!"))
      )
      fixture.getAll.toList shouldBe expectedData2

      fixture.mapValues(_.map(_ + "!!"))
      val expectedData3 = List(
        withHorizontal(intervalTo(5), Set("Hey!!!")),
        withHorizontal(intervalFrom(16), Set("World!!!"))
      )
      fixture.getAll.toList shouldBe expectedData3

      fixture.flatMap(flatMapF)
      val expectedData4 = List(
        withHorizontal(intervalTo(5), Set("Hey!!!")),
        withHorizontal(intervalFrom(16), Set("World!!!"))
      )
      fixture.getAll.toList shouldBe expectedData4
