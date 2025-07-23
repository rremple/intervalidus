package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on the immutable base trait methods (do not differ in 1D, 2D, or 3D).
 */
trait ImmutableMultiBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  def addAndRemoveTests[
    D <: NonEmptyTuple: DomainLike,
    S <: DataMulti[String, D]
  ](
    immutableMultiFrom: Experimental ?=> Iterable[ValidData[String, D]] => S,
    intervalFrom1D: Interval1D[Int] => Interval[D]
  )(using Experimental): Unit =
    def withHorizontalOne(interval: Interval1D[Int], value: String): ValidData[String, D] =
      intervalFrom1D(interval) -> value
    def withHorizontal(interval: Interval1D[Int], value: Set[String]): ValidData[Set[String], D] =
      intervalFrom1D(interval) -> value

    test("Immutable: Element-wise adding and removing data in intervals"):
      val allData = List(
        withHorizontalOne(interval(0, 20), "A"),
        withHorizontalOne(interval(5, 25), "B"),
        withHorizontalOne(interval(10, 30), "C"),
        withHorizontalOne(interval(15, 35), "D")
      )
      val f0: S = immutableMultiFrom(allData)
      f0.getAll.toList shouldBe List(
        withHorizontal(interval(0, 4), Set("A")),
        withHorizontal(interval(5, 9), Set("A", "B")),
        withHorizontal(interval(10, 14), Set("A", "B", "C")),
        withHorizontal(interval(15, 20), Set("A", "B", "C", "D")),
        withHorizontal(interval(21, 25), Set("B", "C", "D")),
        withHorizontal(interval(26, 30), Set("C", "D")),
        withHorizontal(interval(31, 35), Set("D"))
      )

      val f1 = f0.removeOne(withHorizontalOne(intervalTo(18), "A"))
      f1.getAll.toList shouldBe List(
        withHorizontal(interval(5, 9), Set("B")),
        withHorizontal(interval(10, 14), Set("B", "C")),
        withHorizontal(interval(15, 18), Set("B", "C", "D")),
        withHorizontal(interval(19, 20), Set("A", "B", "C", "D")),
        withHorizontal(interval(21, 25), Set("B", "C", "D")),
        withHorizontal(interval(26, 30), Set("C", "D")),
        withHorizontal(interval(31, 35), Set("D"))
      )

      val f2 = f1.addOne(withHorizontalOne(intervalFrom(17), "A"))
      f2.getAll.toList shouldBe List(
        withHorizontal(interval(5, 9), Set("B")),
        withHorizontal(interval(10, 14), Set("B", "C")),
        withHorizontal(interval(15, 16), Set("B", "C", "D")),
        withHorizontal(interval(17, 25), Set("A", "B", "C", "D")),
        withHorizontal(interval(26, 30), Set("A", "C", "D")),
        withHorizontal(interval(31, 35), Set("A", "D")),
        withHorizontal(intervalFrom(36), Set("A"))
      )

      val f3 = f2 + withHorizontal(interval(15, 20), Set("A", "B", "C"))
      f3.getAll.toList shouldBe List(
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
      val f4 = f3.concat(immutableMultiFrom(mergeData))
      f4.getAll.toList shouldBe List(
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

      val f5 = f4.removeAll(
        Seq(
          withHorizontalOne(intervalTo(22), "A"),
          withHorizontalOne(intervalTo(22), "B"),
          withHorizontalOne(intervalTo(22), "C")
        )
      )
      f5.getAll.toList shouldBe List(
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
    D <: NonEmptyTuple: DomainLike,
    S <: DataMulti[String, D]
  ](
    multiFrom: Experimental ?=> Iterable[ValidData[String, D]] => S,
    intervalFrom1D: Interval1D[Int] => Interval[D],
    mapF: ValidData[Set[String], D] => ValidData[Set[String], D],
    flatMapF: ValidData[Set[String], D] => S
  )(using Experimental): Unit =
    def withHorizontalOne(interval: Interval1D[Int], value: String): ValidData[String, D] =
      intervalFrom1D(interval) -> value
    def withHorizontal(interval: Interval1D[Int], value: Set[String]): ValidData[Set[String], D] =
      intervalFrom1D(interval) -> value
    test("Immutable: Mapping, flatmapping, etc."):
      val allData = List(
        withHorizontalOne(intervalTo(4), "Hey"),
        withHorizontalOne(intervalFrom(16), "World")
      )

      val f1: S = multiFrom(allData)
      val f2a = DataMulti.from(f1.map(mapF))
      val expectedData2 = List(
        withHorizontal(intervalTo(5), Set("Hey!")),
        withHorizontal(intervalFrom(16), Set("World!"))
      )
      f2a.getAll.toList shouldBe expectedData2
      val f2b = DataMulti.from(f1.mapIntervals(i => i.to(i.end.rightAdjacent)).mapValues(_.map(_ + "!")))
      f2b.getAll.toList shouldBe expectedData2

      val f3 = DataMulti.from(f2a.mapValues(_.map(_ + "!!")))
      val expectedData3 = List(
        withHorizontal(intervalTo(5), Set("Hey!!!")),
        withHorizontal(intervalFrom(16), Set("World!!!"))
      )
      f3.getAll.toList shouldBe expectedData3

      val f4 = DataMulti.from(f3.flatMap(flatMapF))
      val expectedData4 = List(
        withHorizontal(intervalTo(5), Set("Hey!!!")),
        withHorizontal(intervalFrom(16), Set("World!!!"))
      )
      f4.getAll.toList shouldBe expectedData4

      val f5a = f4.filter(_.value.contains("Hey!!!")).flatMap(flatMapF)
      val f5b = f4.collect:
        case d if d.value.contains("Hey!!!") => withHorizontal(intervalTo(5), Set("Hey!!!"))
      val expectedData5 = List(withHorizontal(intervalTo(5), Set("Hey!!!")))
      f5a.getAll.toList shouldBe expectedData5
      f5b.getAll.toList shouldBe expectedData5

  def applyingDiffActionTests[
    D <: NonEmptyTuple: DomainLike,
    S <: DataMulti[String, D]
  ](
    multiFrom: Experimental ?=> Iterable[ValidData[String, D]] => S,
    multiOf: Experimental ?=> ValidData[String, D] => S,
    intervalFrom1D: Interval1D[Int] => Interval[D]
  )(using Experimental): Unit =
    test("Immutable: Applying diff actions"):
      val fixture5 = multiFrom(
        List(
          intervalFrom1D(interval(0, 4)) -> "Hello",
          intervalFrom1D(interval(5, 15)) -> "to",
          intervalFrom1D(interval(16, 19)) -> "World",
          intervalFrom1D(interval(20, 25)) -> "!",
          intervalFrom1D(intervalFrom(26)) -> "World"
        )
      )

      val fixture6 = multiFrom(
        List(
          intervalFrom1D(intervalTo(4)) -> "Hey",
          intervalFrom1D(interval(5, 15)) -> "to",
          intervalFrom1D(intervalFrom(16)) -> "World"
        )
      )

      val fixture7 = multiOf(intervalFrom1D(intervalTo(0)) -> "Hey")

      val f6sync = fixture5.applyDiffActions(fixture6.diffActionsFrom(fixture5))
      f6sync.getAll.toList shouldBe fixture6.getAll.toList

      val f7sync = fixture5.syncWith(fixture7)
      f7sync.getAll.toList shouldBe fixture7.getAll.toList
