package intervalidus

import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn1DMultiBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*
  import Domain1D.Point

  def basicAndZipTests[S <: DimensionalMultiBase[String, Dim[Int]]](
    prefix: String,
    multiFrom: Experimental ?=> Iterable[ValidData[String, Dim[Int]]] => S,
    multiFrom1D: Experimental ?=> DimensionalBase[Set[String], Dim[Int]] => S,
    multiOf: Experimental ?=> String => S,
    multiApply: Experimental ?=> Iterable[ValidData[Set[String], Dim[Int]]] => S
  )(using Experimental): Unit =
    test(s"$prefix: Basics"):
      {
        given Experimental = Experimental("requireDisjoint")

        assertThrows[IllegalArgumentException]:
          // not valid as it overlaps on [10, +∞)
          val _ =
            multiApply(List(interval(0, 10) -> Set("Hello"), unbounded[Int] -> Set("World")))
      }

      val allData = List(interval(0, 9) -> Set("Hello"), intervalFrom(10) -> Set("World"))

      val fixture1 = multiApply(allData).toMutable.toImmutable.copy
      fixture1.getAll.toList shouldBe allData
      val fixture2 = multiApply(allData).toImmutable.toMutable.copy
      fixture2.getAll.toList shouldBe allData

      val fixture3 = multiFrom1D(immutable.Data.of[Set[String], Dim[Int]](Set("Hello", "world")))
      fixture3.get shouldBe Set("Hello", "world")

      val f0: S = multiFrom(
        List(
          interval(0, 20) -> "A",
          interval(5, 25) -> "B",
          interval(10, 30) -> "C"
        )
      )
      f0.valuesOne should contain theSameElementsAs Set("A", "B", "C")

      // println(f0.toString)
      f0.toString shouldBe
        """|| 0 .. 4   | 5 .. 9   | 10 .. 20 | 21 .. 25 | 26 .. 30 |
           || {A}      |
           |           | {A,B}    |
           |                      | {A,B,C}  |
           |                                 | {B,C}    |
           |                                            | {C}      |
           |""".stripMargin.replaceAll("\r", "")

      // Appropriately fails in one dimension because Tuple.Tail[D] is empty and therefore is not DomainLike
      """f0.getByHeadDimension(15)""" shouldNot typeCheck

    test(s"$prefix: Zipping"):
      multiOf("Hello").zip(multiOf("world")).get shouldBe (
        Set("Hello"),
        Set("world")
      )

      val allData3a = List(interval(0, 9) -> "Hello", interval(12, 20) -> "World")
      val allData3b = List(interval(-4, -2) -> "Goodbye", interval(6, 14) -> "Cruel", interval(16, 24) -> "World")

      val fixture3 = multiFrom(allData3a).zip(multiFrom(allData3b))
      val expected3 = List(
        interval(6, 9) -> (Set("Hello"), Set("Cruel")),
        interval(12, 14) -> (Set("World"), Set("Cruel")),
        interval(16, 20) -> (Set("World"), Set("World"))
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = multiFrom(allData3a).zipAll(multiFrom(allData3b), Set("<"), Set(">"))
      val expected4 = List(
        interval(-4, -2) -> (Set("<"), Set("Goodbye")),
        interval(0, 5) -> (Set("Hello"), Set(">")),
        interval(6, 9) -> (Set("Hello"), Set("Cruel")),
        interval(10, 11) -> (Set("<"), Set("Cruel")),
        interval(12, 14) -> (Set("World"), Set("Cruel")),
        intervalAt(15) -> (Set("World"), Set(">")),
        interval(16, 20) -> (Set("World"), Set("World")),
        interval(21, 24) -> (Set("<"), Set("World"))
      )
      fixture4.getAll.toList shouldBe expected4

    test(s"$prefix: Getting diff actions"):

      import DiffAction.*

      val fixture5Data = List(
        interval(0, 4) -> "Hello",
        interval(5, 15) -> "to",
        interval(16, 19) -> "World",
        interval(20, 25) -> "!",
        intervalFrom(26) -> "World"
      )
      val fixture5 = multiFrom(fixture5Data)

      val expectedWorldIntervals = fixture5Data.collect:
        case ValidData("World", interval) => interval
      fixture5.intervalsOne("World") should contain theSameElementsAs expectedWorldIntervals

      val fixture6 = multiFrom(
        List(
          intervalTo(4) -> "Hey",
          interval(5, 15) -> "to",
          intervalFrom(16) -> "World"
        )
      )

      val fixture7 = multiFrom(List(intervalTo(0) -> "Hey"))

      val actionsFrom5To6 = fixture6.diffActionsFrom(fixture5)
      actionsFrom5To6.toList shouldBe List(
        Create(intervalTo(4) -> Set("Hey")),
        Delete(Point(0)),
        Update(intervalFrom(16) -> Set("World")),
        Delete(Point(20)),
        Delete(Point(26))
      )

      val actionsFrom6To7 = fixture7.diffActionsFrom(fixture6)
      actionsFrom6To7.toList shouldBe List(
        Update(intervalTo(0) -> Set("Hey")),
        Delete(Point(5)),
        Delete(Point(16))
      )
