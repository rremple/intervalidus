package intervalidus

import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In4D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/*
 * Behaviors that only depend on base trait methods (do not differ if mutable or immutable).
 */
trait DataIn4DMultiBaseBehaviors:
  this: AnyFunSuite & Matchers =>

  import Interval1D.*

  def withHorizontal[T: DiscreteValue](i: Interval1D[T]): Interval[Dim[T, T, T, T]] =
    Interval.in4D[T, T, T, T](i, unbounded, unbounded, unbounded)

  def withHorizontal[T: DiscreteValue](d: Domain1D[T]): Dim[T, T, T, T] =
    d x Domain1D.Bottom x Domain1D.Bottom x Domain1D.Bottom

  def basicAndZipTests[S <: DimensionalMultiBase[String, Dim[Int, Int, Int, Int]]](
    prefix: String,
    multiFrom: Experimental ?=> Iterable[ValidData[String, Dim[Int, Int, Int, Int]]] => S,
    multiFrom4D: Experimental ?=> DimensionalBase[Set[String], Dim[Int, Int, Int, Int]] => S,
    multiOf: Experimental ?=> String => S,
    multiApply: Experimental ?=> Iterable[ValidData[Set[String], Dim[Int, Int, Int, Int]]] => S
  )(using Experimental): Unit =
    test(s"$prefix: Basics"):
      val allData = List(
        withHorizontal(interval(0, 9)) -> Set("Hello"),
        withHorizontal(intervalFrom(10)) -> Set("World")
      )

      val fixture1 = multiApply(allData).toMutable.toImmutable.copy
      fixture1.getAll.toList shouldBe allData
      val fixture2 = multiApply(allData).toImmutable.toMutable.copy
      fixture2.getAll.toList shouldBe allData

      val fixture3 = multiFrom4D(immutable.Data.of[Set[String], Dim[Int, Int, Int, Int]](Set("Hello", "world")))
      fixture3.get shouldBe Set("Hello", "world")

      val f0: S = multiFrom(
        List(
          withHorizontal(interval(0, 20)) -> "A",
          withHorizontal(interval(5, 25)) -> "B",
          withHorizontal(interval(10, 30)) -> "C"
        )
      )
      f0.valuesOne should contain theSameElementsAs Set("A", "B", "C")

      // println(f0.toString)
      // format: off
      f0.toString shouldBe
        """|| 0 .. 4                                 | 5 .. 9                                 | 10 .. 20                               | 21 .. 25                               | 26 .. 30                               |
           || {A} (-∞..+∞) x (-∞..+∞) x (-∞..+∞)     |
           |                                         | {A,B} (-∞..+∞) x (-∞..+∞) x (-∞..+∞)   |
           |                                                                                  | {A,B,C} (-∞..+∞) x (-∞..+∞) x (-∞..+∞) |
           |                                                                                                                           | {B,C} (-∞..+∞) x (-∞..+∞) x (-∞..+∞)   |
           |                                                                                                                                                                    | {C} (-∞..+∞) x (-∞..+∞) x (-∞..+∞)     |
           |""".stripMargin.replaceAll("\r", "")
      // format: on
      f0.getByHeadDimension(15).toString shouldBe
        """|| -∞ .. +∞                    |
           || {A,B,C} (-∞..+∞) x (-∞..+∞) |
           |""".stripMargin.replaceAll("\r", "")

      f0
        .getByDimension[Int, Domain.In3D[Int, Int, Int]](1, 15)
        .getByDimension[Int, Domain.In2D[Int, Int]](2, 15)
        .toString shouldBe
        """|| 0 .. 4           | 5 .. 9           | 10 .. 20         | 21 .. 25         | 26 .. 30         |
           || {A} (-∞..+∞)     |
           |                   | {A,B} (-∞..+∞)   |
           |                                      | {A,B,C} (-∞..+∞) |
           |                                                         | {B,C} (-∞..+∞)   |
           |                                                                            | {C} (-∞..+∞)     |
           |""".stripMargin.replaceAll("\r", "")

    test(s"$prefix: Zipping"):
      multiOf("Hello").zip(multiOf("world")).get shouldBe (
        Set("Hello"),
        Set("world")
      )

      val allData3a = List(
        withHorizontal(interval(0, 9)) -> "Hello",
        withHorizontal(interval(12, 20)) -> "World"
      )
      val allData3b = List(
        withHorizontal(interval(-4, -2)) -> "Goodbye",
        withHorizontal(interval(6, 14)) -> "Cruel",
        withHorizontal(interval(16, 24)) -> "World"
      )

      val fixture3 = multiFrom(allData3a).zip(multiFrom(allData3b))
      val expected3 = List(
        withHorizontal(interval(6, 9)) -> (Set("Hello"), Set("Cruel")),
        withHorizontal(interval(12, 14)) -> (Set("World"), Set("Cruel")),
        withHorizontal(interval(16, 20)) -> (Set("World"), Set("World"))
      )
      fixture3.getAll.toList shouldBe expected3

      val fixture4 = multiFrom(allData3a).zipAll(multiFrom(allData3b), Set("<"), Set(">"))
      val expected4 = List(
        withHorizontal(interval(-4, -2)) -> (Set("<"), Set("Goodbye")),
        withHorizontal(interval(0, 5)) -> (Set("Hello"), Set(">")),
        withHorizontal(interval(6, 9)) -> (Set("Hello"), Set("Cruel")),
        withHorizontal(interval(10, 11)) -> (Set("<"), Set("Cruel")),
        withHorizontal(interval(12, 14)) -> (Set("World"), Set("Cruel")),
        withHorizontal(intervalAt(15)) -> (Set("World"), Set(">")),
        withHorizontal(interval(16, 20)) -> (Set("World"), Set("World")),
        withHorizontal(interval(21, 24)) -> (Set("<"), Set("World"))
      )
      fixture4.getAll.toList shouldBe expected4

    test(s"$prefix: Getting diff actions"):

      import DiffAction.*

      val fixture5Data = List(
        withHorizontal(interval(0, 4)) -> "Hello",
        withHorizontal(interval(5, 15)) -> "to",
        withHorizontal(interval(16, 19)) -> "World",
        withHorizontal(interval(20, 25)) -> "!",
        withHorizontal(intervalFrom(26)) -> "World"
      )
      val fixture5 = multiFrom(fixture5Data)

      val expectedWorldIntervals = fixture5Data.collect:
        case ValidData("World", interval) => interval
      fixture5.intervalsOne("World") should contain theSameElementsAs expectedWorldIntervals

      val fixture6 = multiFrom(
        List(
          withHorizontal(intervalTo(4)) -> "Hey",
          withHorizontal(interval(5, 15)) -> "to",
          withHorizontal(intervalFrom(16)) -> "World"
        )
      )

      val fixture7 = multiFrom(List(withHorizontal(intervalTo(0)) -> "Hey"))

      val actionsFrom5To6 = fixture6.diffActionsFrom(fixture5)
      actionsFrom5To6 shouldBe Iterable(
        Create(withHorizontal(intervalTo(4)) -> Set("Hey")),
        Delete(withHorizontal(0)),
        Update(withHorizontal(intervalFrom(16)) -> Set("World")),
        Delete(withHorizontal(20)),
        Delete(withHorizontal(26))
      )

      val actionsFrom6To7 = fixture7.diffActionsFrom(fixture6)
      actionsFrom6To7 shouldBe Iterable(
        Update(withHorizontal(intervalTo(0)) -> Set("Hey")),
        Delete(withHorizontal(5)),
        Delete(withHorizontal(16))
      )

    test(s"$prefix: 4D-specific methods"):
      val allData = List(
        (intervalTo(14) x interval(0, 10) x intervalFrom(5) x unbounded[Int]) -> "Hello",
        (intervalFrom(1) x intervalFrom(11) x intervalFrom(21) x unbounded[Int]) -> "World"
      )
      val fixture1: S = multiFrom(allData)
      fixture1.getByHeadDimension(0).getByHeadDimension(5).getByHeadDimension(10).getAt(10) shouldBe Some(Set("Hello"))
      fixture1
        .getByDimension[Int, Domain.In3D[Int, Int, Int]](2, 10)
        .getByDimension[Int, Domain.In2D[Int, Int]](2, 10)
        .getByDimension[Int, Domain.In1D[Int]](1, 5)
        .getAt(0) shouldBe Some(Set("Hello"))
