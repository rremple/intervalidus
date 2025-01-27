package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn2DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn2DMultiBaseBehaviors
  with MutableMultiBaseBehaviors:

  import Interval1D.*

  testsFor(
    basicAndZipTests("Mutable", DataIn2DMulti.from(_), DataIn2DMulti.from(_), DataIn2DMulti.of(_), DataIn2DMulti(_))
  )
  testsFor(
    addAndRemoveTests[
      Domain2D[Int, Int],
      Interval2D[Int, Int],
      ValidData2D[String, Int, Int],
      ValidData2D[Set[String], Int, Int],
      DiffAction2D[Set[String], Int, Int],
      DataIn2DMulti[String, Int, Int]
    ](
      DataIn2DMulti.from(_),
      (interval, value) => ValidData2D(value, withHorizontal(interval)),
      (interval, valueSet) => ValidData2D(valueSet, withHorizontal(interval))
    )
  )

  testsFor(
    mapAndFlatmapTests[
      Domain2D[Int, Int],
      Interval2D[Int, Int],
      ValidData2D[String, Int, Int],
      ValidData2D[Set[String], Int, Int],
      DiffAction2D[Set[String], Int, Int],
      DataIn2DMulti[String, Int, Int]
    ](
      DataIn2DMulti.from(_),
      (interval, value) => ValidData2D(value, withHorizontal(interval)),
      (interval, valueSet) => ValidData2D(valueSet, withHorizontal(interval)),
      d => d.interval.toAfter(d.interval.end) -> d.value.map(_ + "!"),
      d => DataIn2DMulti.from(d.value.map(d.interval -> _))
    )
  )

  test("Mutable: Applying diff actions"):
    val fixture5 = DataIn2DMulti.from(
      List(
        withHorizontal(interval(0, 4)) -> "Hello",
        withHorizontal(interval(5, 15)) -> "to",
        withHorizontal(interval(16, 19)) -> "World",
        withHorizontal(interval(20, 25)) -> "!",
        withHorizontal(intervalFrom(26)) -> "World"
      )
    )

    val fixture6 = DataIn2DMulti.from(
      List(
        withHorizontal(intervalTo(4)) -> "Hey",
        withHorizontal(interval(5, 15)) -> "to",
        withHorizontal(intervalFrom(16)) -> "World"
      )
    )

    val fixture7 = DataIn2DMulti.of(withHorizontal(intervalTo(0)) -> "Hey")

    val f6sync = fixture5.copy
    f6sync.applyDiffActions(fixture6.diffActionsFrom(fixture5))
    f6sync.getAll.toList shouldBe fixture6.getAll.toList

    val f7sync = fixture5.copy
    f7sync.syncWith(fixture7)
    f7sync.getAll.toList shouldBe fixture7.getAll.toList
