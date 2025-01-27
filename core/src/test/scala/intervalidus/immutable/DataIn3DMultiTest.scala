package intervalidus.immutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn3DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn3DMultiBaseBehaviors
  with ImmutableMultiBaseBehaviors:

  import Interval1D.*

  testsFor(
    basicAndZipTests("Immutable", DataIn3DMulti.from(_), DataIn3DMulti.from(_), DataIn3DMulti.of(_), DataIn3DMulti(_))
  )

  testsFor(
    addAndRemoveTests[
      Domain3D[Int, Int, Int],
      Interval3D[Int, Int, Int],
      ValidData3D[String, Int, Int, Int],
      ValidData3D[Set[String], Int, Int, Int],
      DiffAction3D[Set[String], Int, Int, Int],
      DataIn3DMulti[String, Int, Int, Int]
    ](
      DataIn3DMulti.from(_),
      (interval, value) => ValidData3D(value, withHorizontal(interval)),
      (interval, valueSet) => ValidData3D(valueSet, withHorizontal(interval))
    )
  )

  test("Immutable: Mapping, flatmapping, etc."):
    val allData = List(withHorizontal(intervalTo(4)) -> "Hey", withHorizontal(intervalFrom(16)) -> "World")

    val fixture1 = DataIn3DMulti.from(allData)
    val fixture2 = fixture1.map(d =>
      d.interval.to(
        d.interval.end.horizontalIndex.successor x
          d.interval.end.verticalIndex.successor x
          d.interval.end.depthIndex.successor
      ) -> d.value.map(_ + "!")
    )
    val expectedData2 = List(
      withHorizontal(intervalTo(5)) -> Set("Hey!"),
      withHorizontal(intervalFrom(16)) -> Set("World!")
    )
    fixture2.getAll.toList shouldBe expectedData2

    val fixture3 = fixture2.mapValues(_.map(_ + "!!"))
    val expectedData3 = List(
      withHorizontal(intervalTo(5)) -> Set("Hey!!!"),
      withHorizontal(intervalFrom(16)) -> Set("World!!!")
    )
    fixture3.getAll.toList shouldBe expectedData3

    val fixture4 = fixture3.flatMap(d => DataIn3DMulti.from(d.value.map(d.interval -> _)))
    val expectedData4 = List(
      withHorizontal(intervalTo(5)) -> Set("Hey!!!"),
      withHorizontal(intervalFrom(16)) -> Set("World!!!")
    )
    fixture4.getAll.toList shouldBe expectedData4

  test("Immutable: Applying diff actions"):
    val fixture5 = DataIn3DMulti.from(
      List(
        withHorizontal(interval(0, 4)) -> "Hello",
        withHorizontal(interval(5, 15)) -> "to",
        withHorizontal(interval(16, 19)) -> "World",
        withHorizontal(interval(20, 25)) -> "!",
        withHorizontal(intervalFrom(26)) -> "World"
      )
    )

    val fixture6 = DataIn3DMulti.from(
      List(
        withHorizontal(intervalTo(4)) -> "Hey",
        withHorizontal(interval(5, 15)) -> "to",
        withHorizontal(intervalFrom(16)) -> "World"
      )
    )

    val fixture7 = DataIn3DMulti.of(withHorizontal(intervalTo(0)) -> "Hey")

    val f6sync = fixture5.applyDiffActions(fixture6.diffActionsFrom(fixture5))
    f6sync.getAll.toList shouldBe fixture6.getAll.toList

    val f7sync = fixture5.syncWith(fixture7)
    f7sync.getAll.toList shouldBe fixture7.getAll.toList
