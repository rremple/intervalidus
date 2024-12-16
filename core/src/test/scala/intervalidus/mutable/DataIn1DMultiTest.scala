package intervalidus.mutable

import intervalidus.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn1DMultiBaseBehaviors
  with MutableMultiBaseBehaviors:

  import DiscreteInterval1D.*

  testsFor(
    basicAndZipTests("Mutable", DataIn1DMulti.from(_), DataIn1DMulti.from(_), DataIn1DMulti.of(_), DataIn1DMulti(_))
  )
  testsFor(
    addAndRemoveTests[
      DiscreteDomain1D[Int],
      DiscreteInterval1D[Int],
      ValidData1D[String, Int],
      ValidData1D[Set[String], Int],
      DiffAction1D[Set[String], Int],
      DataIn1DMulti[String, Int]
    ](
      DataIn1DMulti.from(_),
      (interval, value) => ValidData1D(value, interval),
      (interval, valueSet) => ValidData1D(valueSet, interval)
    )
  )

  testsFor(
    mapAndFlatmapTests[
      DiscreteDomain1D[Int],
      DiscreteInterval1D[Int],
      ValidData1D[String, Int],
      ValidData1D[Set[String], Int],
      DiffAction1D[Set[String], Int],
      DataIn1DMulti[String, Int]
    ](
      DataIn1DMulti.from(_),
      (interval, value) => ValidData1D(value, interval),
      (interval, valueSet) => ValidData1D(valueSet, interval),
      d => d.interval.toAfter(d.interval.end) -> d.value.map(_ + "!"),
      d => DataIn1DMulti.from[String, Int](d.value.map(d.interval -> _))
    )
  )

  test("Mutable: Applying diff actions"):
    val fixture5 = DataIn1DMulti.from(
      List(
        interval(0, 4) -> "Hello",
        interval(5, 15) -> "to",
        interval(16, 19) -> "World",
        interval(20, 25) -> "!",
        intervalFrom(26) -> "World"
      )
    )

    val fixture6 = DataIn1DMulti.from(
      List(
        intervalTo(4) -> "Hey",
        interval(5, 15) -> "to",
        intervalFrom(16) -> "World"
      )
    )

    val fixture7 = DataIn1DMulti.of(intervalTo(0) -> "Hey")

    val f6sync = fixture5.copy
    f6sync.applyDiffActions(fixture6.diffActionsFrom(fixture5))
    f6sync.getAll.toList shouldBe fixture6.getAll.toList

    val f7sync = fixture5.copy
    f7sync.syncWith(fixture7)
    f7sync.getAll.toList shouldBe fixture7.getAll.toList
