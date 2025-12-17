package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.Domain.In1D as Dim
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn1DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn1DMultiBaseBehaviors
  with MutableMultiBaseBehaviors:

  import Interval1D.*

  def usingBuilder(data: Iterable[ValidData[String, Dim[Int]]]): DataMulti[String, Dim[Int]] =
    val builder = DataMulti.newBuilder[String, Dim[Int]]
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  testsFor(basicAndZipTests("Mutable", DataMulti.from(_), DataMulti.from(_), DataMulti.of(_), DataMulti(_)))
  testsFor(basicAndZipTests("Mutable (builder)", usingBuilder, DataMulti.from(_), DataMulti.of(_), DataMulti(_)))

  testsFor(
    addAndRemoveTests[
      Dim[Int],
      DataMulti[String, Dim[Int]]
    ](
      DataMulti.from(_),
      _.tupled
    )
  )

  testsFor(
    mapAndFlatmapTests[
      Dim[Int],
      DataMulti[String, Dim[Int]]
    ](
      DataMulti.from(_),
      _.tupled,
      d => d.interval.to(d.interval.end.rightAdjacent) -> d.value.map(_ + "!"),
      d => DataMulti.from[String, Dim[Int]](d.value.map(d.interval -> _))
    )
  )

  test("Mutable: Applying diff actions"):
    val fixture5 = DataMulti.from(
      List(
        interval(0, 4) -> "Hello",
        interval(5, 15) -> "to",
        interval(16, 19) -> "World",
        interval(20, 25) -> "!",
        intervalFrom(26) -> "World"
      )
    )

    val fixture6 = DataMulti.from(
      List(
        intervalTo(4) -> "Hey",
        interval(5, 15) -> "to",
        intervalFrom(16) -> "World"
      )
    )

    val fixture7 = DataMulti.of(intervalTo(0) -> "Hey")

    val f6sync = fixture5.copy
    f6sync.applyDiffActions(fixture6.diffActionsFrom(fixture5))
    f6sync shouldBe fixture6

    val f7sync = fixture5.copy
    f7sync.syncWith(fixture7)
    f7sync shouldBe fixture7
