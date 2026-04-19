package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn4DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn4DMultiBaseBehaviors
  with MutableMultiBaseBehaviors:

  def usingBuilder(
    data: Iterable[ValidData[String, IntDim]]
  ): DataMulti[String, IntDim] =
    val builder = DataMulti.newBuilder[String, IntDim]
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  testsFor(basicAndZipTests("Mutable", DataMulti.from(_), DataMulti.from(_), DataMulti.of(_), DataMulti(_)))
  testsFor(basicAndZipTests("Mutable (builder)", usingBuilder, DataMulti.from(_), DataMulti.of(_), DataMulti(_)))

  testsFor(
    addAndRemoveTests[IntDim, DataMulti[String, IntDim]](
      DataMulti.from(_),
      withHorizontal
    )
  )

  testsFor(
    mapAndFlatmapTests[IntDim, DataMulti[String, IntDim]](
      DataMulti.from(_),
      withHorizontal,
      d => d.interval.to(d.interval.end.rightAdjacent) -> d.value.map(_ + "!"),
      d => DataMulti.from(d.value.map(d.interval -> _))
    )
  )

  testsFor(
    applyingDiffActionTests[IntDim, DataMulti[String, IntDim]](
      DataMulti.from(_),
      DataMulti.of(_),
      withHorizontal
    )
  )
