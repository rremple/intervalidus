package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.Domain.In4D as Dim
import intervalidus.DomainLike.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn4DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn4DMultiBaseBehaviors
  with ImmutableMultiBaseBehaviors:

  testsFor(
    basicAndZipTests("Immutable", DataMulti.from(_), DataMulti.from(_), DataMulti.of(_), DataMulti(_))
  )

  testsFor(
    addAndRemoveTests[Dim[Int, Int, Int, Int], DataMulti[String, Dim[Int, Int, Int, Int]]](
      DataMulti.from(_),
      withHorizontal
    )
  )

  testsFor(
    mapAndFlatmapTests[Dim[Int, Int, Int, Int], DataMulti[String, Dim[Int, Int, Int, Int]]](
      DataMulti.from(_),
      withHorizontal,
      d => d.interval.to(d.interval.end.rightAdjacent) -> d.value.map(_ + "!"),
      d => DataMulti.from(d.value.map(d.interval -> _))
    )
  )

  testsFor(
    applyingDiffActionTests[Dim[Int, Int, Int, Int], DataMulti[String, Dim[Int, Int, Int, Int]]](
      DataMulti.from(_),
      DataMulti.of(_),
      withHorizontal
    )
  )
