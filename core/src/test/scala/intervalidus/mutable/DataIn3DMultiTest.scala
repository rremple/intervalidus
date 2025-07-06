package intervalidus.mutable

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.Domain.In3D as Dim
import intervalidus.DomainLike.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn3DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn3DMultiBaseBehaviors
  with MutableMultiBaseBehaviors:

  testsFor(
    basicAndZipTests("Mutable", DataMulti.from(_), DataMulti.from(_), DataMulti.of(_), DataMulti(_))
  )

  testsFor(
    addAndRemoveTests[Dim[Int, Int, Int], DataMulti[String, Dim[Int, Int, Int]]](
      DataMulti.from(_),
      withHorizontal
    )
  )

  testsFor(
    mapAndFlatmapTests[Dim[Int, Int, Int], DataMulti[String, Dim[Int, Int, Int]]](
      DataMulti.from(_),
      withHorizontal,
      d => d.interval.to(d.interval.end.rightAdjacent) -> d.value.map(_ + "!"),
      d => DataMulti.from(d.value.map(d.interval -> _))
    )
  )

  testsFor(
    applyingDiffActionTests[Dim[Int, Int, Int], DataMulti[String, Dim[Int, Int, Int]]](
      DataMulti.from(_),
      DataMulti.of(_),
      withHorizontal
    )
  )
