package intervalidus.immutable

import intervalidus.*
import intervalidus.Interval1D.*
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataIn2DMultiTest
  extends AnyFunSuite
  with Matchers
  with DataIn2DMultiBaseBehaviors
  with ImmutableMultiBaseBehaviors:

  def usingBuilder(data: Iterable[ValidData[String, IntDim]]): DataMulti[String, IntDim] =
    val builder = DataMulti.newBuilder[String, IntDim]
    builder.addOne(Interval.unbounded -> "Junk")
    builder.clear()
    data.foldLeft(builder)(_.addOne(_)).result()

  testsFor(basicAndZipTests("Immutable", DataMulti.from(_), DataMulti.of(_), DataMulti(_)))
  testsFor(basicAndZipTests("Immutable (builder)", usingBuilder, DataMulti.of(_), DataMulti(_)))

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

  test(s"Immutable: Int DataMulti 2D collection operations"):
    // assert equivalence/non-equivalence
    extension [V, D <: NonEmptyTuple: DomainLike](lhs: DataMulti[V, D])
      infix def ≡≡(rhs: DataMulti[V, D]): Assertion = assert(lhs ≡ rhs, s"\nExpected: $lhs\nActual: $rhs\n")
      infix def !≡(rhs: DataMulti[V, D]): Assertion = assert(!(lhs ≡ rhs))

    type Dim = Domain.In2D[Int, Int]

    /* When discrete, the donut (intervals a, b, c, and d) and, its complement, the hole (interval e) look like:

       +∞    b  b  b  c  c  c  c  c  c
       ..    b  b  b  c  c  c  c  c  c
        2    b  b  b  c  c  c  c  c  c
        1    b  b  b  e  e  e  d  d  d
        0    b  b  b  e  e  e  d  d  d
       -1    b  b  b  e  e  e  d  d  d
       -2    a  a  a  a  a  a  d  d  d
       ..    a  a  a  a  a  a  d  d  d
       -∞    a  a  a  a  a  a  d  d  d

            -∞ .. -2 -1  0  1  2 .. +∞

     */

    val a = intervalTo(1) x intervalToBefore(-1)
    val b = intervalToBefore(-1) x intervalFrom(-1)
    val c = intervalFrom(-1) x intervalFromAfter(1)
    val d = intervalFromAfter(1) x intervalTo(1)
    val e = interval(-1, 1) x interval(-1, 1)

    val donutFilling = Set(3.0, 5.0, 7.0)
    val holeFilling = Set(7.0, 9.0)
    val fillingUnion = donutFilling ++ holeFilling
    val fillingIntersection = donutFilling intersect holeFilling

    val donut = DataMulti(Seq(a, b, c, d).map(_ -> donutFilling)) // no e
    val hole = DataMulti(Seq(e -> holeFilling))

    extension [T](data: DataMulti[T, Dim])
      private def filledWith(v: Set[T]): DataMulti[T, Dim] = DataMulti(data.getAll.map(_.copy(value = v)))

    extension (intervals: Seq[Interval[Dim]])
      private def valueFilled[T](v: Set[T]) = DataMulti(intervals.map(_ -> v))
      private def donutFilled = intervals.valueFilled(donutFilling)
      private def holeFilled = intervals.valueFilled(holeFilling)

    donut.isEmpty shouldBe false
    hole.isEmpty shouldBe false

    val donutFromData: DataMulti[Double, Dim] = Data(Seq(a, b, c, d).map(_ -> donutFilling)) // implicitly converted
    donutFromData ≡≡ donut

    (donut ∩ hole).isEmpty shouldBe true
    (donut ∪ hole).domain.toList shouldBe List(Interval.unbounded[Dim])
    (donut ∪ hole) shouldBe DataMulti(Seq(a, b, c, d).map(_ -> donutFilling) ++ Seq(e -> holeFilling))

    (Seq(a, b).donutFilled ∪ Seq(c, d).donutFilled) ≡≡ donut
    (Seq(a, b).donutFilled ∪ Seq(b, c).holeFilled) ≡≡
      DataMulti(Seq(a -> donutFilling, b -> fillingUnion, c -> holeFilling))
    (Seq(a, e).donutFilled ∩ Seq(e, d).holeFilled) ≡≡ Seq(e).valueFilled(fillingIntersection)

    val clipInterval = interval(-10, 10) x interval(-10, 10)
    val clippedDonutWithHole = (donut ∪ hole) ∩ clipInterval

    val donutIn3D: DataMulti[Double, Domain.In3D[Int, Int, Int]] =
      clippedDonutWithHole.extrudeDimension(2, interval(-1, 1))

    val flattenedDonut: DataMulti[Double, Dim] = donutIn3D.flattenDimension(2)
    flattenedDonut shouldBe clippedDonutWithHole

    val flattenedEdgeShadow: DataMulti[Double, Dim] = donutIn3D.flattenDimension(0)
    flattenedEdgeShadow shouldBe DataMulti(
      Seq(
        (intervalFrom(-10).toBefore(-1) x interval(-1, 1)) -> donutFilling,
        (interval(-1, 1) x interval(-1, 1)) -> fillingUnion,
        (intervalFromAfter(1).to(10) x interval(-1, 1)) -> donutFilling
      )
    )

    def donutFirst(a: Set[Double], b: Set[Double]): Set[Double] = if a.min < b.min then a else b
    def holeFirst(a: Set[Double], b: Set[Double]): Set[Double] = if a.min < b.min then b else a
    val collapsedDonutFirst: DataMulti[Double, Dim] = donutIn3D.collapseDimension(2, donutFirst)
    val collapsedHoleFirst: DataMulti[Double, Dim] = donutIn3D.collapseDimension(2, donutFirst)
    collapsedDonutFirst shouldBe clippedDonutWithHole
    collapsedHoleFirst shouldBe clippedDonutWithHole

    val collapsedEdgeShadowDonutFirst: DataMulti[Double, Dim] = donutIn3D.collapseDimension(0, donutFirst)
    val collapsedEdgeShadowHoleFirst: DataMulti[Double, Dim] = donutIn3D.collapseDimension(0, holeFirst)
    collapsedEdgeShadowDonutFirst shouldBe DataMulti(
      Seq((interval(-10, 10) x interval(-1, 1)) -> donutFilling)
    )
    collapsedEdgeShadowHoleFirst shouldBe DataMulti(
      Seq(
        (intervalFrom(-10).toBefore(-1) x interval(-1, 1)) -> donutFilling,
        (interval(-1, 1) x interval(-1, 1)) -> holeFilling,
        (intervalFromAfter(1).to(10) x interval(-1, 1)) -> donutFilling
      )
    )
