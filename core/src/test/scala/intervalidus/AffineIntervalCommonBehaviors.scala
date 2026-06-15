package intervalidus

import intervalidus.*
import intervalidus.Domain1D.{Bottom, Top, domain}
import intervalidus.Interval1D.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.annotation.{nowarn, tailrec}
import scala.language.implicitConversions

/**
  * Test behaviors that do not differ between discrete or continuous affine intervals.
  */
trait AffineIntervalCommonBehaviors(using op: DomainAffineValueLike[Int]):
  this: AnyFunSuite & Matchers =>

  /*
   * Define proxy test methods to allow client to make concrete reference to the affine domain type (i.e., for path
   * dependent types like Displacement and Scalar.) The *Int methods are specific to structures with 1d or 2d Int
   * dimensions.
   */

  type Int2d = Domain.In2D[Int, Int]

  extension (lhs: Domain1D[Int])
    infix def displacementToInt(rhs: Domain1D[Int]): Option[Int]
    infix def displacedByInt(offset: Int): Domain1D[Int]
    infix def reflectedAboutInt(pivot: Domain1D[Int]): Domain1D[Int]
    infix def scaledAboutInt(center: Domain1D[Int], scaledBy: Double): Domain1D[Int]

  extension (lhs: Interval1D[Int])
    infix def measureInt: Option[Int]
    infix def displacedByInt(offset: Int): Option[Interval1D[Int]]
    infix def reflectedAboutInt(pivot: Domain1D[Int]): Option[Interval1D[Int]]
    infix def scaledAboutInt(center: Domain1D[Int], scaledBy: Double): Option[Interval1D[Int]]

  extension (lhs: Interval[Int2d])
    infix def measureInt: Option[(Int, Int)]
    infix def displacedByInt(offset: (Int, Int)): Option[Interval[Int2d]]
    infix def reflectedAboutInt(pivot: Int2d): Option[Interval[Int2d]]
    infix def scaledAboutInt(center: Int2d, scaledBy: (Double, Double)): Option[Interval[Int2d]]

  extension (lhs: IntervalShape[Int2d]) def boundingShapeInt(thickness: (Int, Int)): IntervalShape[Int2d]

  val top: Domain1D[Int] = Top
  val bottom: Domain1D[Int] = Bottom

  // for testing Interval.mapMeasure and IntervalShape.mapReduceMeasure (continuous only for now)

  extension (lhs: Double) infix def **(power: Double): Double = math.pow(lhs, power)

  // A tuple type with the same number of elements as T, but every element is Double
  type TupleOfDoubles[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case _ *: tail  => Double *: TupleOfDoubles[tail]

  // Folds over a tuple of Doubles to arrive at some non-tuple result
  def foldTuple[T <: Tuple](t: T, z: Double)(op: (Double, Double) => Double)(using T =:= TupleOfDoubles[T]): Double =
    @tailrec
    @nowarn("msg=match may not be exhaustive") // it's a tuple of Doubles (proven), but the compiler doesn't know that
    def loop[TT <: Tuple](t: TT, acc: Double): Double = t match
      case EmptyTuple             => acc
      case (head: Double) *: tail => loop(tail, op(acc, head))

    loop(t, z)

  // Folds over a tuple of Doubles representing measurements to find the volume (all values multiplied)
  def volume[T <: Tuple](t: T)(using T =:= TupleOfDoubles[T]): Double = foldTuple(t, 1.0)(_ * _)

  // Folds over a tuple of Doubles representing measurements to find the diagonal Euclidian distance
  def diagonal[T <: Tuple](t: T)(using T =:= TupleOfDoubles[T]): Double = math.sqrt(foldTuple(t, 0.0)(_ + _ ** 2))

  def commonAffineBehaviors(prefix: String): Unit =

    test(s"$prefix: Int affine 1d domain behaviors"):
      domain(2) displacementToInt domain(2) shouldBe Some(0)
      domain(2) displacementToInt bottom shouldBe None
      domain(2) displacementToInt top shouldBe None

      domain(2) reflectedAboutInt 3 shouldBe domain(4)

      domain(1).scaledAboutInt(0, scaledBy = 3.0) shouldBe domain(3)
      assertThrows[Exception]: // can't scale using an unbounded center
        domain(1).scaledAboutInt(top, scaledBy = 3.0) shouldBe domain(3)
      top.scaledAboutInt(0, scaledBy = 3.0) shouldBe top
      bottom.scaledAboutInt(0, scaledBy = 3.0) shouldBe bottom
      top.scaledAboutInt(0, scaledBy = -3.0) shouldBe bottom // reflected
      bottom.scaledAboutInt(0, scaledBy = -3.0) shouldBe top // reflected

      // displacement from center overflows
      domain(op.minValue).scaledAboutInt(op.maxValue, scaledBy = 3.0) shouldBe bottom
      domain(op.maxValue).scaledAboutInt(op.minValue, scaledBy = 3.0) shouldBe top
      domain(op.minValue).scaledAboutInt(op.maxValue, scaledBy = -3.0) shouldBe top
      domain(op.maxValue).scaledAboutInt(op.minValue, scaledBy = -3.0) shouldBe bottom

      // scaling displacement overflows
      domain(op.minValue).scaledAboutInt(0, scaledBy = 3.0) shouldBe bottom
      domain(op.maxValue).scaledAboutInt(0, scaledBy = 3.0) shouldBe top
      domain(op.minValue).scaledAboutInt(0, scaledBy = -3.0) shouldBe top
      domain(op.maxValue).scaledAboutInt(0, scaledBy = -3.0) shouldBe bottom

      // displacing by scaled displacement overflows
      domain(op.minValue / 3 * 2).scaledAboutInt(op.minValue / 3, scaledBy = 3.0) shouldBe bottom
      domain(op.maxValue / 3 * 2).scaledAboutInt(op.maxValue / 3, scaledBy = 3.0) shouldBe top
      domain(op.minValue / 3).scaledAboutInt(op.minValue / 3 * 2, scaledBy = -3.0) shouldBe bottom
      domain(op.maxValue / 3).scaledAboutInt(op.maxValue / 3 * 2, scaledBy = -3.0) shouldBe top

      domain(2) displacedByInt 3 shouldBe domain(5)
      bottom displacedByInt 3 shouldBe bottom
      top displacedByInt 3 shouldBe top
      domain(op.minValue).displacedByInt(op.minValue) shouldBe bottom
      domain(op.maxValue).displacedByInt(op.maxValue) shouldBe top

    test(s"$prefix: Int affine 1d interval behaviors"):
      intervalFrom(1).toBefore(2).measureInt shouldBe Some(1) // measure = 2-1
      intervalToBefore(2).reflectedAboutInt(3) shouldBe Some(intervalFromAfter(4))
      intervalFrom(1).toBefore(2).scaledAboutInt(0, scaledBy = 3.0) shouldBe Some(
        intervalFrom(3).toBefore(6)
      ) // 2 * 3 = 6
      intervalAt(op.maxValue).scaledAboutInt(0, scaledBy = 3.0) shouldBe None
      intervalFromAfter(1).to(2).displacedByInt(3) shouldBe Some(intervalFromAfter(4).to(5))
      intervalAt(op.maxValue).displacedByInt(3) shouldBe None

    test(s"$prefix: Int affine 2d interval behaviors"):
      (intervalToBefore(2) x intervalFrom(1).toBefore(2)).reflectedAboutInt(Domain.in2D(3, 3)) shouldBe
        Some(intervalFromAfter(4) x intervalFromAfter(4).to(5))

      (intervalFrom(1).toBefore(2) x intervalFrom(3).toBefore(4))
        .scaledAboutInt(Domain.in2D(0, 0), scaledBy = (3.0, 2.0)) shouldBe
        Some(intervalFrom(3).toBefore(6) x intervalFrom(6).toBefore(8)) // measure 1x1 => measure 3x2

      (intervalAt(op.maxValue) x intervalFrom(1).toBefore(2))
        .scaledAboutInt(Domain.in2D(0, 0), scaledBy = (3.0, 2.0)) shouldBe None

      (intervalFrom(1).toBefore(2) x intervalFrom(3).toBefore(4)).displacedByInt((3, 2)) shouldBe
        Some(intervalFrom(4).toBefore(5) x intervalFrom(5).toBefore(6))

      (intervalAt(op.maxValue) x intervalFrom(1).toBefore(2)).displacedByInt((3, 2)) shouldBe None

      (intervalFrom(1).toBefore(3) x intervalFromAfter(3).to(5)).measureInt shouldBe Some((2, 2))
      (intervalFromAfter(2).to(5) x intervalFrom(6).toBefore(10)).measureInt shouldBe Some((3, 4))
      (intervalFrom(4) x interval(4, 5)).measureInt shouldBe None

    test(s"$prefix: Int affine interval shape bounding shapes"):
      extension [D <: NonEmptyTuple: DomainAffineLike](lhs: IntervalShape[D])
        infix def ≡≡(rhs: IntervalShape[D]) = assert(lhs ≡ rhs, s"\nExpected: $lhs\nActual: $rhs\n")

      val i1 = intervalFrom(1).toBefore(3) x intervalFromAfter(3).to(5)
      val i2 = intervalFromAfter(2).to(5) x intervalFromAfter(5).toBefore(10)

      // pad one unit externally, one interval
      IntervalShape(Seq(i1)).boundingShapeInt((1, 1)) ≡≡ IntervalShape(
        Seq(
          intervalFrom(0).toBefore(4) x intervalFromAfter(2).to(3), // under
          intervalFrom(0).toBefore(1) x intervalFromAfter(3).to(6), // left
          intervalFrom(1).toBefore(4) x intervalFromAfter(5).to(6), // above
          intervalFrom(3).toBefore(4) x intervalFromAfter(3).to(5) // right
        )
      )

      // pad two units internally, one interval with a smaller width than the pad amount (shrinks to zero)
      IntervalShape(Seq(i1)).boundingShapeInt((-2, -2)) ≡≡ IntervalShape(Seq(i1))

      // pad one unit internally, one interval
      IntervalShape(Seq(i2)).boundingShapeInt((-1, -1)) ≡≡ IntervalShape(
        Seq(
          intervalFromAfter(2).to(5) x intervalFromAfter(5).to(6), // bottom
          intervalFromAfter(2).to(3) x intervalFromAfter(6).toBefore(10), // left side
          intervalFromAfter(3).to(5) x intervalFrom(9).toBefore(10), // top
          intervalFromAfter(4).to(5) x intervalFromAfter(6).toBefore(9) // right side
        )
      )

      // pad two units externally, two intervals with a shared boundary
      IntervalShape(Seq(i1, i2)).boundingShapeInt((2, 2)) ≡≡ IntervalShape(
        Seq(
          intervalFrom(-1).toBefore(5) x intervalFromAfter(1).to(3), // below i1
          intervalFrom(-1).toBefore(1) x intervalFromAfter(3).to(5), // left of i1
          interval(-1, 2) x intervalFromAfter(5).to(7), // above i1 and left of i2
          intervalFromAfter(0).to(2) x intervalFromAfter(7).toBefore(10), // left of i2
          intervalFromAfter(0).to(7) x intervalFrom(10).toBefore(12), // above i2
          interval(3, 7) x intervalFromAfter(3).to(5), // right of i1 and below i2
          intervalFromAfter(5).to(7) x intervalFromAfter(5).toBefore(10) // right of i2
        )
      )
