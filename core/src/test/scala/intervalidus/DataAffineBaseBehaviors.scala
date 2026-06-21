package intervalidus

import intervalidus.*
import intervalidus.Interval1D.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

/**
  * Test behaviors that do not differ between discrete or continuous interval-based shapes.
  */
trait DataAffineBaseBehaviors(using
  DomainAffineValueLike[Int]
):
  this: AnyFunSuite & Matchers =>

  type Dim = Domain.In2D[Int, Int]

  val origin: Dim = Domain.in2D(0, 0)
  val quadrantOneSample: Dim = Domain.in2D(5, 5)
  val quadrantTwoSample: Dim = Domain.in2D(-5, 5)
  val quadrantThreeSample: Dim = Domain.in2D(-5, -5)
  val quadrantFourSample: Dim = Domain.in2D(5, -5)

  val quadrantSamples: List[(Dim, Int)] = List(
    quadrantOneSample -> 1,
    quadrantTwoSample -> 2,
    quadrantThreeSample -> 3,
    quadrantFourSample -> 4
  )

  val fromOrigin: Interval1D[Int] = intervalFrom(0)
  val toBeforeOrigin: Interval1D[Int] = intervalToBefore(0)
  val toOrigin: Interval1D[Int] = intervalTo(0)
  val fromAfterOrigin: Interval1D[Int] = intervalFromAfter(0)

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
  val aa: Interval[Dim] = intervalTo(1) x intervalToBefore(-1)
  val bb: Interval[Dim] = intervalToBefore(-1) x intervalFrom(-1)
  val cc: Interval[Dim] = intervalFrom(-1) x intervalFromAfter(1)
  val dd: Interval[Dim] = intervalFromAfter(1) x intervalTo(1)
  val ee: Interval[Dim] = interval(-1, 1) x interval(-1, 1)

  val donutFilling: Double = 3.0
  val holeFilling: Double = 7.0

  val clipInterval: Interval[Dim] = interval(-10, 10) x interval(-10, 10)

  /*
   * Define proxy test methods to allow client to make concrete reference to the affine domain type (i.e., for path
   * dependent types like Displacement and Scalar.) The donut* methods are specific to the test clipped 2d donut
   * structure with Int dimensions with Double values.
   */

  def clippedDonut: DimensionalAffineBase[Double, Dim]

  def donutDisplacedByIn(dimensionIndex: Domain.DimensionIndex, offset: Int)(using
    Domain.HasIndex[Dim, dimensionIndex.type],
    Domain.IsAtIndex[Dim, dimensionIndex.type, Int],
    Domain.IsUpdatableAtIndex[Dim, dimensionIndex.type, Int]
  ): DimensionalAffineBase[Double, Dim]

  def donutDisplacedBy(offset: (Int, Int)): DimensionalAffineBase[Double, Dim]

  def donutBoundingShape(thickness: (Int, Int)): IntervalShape[Dim]

  // accomodates any multidimensional signal with Int dimensions
  def convolvedByIn[V, D <: NonEmptyTuple: DomainAffineLike, K](
    dimensionIndex: Domain.DimensionIndex,
    signalData: Seq[ValidData[V, D]],
    kernelData: Seq[ValidData[K, Domain.In1D[Int]]],
    kernelOrigin: Domain1D[Int],
    epsilon: Int,
    combine: (V, K) => V,
    scaledByEpsilon: (V, Int) => V,
    accumulate: (V, V) => V
  )(using
    signalConfig: CoreConfig[D],
    kernelConfig: CoreConfig[Domain.In1D[Int]]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, Int],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, Int]
  ): DimensionalAffineBase[V, D]

  // assert equivalence/non-equivalence
  extension [V, D <: NonEmptyTuple: DomainAffineLike](lhs: DimensionalAffineBase[V, D])
    infix def ≡≡(rhs: DimensionalAffineBase[V, D]): Assertion =
      assert(lhs ≡ rhs, s"\nExpected (rhs): \n$rhs\nActual (lhs): \n$lhs\n")
    infix def !≡(rhs: DimensionalAffineBase[V, D]): Assertion = assert(!(lhs ≡ rhs))

  def affineBehaviors(
    prefix: String
  ): Unit =

    /**
      * Continuous/discrete results are the same, but the Displacement type must resolve where there is a concrete
      * reference to the affine domain type.
      */
    test(s"$prefix: Int DataAffine displacing"):

      // - dimension 0 (horizontal)

      donutDisplacedByIn(0, 0) ≡≡ clippedDonut // no-op displacement

      donutDisplacedByIn(0, 1).getAll.toList shouldBe List(
        (interval(-9, 2) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa displaced horizontally
        (intervalFrom(-9).toBefore(0) x interval(-1, 10)) -> donutFilling, // clipped bb displaced horizontally
        (interval(0, 11) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc displaced horizontally
        (intervalFromAfter(2).to(11) x interval(-10, 1)) -> donutFilling // clipped dd displaced horizontally
      )

      // - dimension 1 (vertical)

      donutDisplacedByIn(1, 0) ≡≡ clippedDonut // no-op displacement

      donutDisplacedByIn(1, 1).getAll.toList shouldBe List(
        (interval(-10, 1) x intervalFrom(-9).toBefore(0)) -> donutFilling, // clipped aa displaced vertically
        (intervalFrom(-10).toBefore(-1) x interval(0, 11)) -> donutFilling, // clipped bb displaced vertically
        (interval(-1, 10) x intervalFromAfter(2).to(11)) -> donutFilling, // clipped cc displaced vertically
        (intervalFromAfter(1).to(10) x interval(-9, 2)) -> donutFilling // clipped dd displaced vertically
      )

      // multi-dimensional

      donutDisplacedBy((0, 0)) ≡≡ clippedDonut // no-op displacement

      donutDisplacedBy((1, 1)).getAll.toList shouldBe List(
        (interval(-9, 2) x intervalFrom(-9).toBefore(0)) -> donutFilling,
        (intervalFrom(-9).toBefore(0) x interval(0, 11)) -> donutFilling,
        (interval(0, 11) x intervalFromAfter(2).to(11)) -> donutFilling,
        (intervalFromAfter(2).to(11) x interval(-9, 2)) -> donutFilling
      )

      assert(
        donutBoundingShape((2, 2)) ≡ IntervalShape(
          Seq(
            ee, // the entire hole, because the (2, 2) padding captures the "whole" thing (pun intended)
            interval(-12, 12) x intervalFrom(-12).toBefore(-10), // under
            interval(-10, 12) x intervalFromAfter(10).to(12), // above
            intervalFrom(-12).toBefore(-10) x interval(-10, 12), // left
            intervalFromAfter(10).to(12) x interval(-10, 10) // right
          )
        )
      )

    /**
      * Continuous/discrete results are the same, but the Displacement type must resolve where there is a concrete
      * reference to the affine domain value type (IntContinuousAffineValue in the continuous case).
      */
    test(s"$prefix: Int DataAffine convolving"):
      /**
        * Convolution kernels must be finitely bound
        */
      assertThrows[IllegalArgumentException]:
        val _ = convolvedByIn(
          dimensionIndex = 0,
          signalData = Seq(intervalFrom(0) -> 1.0), // Unbounded signal is fine, but an
          kernelData = Seq(intervalFrom(0) -> 0.5), // unbounded kernel is not allowed.
          kernelOrigin = 1,
          epsilon = 1,
          combine = _ * _,
          scaledByEpsilon = _ * _,
          accumulate = _ + _
        )

      /**
        * Convolving an impulse spread over a block should result in a smooth, ramped trapezoid spanning a wider region.
        * If we think of the valid value as the energy, then the convolved result should conserve the same total energy
        * as the signal. I.e., the area under the resulting trapezoid should match the area under the signal block.
        */
      // A single data value valid in a specific range representing a "impulse" spread over a clean block.
      val signal1 = Seq(intervalFrom(0).toBefore(4) -> 1.0) // clean block measure 4, total energy = 4 * 1.0 = 4.0
      // A uniform box kernel, measure 2
      val kernel1 = Seq(intervalFrom(0).toBefore(2) -> 0.5) // 2 * 0.5 = 1.0 - energy will be conserved
      val convolved1 = convolvedByIn(
        dimensionIndex = 0,
        signalData = signal1,
        kernelData = kernel1,
        kernelOrigin = 1,
        epsilon = 1,
        combine = _ * _,
        scaledByEpsilon = _ * _,
        accumulate = _ + _
      )
      convolved1.getAll.toList shouldBe List(
        intervalFrom(0).toBefore(1) -> 0.5, // energy = measure 1 * 0.5 = 0.5
        intervalFrom(1).toBefore(4) -> 1.0, // energy = measure 3 * 1.0 = 3.0
        intervalFrom(4).toBefore(5) -> 0.5 // energy = measure 1 * 0.5 = 0.5
      ) // total energy = 0.5 + 3.0 + 0.5 = 4.0

      /**
        * Convolves data sampled at step sizes that don't neatly divide into the kernel measure. When the kernel slides
        * across, it will temporarily cover part of the `(0, 3]` block and part of the `(3, 6]` block. Tests that the
        * library properly computes the weight, fractionalizing the interval value based on the overlap measure.
        *
        * If the kernel slides by a step size of 1, but the data changes every 3, this test shows we properly create
        * "synthetic" sub-intervals in the output data. The test asserts that a few wide input intervals gracefully
        * shatter into many smaller, precisely weighted output intervals.
        */
      // A repeating step function with a stride measure of 3 (adjacent blocks).
      val signal2 =
        Seq(intervalFromAfter(0).to(3) -> 10.0, intervalFromAfter(3).to(6) -> 20.0, intervalFromAfter(6).to(9) -> 30.0)
        // Two regions with unaligned measures of 2.
      val kernel2 =
        Seq(intervalFrom(0).toBefore(2) -> 1.0, intervalFrom(4).toBefore(6) -> -1.0)

      val convolved2 = convolvedByIn(
        dimensionIndex = 0,
        signalData = signal2,
        kernelData = kernel2,
        kernelOrigin = 3,
        epsilon = 1,
        combine = _ * _,
        scaledByEpsilon = _ * _,
        accumulate = _ + _
      )

      convolved2.getAll.toList shouldBe List(
        intervalFromAfter(-2).to(-1) -> -10.0, // kernel block 2
        intervalFromAfter(-1).to(1) -> -20.0, // kernel block 2
        intervalFromAfter(1).to(3) -> -30.0, // kernel block 1 & 2
        intervalFromAfter(3).to(4) -> -20.0, // kernel block 1 & 2
        intervalFromAfter(4).to(6) -> -30.0, // kernel block 1 & 2
        intervalFromAfter(6).to(7) -> -20.0, // kernel block 1 & 2
        intervalFromAfter(7).to(8) -> 10.0, // kernel block 1 & 2
        intervalFromAfter(8).to(9) -> 50.0, // kernel block 1
        intervalFromAfter(9).to(11) -> 60.0, // kernel block 1
        intervalFromAfter(11).to(12) -> 30.0 // kernel block 1
      )

      /**
        * Doing the same, but with config.withCompressOnUpdate(false) yields different but equivilant results
        */
      val convolvedNotCompressed = convolvedByIn(
        dimensionIndex = 0,
        signalData = signal2,
        kernelData = kernel2,
        kernelOrigin = 3,
        epsilon = 1,
        combine = _ * _,
        scaledByEpsilon = _ * _,
        accumulate = _ + _
      )(using signalConfig = CoreConfig.default[Domain.In1D[Int]].withCompressOnUpdate(false))
      convolvedNotCompressed.getAll.toList shouldBe List(
        intervalFromAfter(-2).to(-1) -> -10.0, // kernel block 2
        intervalFromAfter(-1).to(1) -> -20.0, // kernel block 2
        intervalFromAfter(1).to(2) -> -30.0, // kernel block 1 & 2 <-- uncompressed
        intervalFromAfter(2).to(3) -> -30.0, // kernel block 1 & 2 <--
        intervalFromAfter(3).to(4) -> -20.0, // kernel block 1 & 2
        intervalFromAfter(4).to(5) -> -30.0, // kernel block 1 & 2 <-- uncompressed
        intervalFromAfter(5).to(6) -> -30.0, // kernel block 1 & 2 <--
        intervalFromAfter(6).to(7) -> -20.0, // kernel block 1 & 2
        intervalFromAfter(7).to(8) -> 10.0, // kernel block 1 & 2
        intervalFromAfter(8).to(9) -> 50.0, // kernel block 1
        intervalFromAfter(9).to(11) -> 60.0, // kernel block 1
        intervalFromAfter(11).to(12) -> 30.0 // kernel block 1
      )

      /**
        * Convolves multidimensional data in each dimension. The horizontal kernel is wider than the vertical kernel,
        * and the signal is not symmetrical.
        *
        * The test shows the horizontal bar blurs differently than the vertical bar because the kernel's horizontal
        * measure is much larger than its vertical height. It also validates that the asymmetric shape does not warp or
        * skew in unexpected ways.
        */
      // An asymmetric "L" shape.
      val signal3 = Seq(
        (interval(-5, 5) x interval(-1, 1)) -> 1.0, // longer horizontal  bar -- bottom of "L" shape
        (interval(-5, -3) x intervalFromAfter(1).to(4)) -> 1.0 // adjacent vertical bar -- left side of "L" shape
      )

      //  Y
      //  4 |         █ █ █ █ █
      //    |         █ █ █ █ █
      //  3 |         █ █ █ █ █
      //    |         █ █ █ █ █
      //  2 |         █ █ █ █ █
      //    |         █ █ █ █ █
      //  1 |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      //    |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      //  0 |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █             Grayscale Key:
      //    |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                      = 0.0
      // -1 |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                  █   = 1.0
      //    |
      // -2 |
      //    +--------------------------------------------------
      //     -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5  X

      // An asymmetric rectangle with unaligned step resolutions:
      val kernel3Horizontal =
        Seq(intervalFrom(0).toBefore(3) -> 2.0) // measure 3 horizontal kernel with larger value
      val kernel3Vertical =
        Seq(intervalFrom(0).toBefore(2) -> 1.0) // measure 2 vertical kernel with smaller value

      val convolved3Horizontal = convolvedByIn(
        dimensionIndex = 0,
        signalData = signal3,
        kernelData = kernel3Horizontal,
        kernelOrigin = 0,
        epsilon = 1,
        combine = _ * _,
        scaledByEpsilon = _ * _,
        accumulate = _ + _
      )
      convolved3Horizontal.getAll.toList shouldBe List(
        (intervalFrom(-7).toBefore(-6) x interval(-1, 4)) -> 2.0,
        (intervalFrom(-6).toBefore(-5) x interval(-1, 4)) -> 4.0,
        (interval(-5, 3) x interval(-1, 1)) -> 6.0,
        (intervalAt(-5) x intervalFromAfter(1).to(4)) -> 6.0,
        (intervalFromAfter(-5).to(-4) x intervalFromAfter(1).to(4)) -> 4.0,
        (intervalFromAfter(-4).to(-3) x intervalFromAfter(1).to(4)) -> 2.0,
        (intervalFromAfter(3).to(4) x interval(-1, 1)) -> 4.0,
        (intervalFromAfter(4).to(5) x interval(-1, 1)) -> 2.0
      )
      //  Y
      //  4 | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //    | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //  3 | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //    | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //  2 | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //    | ░ ░ ▒ ▒ █ █ ▒ ▒ ░ ░
      //  1 | ░ ░ ▒ ▒ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ▒ ▒ ░ ░             Grayscale Key:
      //    | ░ ░ ▒ ▒ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ▒ ▒ ░ ░                      = 0.0
      //  0 | ░ ░ ▒ ▒ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ▒ ▒ ░ ░                  ░   = 2.0
      //    | ░ ░ ▒ ▒ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ▒ ▒ ░ ░                  ▒   = 4.0
      // -1 | ░ ░ ▒ ▒ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ▒ ▒ ░ ░                  █   = 6.0
      //    |
      // -2 |
      //    +--------------------------------------------------
      //     -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5  X

      val convolved3Vertical = convolvedByIn(
        dimensionIndex = 1,
        signalData = signal3,
        kernelData = kernel3Vertical,
        kernelOrigin = 0,
        epsilon = 1,
        combine = _ * _,
        scaledByEpsilon = _ * _,
        accumulate = _ + _
      )
      convolved3Vertical.getAll.toList shouldBe List(
        (interval(-5, 5) x intervalFrom(-2).toBefore(-1)) -> 1.0,
        (interval(-5, 5) x interval(-1, 0)) -> 2.0,
        (interval(-5, -3) x intervalFromAfter(0).to(3)) -> 2.0,
        (interval(-5, -3) x intervalFromAfter(3).to(4)) -> 1.0,
        (intervalFromAfter(-3).to(5) x intervalFromAfter(0).to(1)) -> 1.0
      )
    //  Y
    //  4 |         ░ ░ ░ ░ ░
    //    |         ░ ░ ░ ░ ░
    //  3 |         █ █ █ █ █
    //    |         █ █ █ █ █
    //  2 |         █ █ █ █ █
    //    |         █ █ █ █ █
    //  1 |         █ █ █ █ █ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░
    //    |         █ █ █ █ █ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░
    //  0 |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █             Grayscale Key:
    //    |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                      = 0.0
    // -1 |         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █                  ░   = 1.0
    //    |         ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░                  █   = 2.0
    // -2 |         ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░ ░
    //    +--------------------------------------------------
    //     -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5  X
