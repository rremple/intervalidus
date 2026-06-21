package intervalidus.immutable

import intervalidus.*
import intervalidus.ContinuousAffineValue.IntContinuousAffineValue
import intervalidus.Interval1D.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataAffineContinuousTest
  extends AnyFunSuite
  with Matchers
  with DataAffineBaseBehaviors
  with ImmutableAffineBaseBehaviors:

  /*
   * Implementations for DataAffineBaseBehaviors.
   */

  override def donutDisplacedByIn(
    dimensionIndex: Domain.DimensionIndex,
    offset: Int
  )(using
    Domain.HasIndex[Dim, dimensionIndex.type],
    Domain.IsAtIndex[Dim, dimensionIndex.type, Int],
    Domain.IsUpdatableAtIndex[Dim, dimensionIndex.type, Int]
  ): DimensionalAffineBase[Double, Dim] =
    clippedDonut.displacedByIn(dimensionIndex, offset)

  override def donutDisplacedBy(offset: (Int, Int)): DimensionalAffineBase[Double, Dim] =
    clippedDonut.displacedBy(offset)

  def donutBoundingShape(thickness: (Int, Int)): IntervalShape[Dim] =
    clippedDonut.boundingShape(thickness)

  override def convolvedByIn[V, D <: NonEmptyTuple: DomainAffineLike, K](
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
  ): DimensionalAffineBase[V, D] = DataAffine(signalData).convolvedByIn(
    dimensionIndex,
    DataAffine(kernelData),
    kernelOrigin,
    epsilon,
    combine,
    scaledByEpsilon,
    accumulate
  )

  testsFor(commonBehaviors("Continuous immutable"))

  testsFor(affineBehaviors("Continuous immutable"))

  // Important continuous interval measures:
  // clipInterval: [-10, 10] x [-10, 10] has measure 20 x 20
  // hole: [-1, 1] x [-1, 1] has measure 2 x 2

  /*
   * Continuous/discrete results of reflection and scaling are slightly different because of how each measures interval
   * width and how the center translates to the actual axis of reflection/scaling.
   */

  test("Continuous immutable: Int DataAffine reflecting"):

    // - dimension 0 (horizontal)

    clippedDonut.reflectedAboutIn(0, 0) ≡≡ clippedDonut // no-op reflection (since the donut is symmetric)

    clippedDonut.reflectedAboutIn(0, 1).getAll.toList shouldBe List(
      (intervalFrom(-8).toBefore(1) x interval(-10, 1)) -> donutFilling, // clipped dd reflected horizontally
      (interval(-8, 3) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc reflected horizontally
      (interval(1, 12) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa reflected horizontally
      (intervalFromAfter(3).to(12) x interval(-1, 10)) -> donutFilling // clipped bb reflected horizontally
    )

    // - dimension 1 (vertical)

    clippedDonut.reflectedAboutIn(1, 0) ≡≡ clippedDonut // no-op reflection (since the donut is symmetric)

    clippedDonut.reflectedAboutIn(1, 1).getAll.toList shouldBe List(
      (intervalFrom(-10)
        .toBefore(-1) x interval(-8, 3)) -> donutFilling, // clipped part of bb+dd reflected vertically
      (interval(-10, 1) x intervalFromAfter(3).to(12)) -> donutFilling, // clipped aa reflected vertically
      (interval(-1, 10) x intervalFrom(-8).toBefore(1)) -> donutFilling, // clipped cc reflected vertically
      (intervalFromAfter(1).to(10) x interval(1, 12)) -> donutFilling // clipped part of bb+dd reflected vertically
    )

    // multi-dimensional

    clippedDonut.reflectedAbout(Domain.in2D(0, 0)) ≡≡ clippedDonut // no-op reflection (since the donut is symmetric)

    clippedDonut.reflectedAbout(Domain.in2D(1, 1)).getAll.toList shouldBe List(
      (interval(-8, 3) x intervalFrom(-8).toBefore(1)) -> donutFilling,
      (intervalFrom(-8).toBefore(1) x interval(1, 12)) -> donutFilling,
      (interval(1, 12) x intervalFromAfter(3).to(12)) -> donutFilling,
      (intervalFromAfter(3).to(12) x interval(-8, 3)) -> donutFilling
    )

  test(s"Continuous immutable: Int DataAffine scaling"):

    // - dimension 0 (horizontal)

    clippedDonut.scaledAboutIn(0, 1, 1.0) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21, 1] ∪ (1, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.scaledAboutIn(0, 1, 2.0).getAll.toList shouldBe List(
      (interval(-21, 1) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa scaled horizontally
      (intervalFrom(-21).toBefore(-3) x interval(-1, 10)) -> donutFilling, // clipped bb scaled horizontally
      (interval(-3, 19) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc scaled horizontally
      (intervalFromAfter(1).to(19) x interval(-10, 1)) -> donutFilling // clipped dd scaled horizontally
    )

    // horizontal [-3, 1] has expected measure 2 * 2 = 4
    hole.scaledAboutIn(0, 1, 2.0).getAll.toList shouldBe List(
      (interval(-3, 1) x interval(-1, 1)) -> holeFilling
    )

    // - dimension 1 (vertical)

    clippedDonut.scaledAboutIn(1, 1, 1.0) ≡≡ clippedDonut // no-op scaling

    // vertical [-21, -3) ∪ [-3, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.scaledAboutIn(1, 1, 2.0).getAll.toList shouldBe List(
      (interval(-10, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling, // clipped aa scaled vertically
      (intervalFrom(-10).toBefore(-1) x interval(-3, 19)) -> donutFilling, // clipped bb scaled vertically
      (interval(-1, 10) x intervalFromAfter(1).to(19)) -> donutFilling, // clipped cc scaled vertically
      (intervalFromAfter(1).to(10) x interval(-21, 1)) -> donutFilling // clipped dd scaled vertically
    )
    // vertical [-3, 1] has expected measure 2 * 2 = 4
    hole.scaledAboutIn(1, 1, 2.0).getAll.toList shouldBe List(
      (interval(-1, 1) x interval(-3, 1)) -> holeFilling
    )

    // multi-dimensional

    clippedDonut.scaledAbout(Domain.in2D(1, 1), (1.0, 1.0)) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21, 1] ∪ (1, 19] = [-21, 19] has expected measure 2 * 20 = 40
    // vertical [-21, -3) ∪ [-3, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0)).getAll.toList shouldBe List(
      (interval(-21, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling,
      (intervalFrom(-21).toBefore(-3) x interval(-3, 19)) -> donutFilling,
      (interval(-3, 19) x intervalFromAfter(1).to(19)) -> donutFilling,
      (intervalFromAfter(1).to(19) x interval(-21, 1)) -> donutFilling
    )
    // horizontal and vertical [-3, 1] have expected measures 2 * 2 = 4
    hole.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0)).getAll.toList shouldBe List(
      (interval(-3, 1) x interval(-3, 1)) -> holeFilling
    )
