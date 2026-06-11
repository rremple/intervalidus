package intervalidus.mutable

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
  with MutableAffineBaseBehaviors:

  /*
   * Implementations for DataAffineBaseBehaviors.
   */

  override def donutReflectedAboutIn(
    dimensionIndex: Domain.DimensionIndex,
    pivot: Domain1D[Int]
  )(using
    Domain.HasIndex[Dim, dimensionIndex.type],
    Domain.IsAtIndex[Dim, dimensionIndex.type, Int],
    Domain.IsUpdatableAtIndex[Dim, dimensionIndex.type, Int]
  ): DimensionalAffineBase[Double, Dim] =
    clippedDonut.mutate(_.reflectedAboutIn(dimensionIndex, pivot))

  override def donutReflectedAbout[V, D](pivot: Dim): DimensionalAffineBase[Double, Dim] =
    clippedDonut.mutate(_.reflectedAbout(pivot))

  override def donutDisplacedByIn(
    dimensionIndex: Domain.DimensionIndex,
    offset: Int
  )(using
    Domain.HasIndex[Dim, dimensionIndex.type],
    Domain.IsAtIndex[Dim, dimensionIndex.type, Int],
    Domain.IsUpdatableAtIndex[Dim, dimensionIndex.type, Int]
  ): DimensionalAffineBase[Double, Dim] =
    clippedDonut.mutate(_.displacedByIn(dimensionIndex, offset))

  override def donutDisplacedBy(offset: (Int, Int)): DimensionalAffineBase[Double, Dim] =
    clippedDonut.mutate(_.displacedBy(offset))

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
  ): DimensionalAffineBase[V, D] =
    val result = DataAffine(signalData)
    result.convolvedByIn(
      dimensionIndex,
      DataAffine(kernelData),
      kernelOrigin,
      epsilon,
      combine,
      scaledByEpsilon,
      accumulate
    )
    result

  testsFor(commonBehaviors("Continuous"))

  testsFor(affineBehaviors("Continuous"))

  // Important continuous interval measures:
  // clipInterval: [-10, 10] x [-10, 10] has measure 20 x 20
  // hole: [-1, 1] x [-1, 1] has measure 2 x 2

  /**
    * Continuous/discrete results are slightly different because of how measure is calculated Also the Scalar type must
    * resolve where there is a concrete reference to the affine domain value type (IntContinuousAffineValue in the
    * continuous case).
    */
  test(s"Continuous: Int DataAffine scaling"):

    // - dimension 0 (horizontal)

    clippedDonut.mutate(_.scaledAboutIn(0, 1, 1.0)) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21, 1] ∪ (1, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.mutate(_.scaledAboutIn(0, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-21, 1) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa scaled horizontally
      (intervalFrom(-21).toBefore(-3) x interval(-1, 10)) -> donutFilling, // clipped bb scaled horizontally
      (interval(-3, 19) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc scaled horizontally
      (intervalFromAfter(1).to(19) x interval(-10, 1)) -> donutFilling // clipped dd scaled horizontally
    )

    // horizontal [-3, 1] has expected measure 2 * 2 = 4
    hole.mutate(_.scaledAboutIn(0, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-3, 1) x interval(-1, 1)) -> holeFilling
    )

    // - dimension 1 (vertical)

    clippedDonut.mutate(_.scaledAboutIn(1, 1, 1.0)) ≡≡ clippedDonut // no-op scaling

    // vertical [-21, -3) ∪ [-3, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.mutate(_.scaledAboutIn(1, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-10, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling, // clipped aa scaled vertically
      (intervalFrom(-10).toBefore(-1) x interval(-3, 19)) -> donutFilling, // clipped bb scaled vertically
      (interval(-1, 10) x intervalFromAfter(1).to(19)) -> donutFilling, // clipped cc scaled vertically
      (intervalFromAfter(1).to(10) x interval(-21, 1)) -> donutFilling // clipped dd scaled vertically
    )
    // vertical [-3, 1] has expected measure 2 * 2 = 4
    hole.mutate(_.scaledAboutIn(1, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-1, 1) x interval(-3, 1)) -> holeFilling
    )

    // multi-dimensional

    clippedDonut.mutate(_.scaledAbout(Domain.in2D(1, 1), (1.0, 1.0))) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21, 1] ∪ (1, 19] = [-21, 19] has expected measure 2 * 20 = 40
    // vertical [-21, -3) ∪ [-3, 19] = [-21, 19] has expected measure 2 * 20 = 40
    clippedDonut.mutate(_.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0))).getAll.toList shouldBe List(
      (interval(-21, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling,
      (intervalFrom(-21).toBefore(-3) x interval(-3, 19)) -> donutFilling,
      (interval(-3, 19) x intervalFromAfter(1).to(19)) -> donutFilling,
      (intervalFromAfter(1).to(19) x interval(-21, 1)) -> donutFilling
    )
    // horizontal and vertical [-3, 1] have expected measures 2 * 2 = 4
    hole.mutate(_.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0))).getAll.toList shouldBe List(
      (interval(-3, 1) x interval(-3, 1)) -> holeFilling
    )
