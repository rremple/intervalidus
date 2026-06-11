package intervalidus.immutable

import intervalidus.*
import intervalidus.DiscreteAffineValue.IntDiscreteAffineValue
import intervalidus.Interval1D.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataAffineDiscreteTest
  extends AnyFunSuite
  with Matchers
  with DataAffineBaseBehaviors
  with ImmutableAffineBaseBehaviors:

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
    clippedDonut.reflectedAboutIn(dimensionIndex, pivot)

  override def donutReflectedAbout[V, D](pivot: Dim): DimensionalAffineBase[Double, Dim] =
    clippedDonut.reflectedAbout(pivot)

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

  testsFor(commonBehaviors("Discrete"))

  testsFor(affineBehaviors("Discrete"))

  // Important discrete interval measures:
  // clipInterval: [-10..10] x [-10..10] has measure 21 x 21
  // hole: [-1..1] x [-1..1] has measure 3 x 3

  /**
    * Continuous/discrete results are slightly different because of how measure is calculated Also the Scalar type must
    * resolve where there is a concrete reference to the affine domain value type (IntDiscreteAffineValue in the
    * discrete case).
    */
  test(s"Discrete: Int DataAffine scaling"):

    // - dimension 0 (horizontal)

    clippedDonut.scaledAboutIn(0, 1, 1.0) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21..2] ∪ [(2 + 1)..20] = [-21..20] has expected measure 2 * 21 = 42
    clippedDonut.scaledAboutIn(0, 1, 2.0).getAll.toList shouldBe List(
      (interval(-21, 2) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa scaled horizontally
      (intervalFrom(-21).toBefore(-3) x interval(-1, 10)) -> donutFilling, // clipped bb scaled horizontally
      (interval(-3, 20) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc scaled horizontally
      (intervalFromAfter(2).to(20) x interval(-10, 1)) -> donutFilling // clipped dd scaled horizontally
    )

    // horizontal [-3..2] has expected measure 3 * 2 = 6
    hole.scaledAboutIn(0, 1, 2.0).getAll.toList shouldBe List(
      (interval(-3, 2) x interval(-1, 1)) -> holeFilling
    )

    // - dimension 1 (vertical)

    clippedDonut.scaledAboutIn(1, 1, 1.0) ≡≡ clippedDonut // no-op scaling

    // vertical [-21..(-3 - 1)] ∪ [-3..20] = [-21..20] has expected measure 2 * 21 = 42
    clippedDonut.scaledAboutIn(1, 1, 2.0).getAll.toList shouldBe List(
      (interval(-10, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling, // clipped aa scaled vertically
      (intervalFrom(-10).toBefore(-1) x interval(-3, 20)) -> donutFilling, // clipped bb scaled vertically
      (interval(-1, 10) x intervalFromAfter(2).to(20)) -> donutFilling, // clipped cc scaled vertically
      (intervalFromAfter(1).to(10) x interval(-21, 2)) -> donutFilling // clipped dd scaled vertically
    )

    // vertical [-3..2] has expected measure 3 * 2 = 6
    hole.scaledAboutIn(1, 1, 2.0).getAll.toList shouldBe List(
      (interval(-1, 1) x interval(-3, 2)) -> holeFilling
    )

    // multi-dimensional

    clippedDonut.scaledAbout(Domain.in2D(1, 1), (1.0, 1.0)) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21..2] ∪ [(2 + 1)..20] = [-21..20] has expected measure 2 * 21 = 42
    // vertical [-21..(-3 - 1)] ∪ [-3..20] = [-21, 20] has expected measure 2 * 21 = 42
    clippedDonut.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0)).getAll.toList shouldBe List(
      (interval(-21, 2) x intervalFrom(-21).toBefore(-3)) -> donutFilling,
      (intervalFrom(-21).toBefore(-3) x interval(-3, 20)) -> donutFilling,
      (interval(-3, 20) x intervalFromAfter(2).to(20)) -> donutFilling,
      (intervalFromAfter(2).to(20) x interval(-21, 2)) -> donutFilling
    )

    // horizontal and vertical [-3..2] have expected measures 3 * 2 = 6
    hole.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0)).getAll.toList shouldBe List(
      (interval(-3, 2) x interval(-3, 2)) -> holeFilling
    )
