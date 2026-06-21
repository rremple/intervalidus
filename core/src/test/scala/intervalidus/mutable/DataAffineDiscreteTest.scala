package intervalidus.mutable

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
  with MutableAffineBaseBehaviors:

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
    clippedDonut.mutate(_.displacedByIn(dimensionIndex, offset))

  override def donutDisplacedBy(offset: (Int, Int)): DimensionalAffineBase[Double, Dim] =
    clippedDonut.mutate(_.displacedBy(offset))

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

  testsFor(commonBehaviors("Discrete mutable"))

  testsFor(affineBehaviors("Discrete mutable"))

  // Important discrete interval measures:
  // clipInterval: [-10..10] x [-10..10] has measure 21 x 21
  // hole: [-1..1] x [-1..1] has measure 3 x 3

  /*
   * Continuous/discrete results of reflection and scaling are slightly different because of how each measures interval
   * width and how the center translates to the actual axis of reflection/scaling.
   */

  test("Discrete mutable: Int DataAffine reflecting"):

    // - dimension 0 (horizontal)

    // Choosing an axis left of a 0 center yields a donut lobsided on the right, so the reflection is lobsided on the
    // left, so it will match the donut shifted left by 1.
    clippedDonut.mutate(_.reflectedAboutIn(0, 0)) ≡≡ clippedDonut.mutate(_.displacedBy((-1, 0))) // nearly symmetric

    // Choosing an axis left of a 1 center yields a donut lobsided on the left, so the reflection is lobsided on the
    // right, so it will match the donut shifted right by 1.
    clippedDonut.mutate(_.reflectedAboutIn(0, 1)) ≡≡ clippedDonut.mutate(_.displacedBy((1, 0))) // nearly symmetric

    // And there's nothing between 0 and 1 that you can use as the center in a discrete domain.

    clippedDonut.mutate(_.reflectedAboutIn(0, 1)).mutate(_.displacedBy((1, 0))).getAll.toList shouldBe List(
      (intervalFrom(-8).toBefore(1) x interval(-10, 1)) -> donutFilling, // clipped dd reflected horizontally
      (interval(-8, 3) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc reflected horizontally
      (interval(1, 12) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa reflected horizontally
      (intervalFromAfter(3).to(12) x interval(-1, 10)) -> donutFilling // clipped bb reflected horizontally
    )

    // - dimension 1 (vertical)

    clippedDonut.mutate(_.reflectedAboutIn(1, 0)) ≡≡ clippedDonut.mutate(_.displacedBy((0, -1))) // nearly symmetric

    clippedDonut.mutate(_.reflectedAboutIn(1, 1)).mutate(_.displacedBy((0, 1))).getAll.toList shouldBe List(
      (intervalFrom(-10)
        .toBefore(-1) x interval(-8, 3)) -> donutFilling, // clipped part of bb+dd reflected vertically
      (interval(-10, 1) x intervalFromAfter(3).to(12)) -> donutFilling, // clipped aa reflected vertically
      (interval(-1, 10) x intervalFrom(-8).toBefore(1)) -> donutFilling, // clipped cc reflected vertically
      (intervalFromAfter(1).to(10) x interval(1, 12)) -> donutFilling // clipped part of bb+dd reflected vertically
    )

    // multi-dimensional

    clippedDonut.mutate(_.reflectedAbout(Domain.in2D(0, 0))) ≡≡ clippedDonut.mutate(
      _.displacedBy((-1, -1))
    ) // nearly symmetric

    clippedDonut.mutate(_.reflectedAbout(Domain.in2D(1, 1))).mutate(_.displacedBy((1, 1))).getAll.toList shouldBe List(
      (interval(-8, 3) x intervalFrom(-8).toBefore(1)) -> donutFilling,
      (intervalFrom(-8).toBefore(1) x interval(1, 12)) -> donutFilling,
      (interval(1, 12) x intervalFromAfter(3).to(12)) -> donutFilling,
      (intervalFromAfter(3).to(12) x interval(-8, 3)) -> donutFilling
    )

  test(s"Discrete mutable: Int DataAffine scaling"):

    // - dimension 0 (horizontal)

    clippedDonut.mutate(_.scaledAboutIn(0, 1, 1.0)) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21..2] ∪ [(2 + 1)..20] = [-21..20] has expected measure 2 * 21 = 42
    clippedDonut.mutate(_.scaledAboutIn(0, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-21, 2) x intervalFrom(-10).toBefore(-1)) -> donutFilling, // clipped aa scaled horizontally
      (intervalFrom(-21).toBefore(-3) x interval(-1, 10)) -> donutFilling, // clipped bb scaled horizontally
      (interval(-3, 20) x intervalFromAfter(1).to(10)) -> donutFilling, // clipped cc scaled horizontally
      (intervalFromAfter(2).to(20) x interval(-10, 1)) -> donutFilling // clipped dd scaled horizontally
    )

    // horizontal [-3..2] has expected measure 3 * 2 = 6
    hole.mutate(_.scaledAboutIn(0, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-3, 2) x interval(-1, 1)) -> holeFilling
    )

    // - dimension 1 (vertical)

    clippedDonut.mutate(_.scaledAboutIn(1, 1, 1.0)) ≡≡ clippedDonut // no-op scaling

    // vertical [-21..(-3 - 1)] ∪ [-3..20] = [-21..20] has expected measure 2 * 21 = 42
    clippedDonut.mutate(_.scaledAboutIn(1, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-10, 1) x intervalFrom(-21).toBefore(-3)) -> donutFilling, // clipped aa scaled vertically
      (intervalFrom(-10).toBefore(-1) x interval(-3, 20)) -> donutFilling, // clipped bb scaled vertically
      (interval(-1, 10) x intervalFromAfter(2).to(20)) -> donutFilling, // clipped cc scaled vertically
      (intervalFromAfter(1).to(10) x interval(-21, 2)) -> donutFilling // clipped dd scaled vertically
    )

    // vertical [-3..2] has expected measure 3 * 2 = 6
    hole.mutate(_.scaledAboutIn(1, 1, 2.0)).getAll.toList shouldBe List(
      (interval(-1, 1) x interval(-3, 2)) -> holeFilling
    )

    // multi-dimensional

    clippedDonut.mutate(_.scaledAbout(Domain.in2D(1, 1), (1.0, 1.0))) ≡≡ clippedDonut // no-op scaling

    // horizontal [-21..2] ∪ [(2 + 1)..20] = [-21..20] has expected measure 2 * 21 = 42
    // vertical [-21..(-3 - 1)] ∪ [-3..20] = [-21, 20] has expected measure 2 * 21 = 42
    clippedDonut.mutate(_.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0))).getAll.toList shouldBe List(
      (interval(-21, 2) x intervalFrom(-21).toBefore(-3)) -> donutFilling,
      (intervalFrom(-21).toBefore(-3) x interval(-3, 20)) -> donutFilling,
      (interval(-3, 20) x intervalFromAfter(2).to(20)) -> donutFilling,
      (intervalFromAfter(2).to(20) x interval(-21, 2)) -> donutFilling
    )

    // horizontal and vertical [-3..2] have expected measures 3 * 2 = 6
    hole.mutate(_.scaledAbout(Domain.in2D(1, 1), (2.0, 2.0))).getAll.toList shouldBe List(
      (interval(-3, 2) x interval(-3, 2)) -> holeFilling
    )
