package intervalidus.laws

import intervalidus.*
import AffineGenerator.*
import DataGenerator.testCoreConfig
import DomainGenerator.{Dim1, Dim2, Dim3, Dim4, GenDomainOps}
import intervalidus.DomainAffineLike.given
import IntervalShapeGenerator.*
import intervalidus.Domain.{HasDisplacementType, HasScalarType}
import intervalidus.{ContinuousAffineValue, DiscreteAffineValue, DomainAffineLike, IntervalShape}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions
import scala.util.{Try, Success, Failure}

class IntervalShapeAffineLaws
  extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ParallelTestExecution
  with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 200 /*, workers = 2*/ )

  /**
    * Property tests that are applied to IntervalShapes with intervals in 1, 2, 3, and 4 dimensions.
    */
  trait AffineShapePropertyTest:
    def apply[D <: NonEmptyTuple: DomainAffineLike](
      shapeGen: Gen[IntervalShape[D]],
      displacementGen: Gen[TupleOfInts[D]],
      scaleGen: Gen[TupleOfDoubles[D]],
      elementGen: Gen[Interval[D]],
      centerGen: Gen[D]
    )(using
      D HasDisplacementType TupleOfInts[D],
      D HasScalarType TupleOfDoubles[D]
    ): Assertion

    def runFor[D <: NonEmptyTuple: DomainAffineLike: GenDomainOps: GenAffineOps](using
      D HasDisplacementType TupleOfInts[D],
      D HasScalarType TupleOfDoubles[D]
    ): Assertion = apply(
      gen[D](using config = testCoreConfig),
      genDisplacement[D],
      genScalar[D],
      IntervalGenerator.genBounded[D],
      DomainGenerator.genBoundedStart[D]
    )

  /**
    * Evaluate an IntervalShape property in 1, 2, 3, and 4 dimensions using both discrete and continuous interval domain
    * value semantics.
    */
  def affineShapeProperty(propertyName: String)(testFun: AffineShapePropertyTest): Unit =
    {
      import DiscreteAffineValue.IntDiscreteAffineValue
      property(s"4D Discrete   $propertyName")(testFun.runFor[Dim4])
      property(s"3D Discrete   $propertyName")(testFun.runFor[Dim3])
      property(s"2D Discrete   $propertyName")(testFun.runFor[Dim2])
      property(s"1D Discrete   $propertyName")(testFun.runFor[Dim1])
    }
    {
      import ContinuousAffineValue.IntContinuousAffineValue
      property(s"4D Continuous $propertyName")(testFun.runFor[Dim4])
      property(s"3D Continuous $propertyName")(testFun.runFor[Dim3])
      property(s"2D Continuous $propertyName")(testFun.runFor[Dim2])
      property(s"1D Continuous $propertyName")(testFun.runFor[Dim1])
    }

  extension [D <: NonEmptyTuple: DomainAffineLike](lhs: IntervalShape[D])
    infix def ≡≡(rhs: IntervalShape[D]): Assertion =
      assert(lhs ≡ rhs, s"\nExpected (rhs): ${rhs.toCodeLikeString}\nActual (lhs): ${lhs.toCodeLikeString}\n")

  /*
   * --- The actual property-based tests ---
   */

  import DomainAffineLike.* // extension methods
  import IntervalShape.∅

  /**
    * Proves the following properties of affine opearations which are true in all dimensions given any two shapes a and
    * b, an arbitrary scaling factor s, an arbitrary displacement d, and an arbitrary center c:
    *
    *   - Scaled inversion identity: a ≡ scale(scale(a, s, c), 1/s, c) ≡ reflect(reflect(a, c), c)
    *   - Scaled adjacency: iff a is adjacent to b, scale(a, s, c) is adjacent to scale(b, s, c). If s is positive, they
    *     are adjacent in the same way (both left or both right), where if s is negative, adjacency flips (left becomes
    *     right or right becomes left)
    *   - Scaled measure: measure(scale(a, s, c)) == measure(a) * |s|
    *   - Displacement identity: displace(displace(a, d), negate(d)) ≡ a
    *   - Idempotency of morphology: Once a shape is opened or closed, doing it a second time with the same structuring
    *     element and center should have absolutely no effect.
    *   - Monotonicity of morphology: If shape A ⊆ shape B, then the morphology operations must preserve that subset
    *     relationship.
    *   - Extensive and Anti-Extensive morphology: closing is extensive in that the original shape is always a subset of
    *     the result, and opening is anti-extensive in that the result is always a subset of the original shape.
    *   - Duality of morphology: Dilation and erosion are duals of each other. If you complement a shape, erode it with
    *     a structuring element, and then complement the result, you must get the dilation.
    *   - Gradient inclusion: a gradient is bounded by the dilation and erosion space, and always contains the true
    *     mathematical boundary of the set.
    *   - Gradient extensivity over the identity: if the structuring element contains the center, the gradient is
    *     guaranteed to contain the internal and external edge zones relative to the original shape.
    *   - Gradient scale monotonicity: if you scale up your structuring element, the gradient is guaranteed to grow
    *     monotonically.
    *   - Top-hat boundedness: the original set is perfectly partitioned by its opening and its white top-hat. Because
    *     (A ○ B) ⊆ A, the set difference is an exact decomposition, i.e., (A ○ B) ∪ (A wth B) ≡ A and (A ○ B) ∩ (A wth
    *     B) ≡ ∅ . Conversely, the closing can be broken down by the original set and the black top-hat, i.e., (A ● B) ≡
    *     A ∪ (A bth B).
    *   - Top-hat idempotency: applying a white top-hat transform a second time has no effect because all structures
    *     larger than the element have already been removed, i.e., (A wth B) wth B ≡ (A wth B).
    *   - Top-hat sieving: because a top-hat contains only structures smaller than the element, opening either the white
    *     or black top-hat reduces it to empty, i.e., (A wth B) ○ B ≡ (A bth B) ○ B ≡ ∅.
    *   - Top-hat duality: just like erosion and dilation are duals, the white and black top-hat transforms are exact
    *     duals of each other under set complementation A', i.e., (A wth B) ≡ (A' bth B)' and (A bth B) ≡ (A' wth B)'.
    *   - Translation invariance: both the gradient and the top-hat transforms are strictly translation invariant. If
    *     you translate your set A by some vector x, the resulting gradient or top-hat shifts by that exact same vector.
    */
  affineShapeProperty("Affine operations on shapes"):
    new AffineShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainAffineLike](
        shapeGen: Gen[IntervalShape[D]],
        displacementGen: Gen[TupleOfInts[D]],
        scaleGen: Gen[TupleOfDoubles[D]],
        elementGen: Gen[Interval[D]],
        centerGen: Gen[D]
      )(using
        D HasDisplacementType TupleOfInts[D],
        D HasScalarType TupleOfDoubles[D]
      ): Assertion =
        forAll(shapeGen, displacementGen, scaleGen, elementGen, centerGen): (shape, offset, scale, element, center) =>
          // Inversion
          shape.reflectedAbout(center).reflectedAbout(center) ≡≡ shape // scale = -1, and 1/(-1) = -1
          shape.scaledAbout(center, scale).scaledAbout(center, inverted(scale)) ≡≡ shape

          // Adjacency
          if shape.isEmpty then () // ignore
          else
            shape.allIntervals
              .zip(shape.allIntervals.drop(1))
              .foreach: (left, right) =>
                (left.scaledAbout(center, scale), right.scaledAbout(center, scale)) match
                  case (Some(leftScaled), Some(rightScaled)) =>
                    left.isAdjacentTo(right) shouldBe leftScaled.isAdjacentTo(rightScaled)
                  case _ => succeed

          // Measure
          shape.allIntervals.foreach: i =>
            i.scaledAbout(center, scale) match
              case None          => () // ignore
              case Some(iScaled) =>
                val measureThenScale = i.measure.map: m =>
                  mapScaledDisplacement(m, scale, (i, s) => (i * Math.abs(s)).toInt)
                val scaleThenMeasure = i.scaledAbout(center, scale).flatMap(_.measure)
                measureThenScale shouldBe scaleThenMeasure

          // Displacement
          shape.displacedBy(offset).displacedBy(negated(offset)) ≡≡ shape

          Try(element.withCenter(center)) match
            case Failure(_)     => succeed // ignore results if the element can't be reflected
            case Success(probe) =>

              // NOTE: erodedBy internally reflects the element to satisfy complementation duality.
              // To keep our test suite's spatial orientations aligned for direct comparison,
              // we pass the pre-reflected element here.
              val erodedShape = shape ⊖ probe.reflected // using reflected element
              val dilatedShape = shape ⊕ probe
              val openedShape = shape ○ probe
              val closedShape = shape ● probe
              val gradientShape = shape ∇ probe
              val whiteTopHatShape = shape wth probe
              val blackTopHatShape = shape bth probe

              // Idempotency
              openedShape ○ probe ≡≡ openedShape
              closedShape ● probe ≡≡ closedShape

              // Monotonicity
              val shapeB = shape + element // shape is a subset of shapeB
              (dilatedShape ⊆ (shapeB ⊕ probe)) shouldBe true

              // Extensive closing and Anti-Extensive opening
              (shape ⊆ closedShape) shouldBe true
              (openedShape ⊆ shape) shouldBe true

              // Duality
              (shape.c ⊖ probe.reflected).c ≡≡ dilatedShape

              // Gradient inclusion
              (gradientShape ∩ erodedShape) ≡≡ ∅
              (gradientShape ⊆ dilatedShape) shouldBe true

              val gradientShapeSuper = shape ∇ probe.containingCenter

              // Gradient extensivity over the identity
              (((shape ⊕ probe.containingCenter) \ shape) ⊆ gradientShapeSuper) shouldBe true
              ((shape \ (shape ⊖ probe.containingCenter)) ⊆ gradientShapeSuper) shouldBe true

              // Gradient scale monotonicity
              probe.containingCenter.element ⊕ probe.containingCenter match
                case Some(scaledProbeElement) =>
                  val scaledProbe = scaledProbeElement.withCenter(center)
                  (gradientShapeSuper ⊆ (shape ∇ scaledProbe)) shouldBe true
                case _ => succeed // ignore

              // Top-hat boundedness
              (openedShape ∪ whiteTopHatShape) ≡≡ shape
              (openedShape ∩ whiteTopHatShape) ≡≡ ∅
              (shape ∪ blackTopHatShape) ≡≡ closedShape

              // Top-hat idempotency
              val whiteTopHatShapeSuper = shape wth probe.containingCenter
              (whiteTopHatShapeSuper wth probe.containingCenter) ≡≡ whiteTopHatShapeSuper

              // Top-hat sieving
              (whiteTopHatShape ○ probe) ≡≡ ∅
              (blackTopHatShape ○ probe) ≡≡ ∅

              // Top-hat duality
              (shape.c bth probe.reflected) ≡≡ whiteTopHatShape
              (shape.c wth probe.reflected) ≡≡ blackTopHatShape

              // Translation invariance
              probe.element
                .displacedBy(offset)
                .flatMap: displacedElement =>
                  Try(displacedElement.withCenter(probe.center displacedBy offset)).toOption
                .map: displacedProbe =>
                  gradientShape.displacedBy(offset) ≡≡ (shape.displacedBy(offset) ∇ displacedProbe)
                  whiteTopHatShape.displacedBy(offset) ≡≡ (shape.displacedBy(offset) wth displacedProbe)
                  blackTopHatShape.displacedBy(offset) ≡≡ (shape.displacedBy(offset) bth displacedProbe)
                .getOrElse:
                  succeed // ignore
