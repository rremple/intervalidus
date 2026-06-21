package intervalidus.laws

import intervalidus.*
import AffineGenerator.*
import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
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
    def testDim1(
      shapeGen: Gen[IntervalShape[Dim1]],
      displacementGen: Gen[TupleOfInts[Dim1]],
      scaleGen: Gen[TupleOfDoubles[Dim1]],
      elementGen: Gen[Interval[Dim1]],
      centerGen: Gen[Dim1]
    )(using
      op: DomainAffineValueLike[Int]
    )(using
      DomainAffineLike[Dim1],
      Dim1 HasDisplacementType TupleOfInts[Dim1],
      Dim1 HasScalarType TupleOfDoubles[Dim1]
    ): Assertion

    def testDim2(
      shapeGen: Gen[IntervalShape[Dim2]],
      displacementGen: Gen[TupleOfInts[Dim2]],
      scaleGen: Gen[TupleOfDoubles[Dim2]],
      elementGen: Gen[Interval[Dim2]],
      centerGen: Gen[Dim2]
    )(using
      op: DomainAffineValueLike[Int]
    )(using
      DomainAffineLike[Dim2],
      Dim2 HasDisplacementType TupleOfInts[Dim2],
      Dim2 HasScalarType TupleOfDoubles[Dim2]
    ): Assertion

    def testDim3(
      shapeGen: Gen[IntervalShape[Dim3]],
      displacementGen: Gen[TupleOfInts[Dim3]],
      scaleGen: Gen[TupleOfDoubles[Dim3]],
      elementGen: Gen[Interval[Dim3]],
      centerGen: Gen[Dim3]
    )(using
      op: DomainAffineValueLike[Int]
    )(using
      DomainAffineLike[Dim3],
      Dim3 HasDisplacementType TupleOfInts[Dim3],
      Dim3 HasScalarType TupleOfDoubles[Dim3]
    ): Assertion

    def testDim4(
      shapeGen: Gen[IntervalShape[Dim4]],
      displacementGen: Gen[TupleOfInts[Dim4]],
      scaleGen: Gen[TupleOfDoubles[Dim4]],
      elementGen: Gen[Interval[Dim4]],
      centerGen: Gen[Dim4]
    )(using
      op: DomainAffineValueLike[Int]
    )(using
      DomainAffineLike[Dim4],
      Dim4 HasDisplacementType TupleOfInts[Dim4],
      Dim4 HasScalarType TupleOfDoubles[Dim4]
    ): Assertion

  /**
    * Evaluate an IntervalShape property in 1, 2, 3, and 4 dimensions using both discrete and continuous interval domain
    * value semantics.
    */
  def affineShapeProperty(propertyName: String)(testFun: AffineShapePropertyTest): Unit =
    import DataGenerator.testCoreConfig
    {
      import DiscreteAffineValue.IntDiscreteAffineValue
      property(s"4D Discrete   $propertyName")(
        testFun.testDim4(
          genDim4(using testCoreConfig),
          genDisplacemenDim4,
          genScalarDim4,
          IntervalGenerator.genBoundedDim4,
          DomainGenerator.genBoundedStartDim4
        )
      )
      property(s"3D Discrete   $propertyName")(
        testFun.testDim3(
          genDim3(using testCoreConfig),
          genDisplacemenDim3,
          genScalarDim3,
          IntervalGenerator.genBoundedDim3,
          DomainGenerator.genBoundedStartDim3
        )
      )
      property(s"2D Discrete   $propertyName")(
        testFun.testDim2(
          genDim2(using testCoreConfig),
          genDisplacemenDim2,
          genScalarDim2,
          IntervalGenerator.genBoundedDim2,
          DomainGenerator.genBoundedStartDim2
        )
      )
      property(s"1D Discrete   $propertyName")(
        testFun.testDim1(
          genDim1(using testCoreConfig),
          genDisplacemenDim1,
          genScalarDim1,
          IntervalGenerator.genBoundedDim1,
          DomainGenerator.genBoundedStartDim1
        )
      )
    }
    {
      import ContinuousAffineValue.IntContinuousAffineValue
      property(s"4D Continuous $propertyName")(
        testFun.testDim4(
          genDim4(using testCoreConfig),
          genDisplacemenDim4,
          genScalarDim4,
          IntervalGenerator.genBoundedDim4,
          DomainGenerator.genBoundedStartDim4
        )
      )
      property(s"3D Continuous $propertyName")(
        testFun.testDim3(
          genDim3(using testCoreConfig),
          genDisplacemenDim3,
          genScalarDim3,
          IntervalGenerator.genBoundedDim3,
          DomainGenerator.genBoundedStartDim3
        )
      )
      property(s"2D Continuous $propertyName")(
        testFun.testDim2(
          genDim2(using testCoreConfig),
          genDisplacemenDim2,
          genScalarDim2,
          IntervalGenerator.genBoundedDim2,
          DomainGenerator.genBoundedStartDim2
        )
      )
      property(s"1D Continuous $propertyName")(
        testFun.testDim1(
          genDim1(using testCoreConfig),
          genDisplacemenDim1,
          genScalarDim1,
          IntervalGenerator.genBoundedDim1,
          DomainGenerator.genBoundedStartDim1
        )
      )
    }

  extension [D <: NonEmptyTuple: DomainAffineLike](lhs: IntervalShape[D])
    infix def ≡≡(rhs: IntervalShape[D]): Assertion = assert(lhs ≡ rhs)

  /*
   * --- The actual property-based tests ---
   */

  import DomainAffineLike.*

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
    *   - Anti-Extensive morphology: Opening is anti-extensive in that the result is always a subset of the original
    *     shape.
    *
    * Also proves the following properties of affine opearations only in the one-dimensional case. (Trailing operation
    * is erosion, and that leaves gap artifacts on the boundaries of partially adjacent interval components of a
    * higher-dimensional shape):
    *   - Extensive morphology: Closing is extensive in that the original shape is always a subset of the result.
    *   - Duality of morphology: Dilation and erosion are duals of each other. If you complement a shape, erode it with
    *     a structuring element, and then complement the result, you must get the dilation.
    */
  affineShapeProperty("Affine operations on shapes"):
    new AffineShapePropertyTest:
      override def testDim1(
        shapeGen: Gen[IntervalShape[Dim1]],
        displacementGen: Gen[TupleOfInts[Dim1]],
        scaleGen: Gen[TupleOfDoubles[Dim1]],
        elementGen: Gen[Interval[Dim1]],
        centerGen: Gen[Dim1]
      )(using
        op: DomainAffineValueLike[Int]
      )(using
        DomainAffineLike[Dim1],
        Dim1 HasDisplacementType TupleOfInts[Dim1],
        Dim1 HasScalarType TupleOfDoubles[Dim1]
      ): Assertion =
        forAll(shapeGen, displacementGen, scaleGen, elementGen, centerGen): (shape, offset, scale, element, center) =>
          // Inversion
          shape.reflectedAbout(center).reflectedAbout(center) ≡≡ shape // scale = -1, and 1/(-1) = -1
          shape.scaledAbout(center, scale).scaledAbout(center, inverted(scale)) ≡≡ shape

          // Adjacency
          if shape.isEmpty then () // succeed
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
              case None          => ()
              case Some(iScaled) =>
                val measureThenScale =
                  i.measure.map: m =>
                    mapScaledDisplacement(m, scale, (i, s) => (i * Math.abs(s)).toInt)
                val scaleThenMeasure =
                  i.scaledAbout(center, scale)
                    .flatMap(_.measure)
                measureThenScale shouldBe scaleThenMeasure

          // Displacement
          shape.displacedBy(offset).displacedBy(negated(offset)) ≡≡ shape

          // Idempotency
          shape.openingBy(element, center).openingBy(element, center) ≡≡ shape.openingBy(element, center)
          shape.closingBy(element, center).closingBy(element, center) ≡≡ shape.closingBy(element, center)

          // Monotonicity
          val shapeB = shape + element // shape is a subset of shapeB
          (shape.dilatedBy(element, center) ⊆ shapeB.dilatedBy(element, center)) shouldBe true

          // Anti-Extensive
          (shape.openingBy(element, center) isSubsetOf shape) shouldBe true

          // Extensive (1D)
          (shape isSubsetOf shape.closingBy(element, center)) shouldBe true

          // Duality (1D)
          element.reflectedAbout(center) match
            case Some(reflected) => shape.dilatedBy(element, center) ≡≡ shape.c.erodedBy(reflected, center).c
            case _               => succeed

      override def testDim2(
        shapeGen: Gen[IntervalShape[Dim2]],
        displacementGen: Gen[TupleOfInts[Dim2]],
        scaleGen: Gen[TupleOfDoubles[Dim2]],
        elementGen: Gen[Interval[Dim2]],
        centerGen: Gen[Dim2]
      )(using
        op: DomainAffineValueLike[Int]
      )(using
        DomainAffineLike[Dim2],
        Dim2 HasDisplacementType TupleOfInts[Dim2],
        Dim2 HasScalarType TupleOfDoubles[Dim2]
      ): Assertion =
        forAll(shapeGen, displacementGen, scaleGen, elementGen, centerGen): (shape, offset, scale, element, center) =>
          // Inversion
          shape.reflectedAbout(center).reflectedAbout(center) ≡≡ shape // scale = -1, and 1/(-1) = -1
          shape.scaledAbout(center, scale).scaledAbout(center, inverted(scale)) ≡≡ shape

          // Adjacency
          if shape.isEmpty then () // succeed
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
              case None          => ()
              case Some(iScaled) =>
                val measureThenScale = i.measure.map: m =>
                  mapScaledDisplacement(m, scale, (i, s) => (i * Math.abs(s)).toInt)
                val scaleThenMeasure = i.scaledAbout(center, scale).flatMap(_.measure)
                measureThenScale shouldBe scaleThenMeasure

          // Displacement
          shape.displacedBy(offset).displacedBy(negated(offset)) ≡≡ shape

          // Idempotency
          shape.openingBy(element, center).openingBy(element, center) ≡≡ shape.openingBy(element, center)
          shape.closingBy(element, center).closingBy(element, center) ≡≡ shape.closingBy(element, center)

          // Monotonicity
          val shapeB = shape + element // shape is a subset of shapeB
          (shape.dilatedBy(element, center) ⊆ shapeB.dilatedBy(element, center)) shouldBe true

          // Anti-Extensive
          (shape.openingBy(element, center) isSubsetOf shape) shouldBe true

      override def testDim3(
        shapeGen: Gen[IntervalShape[Dim3]],
        displacementGen: Gen[TupleOfInts[Dim3]],
        scaleGen: Gen[TupleOfDoubles[Dim3]],
        elementGen: Gen[Interval[Dim3]],
        centerGen: Gen[Dim3]
      )(using
        op: DomainAffineValueLike[Int]
      )(using
        DomainAffineLike[Dim3],
        Dim3 HasDisplacementType TupleOfInts[Dim3],
        Dim3 HasScalarType TupleOfDoubles[Dim3]
      ): Assertion =
        forAll(shapeGen, displacementGen, scaleGen, elementGen, centerGen): (shape, offset, scale, element, center) =>
          // Inversion
          shape.reflectedAbout(center).reflectedAbout(center) ≡≡ shape // scale = -1, and 1/(-1) = -1
          shape.scaledAbout(center, scale).scaledAbout(center, inverted(scale)) ≡≡ shape

          // Adjacency
          if shape.isEmpty then () // succeed
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
              case None          => ()
              case Some(iScaled) =>
                val measureThenScale = i.measure.map: m =>
                  mapScaledDisplacement(m, scale, (i, s) => (i * Math.abs(s)).toInt)
                val scaleThenMeasure = i.scaledAbout(center, scale).flatMap(_.measure)
                measureThenScale shouldBe scaleThenMeasure

          // Displacement
          shape.displacedBy(offset).displacedBy(negated(offset)) ≡≡ shape

          // Idempotency
          shape.openingBy(element, center).openingBy(element, center) ≡≡ shape.openingBy(element, center)
          shape.closingBy(element, center).closingBy(element, center) ≡≡ shape.closingBy(element, center)

          // Monotonicity
          val shapeB = shape + element // shape is a subset of shapeB
          (shape.dilatedBy(element, center) ⊆ shapeB.dilatedBy(element, center)) shouldBe true

          // Anti-Extensive
          (shape.openingBy(element, center) isSubsetOf shape) shouldBe true

      override def testDim4(
        shapeGen: Gen[IntervalShape[Dim4]],
        displacementGen: Gen[TupleOfInts[Dim4]],
        scaleGen: Gen[TupleOfDoubles[Dim4]],
        elementGen: Gen[Interval[Dim4]],
        centerGen: Gen[Dim4]
      )(using
        op: DomainAffineValueLike[Int]
      )(using
        DomainAffineLike[Dim4],
        Dim4 HasDisplacementType TupleOfInts[Dim4],
        Dim4 HasScalarType TupleOfDoubles[Dim4]
      ): Assertion =
        forAll(shapeGen, displacementGen, scaleGen, elementGen, centerGen): (shape, offset, scale, element, center) =>
          // Inversion
          shape.reflectedAbout(center).reflectedAbout(center) ≡≡ shape // scale = -1, and 1/(-1) = -1
          shape.scaledAbout(center, scale).scaledAbout(center, inverted(scale)) ≡≡ shape

          // Adjacency
          if shape.isEmpty then () // succeed
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
              case None          => ()
              case Some(iScaled) =>
                val measureThenScale = i.measure.map: m =>
                  mapScaledDisplacement(m, scale, (i, s) => (i * Math.abs(s)).toInt)
                val scaleThenMeasure = i.scaledAbout(center, scale).flatMap(_.measure)
                measureThenScale shouldBe scaleThenMeasure

          // Displacement
          shape.displacedBy(offset).displacedBy(negated(offset)) ≡≡ shape

          // Idempotency
          shape.openingBy(element, center).openingBy(element, center) ≡≡ shape.openingBy(element, center)
          shape.closingBy(element, center).closingBy(element, center) ≡≡ shape.closingBy(element, center)

          // Monotonicity
          val shapeB = shape + element // shape is a subset of shapeB
          (shape.dilatedBy(element, center) ⊆ shapeB.dilatedBy(element, center)) shouldBe true

          // Anti-Extensive
          (shape.openingBy(element, center) isSubsetOf shape) shouldBe true
