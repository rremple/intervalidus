package intervalidus.laws

import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
import intervalidus.DomainLike.given
import intervalidus.{ContinuousValue, DiscreteValue, DomainLike}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

class DomainLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 200 /*, workers = 2*/ )

  /**
    * Property tests that are applied in 1, 2, 3, and 4 dimensions.
    */
  trait ManyDimensionsPropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](domainGen: Gen[D]): Assertion

  /**
    * Evaluate a property in 1, 2, 3, and 4 dimensions using only discrete value semantics.
    */
  def manyDimensionsDiscreteProperty(propertyName: String)(testFun: ManyDimensionsPropertyTest): Unit =
    import DiscreteValue.IntDiscreteValue
    property(s"4D Discrete   $propertyName")(testFun[Dim4](DomainGenerator.genDim4))
    property(s"3D Discrete   $propertyName")(testFun[Dim3](DomainGenerator.genDim3))
    property(s"2D Discrete   $propertyName")(testFun[Dim2](DomainGenerator.genDim2))
    property(s"1D Discrete   $propertyName")(testFun[Dim1](DomainGenerator.genDim1))

  /**
    * Evaluate a property in 1, 2, 3, and 4 dimensions using only continuous value semantics.
    */
  def manyDimensionsContinuousProperty(propertyName: String)(testFun: ManyDimensionsPropertyTest): Unit =
    import ContinuousValue.IntContinuousValue
    property(s"4D Continuous $propertyName")(testFun[Dim4](DomainGenerator.genDim4))
    property(s"3D Continuous $propertyName")(testFun[Dim3](DomainGenerator.genDim3))
    property(s"2D Continuous $propertyName")(testFun[Dim2](DomainGenerator.genDim2))
    property(s"1D Continuous $propertyName")(testFun[Dim1](DomainGenerator.genDim1))

  /**
    * Evaluate a property in 1, 2, 3, and 4 dimensions using both discrete and continuous value semantics.
    */
  def manyDimensionsProperty(propertyName: String)(testFun: ManyDimensionsPropertyTest): Unit =
    manyDimensionsDiscreteProperty(propertyName)(testFun)
    manyDimensionsContinuousProperty(propertyName)(testFun)

  /*
   * --- The actual property-based tests ---
   */

  /**
    * If we could choose Int.MaxValue or Int.MinValue, this could break for discrete domains
    */
  manyDimensionsProperty("adjacency is symmetric: A ~> B => A <~ B"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](domainGen: Gen[D]): Assertion =
        forAll(domainGen): a =>
          a.leftAdjacent.rightAdjacent shouldBe a
          a.rightAdjacent.leftAdjacent shouldBe a

  /**
    * This stronger property is only true for discrete.
    */
  manyDimensionsDiscreteProperty("adjacency is ordered: A ~> B => A < B"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](domainGen: Gen[D]): Assertion =
        forAll(domainGen): a =>
          a.leftAdjacent < a || a.isUnbounded shouldBe true
          a.rightAdjacent > a || a.isUnbounded shouldBe true

  /**
    * This weaker property is true for discrete and continuous (but only evaluated for continuous because discrete has
    * the stronger "ordered" property).
    */
  manyDimensionsContinuousProperty("adjacency is unique: A ~> B => A != B"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](domainGen: Gen[D]): Assertion =
        forAll(domainGen): a =>
          a.leftAdjacent != a || a.isUnbounded shouldBe true
          a.rightAdjacent != a || a.isUnbounded shouldBe true

  property(s"2D Discrete   orthogonal translations are commutative"):
    import DiscreteValue.IntDiscreteValue
    forAll(DomainGenerator.genDim2): a =>
      val a0 = a.updateDimension(0, a(0).rightAdjacent) // do dimension 0 first
      val a01 = a0.updateDimension(1, a0(1).rightAdjacent) // ... then 1
      val a1 = a.updateDimension(1, a(1).rightAdjacent) // do dimension 1 first...
      val a10 = a1.updateDimension(0, a1(0).rightAdjacent) // ... then 0
      a01 shouldBe a10

  property(s"2D Continuous orthogonal translations are commutative"):
    import ContinuousValue.IntContinuousValue
    forAll(DomainGenerator.genDim2): a =>
      val a0 = a.updateDimension(0, a(0).rightAdjacent) // do dimension 0 first
      val a01 = a0.updateDimension(1, a0(1).rightAdjacent) // ... then 1
      val a1 = a.updateDimension(1, a(1).rightAdjacent) // do dimension 1 first...
      val a10 = a1.updateDimension(0, a1(0).rightAdjacent) // ... then 0
      a01 shouldBe a10
