package intervalidus.laws

import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
import intervalidus.DomainLike.given
import intervalidus.IntervalShape.*
import IntervalShapeGenerator.*
import intervalidus.{ContinuousValue, DiscreteValue, DomainLike, IntervalShape}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions

class IntervalShapeLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 200 /*, workers = 2*/ )

  /**
    * Property tests that are applied to IntervalShape with intervals in 1, 2, 3, and 4 dimensions.
    */
  trait IntervalShapePropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion

  /**
    * Evaluate an IntervalShape property in 1, 2, 3, and 4 dimensions using both discrete and continuous interval domain
    * value semantics.
    */
  def intervalMultiProperty(propertyName: String)(testFun: IntervalShapePropertyTest): Unit =
    {
      import DiscreteValue.IntDiscreteValue
      property(s"4D Discrete   $propertyName")(testFun[Dim4](genDim4))
      property(s"3D Discrete   $propertyName")(testFun[Dim3](genDim3))
      property(s"2D Discrete   $propertyName")(testFun[Dim2](genDim2))
      property(s"1D Discrete   $propertyName")(testFun[Dim1](genDim1))
    }
    {
      import ContinuousValue.IntContinuousValue
      property(s"4D Continuous $propertyName")(testFun[Dim4](genDim4))
      property(s"3D Continuous $propertyName")(testFun[Dim3](genDim3))
      property(s"2D Continuous $propertyName")(testFun[Dim2](genDim2))
      property(s"1D Continuous $propertyName")(testFun[Dim1](genDim1))
    }

  /*
   * --- The actual property-based tests ---
   */

  extension [D <: NonEmptyTuple: DomainLike](lhs: IntervalShape[D])
    infix def ≡≡(rhs: IntervalShape[D]): Assertion = assert(lhs ≡ rhs)

  // Key Laws of Set Theory. https://en.wikipedia.org/wiki/List_of_set_identities_and_relations

  intervalMultiProperty("Associative Laws: The grouping of operations does not matter."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen, intervalMultiGen): (a, b, c) =>
          (a ∪ (b ∪ c)) ≡≡ ((a ∪ b) ∪ c)
          (a ∩ (b ∩ c)) ≡≡ ((a ∩ b) ∩ c)
          (a △ (b △ c)) ≡≡ ((a △ b) △ c)

  intervalMultiProperty("Distributive Laws: Determines how intersection distributes over union and vice versa."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen, intervalMultiGen): (a, b, c) =>
          (a ∪ (b ∩ c)) ≡≡ ((a ∪ b) ∩ (a ∪ c))
          (a ∩ (b ∪ c)) ≡≡ ((a ∩ b) ∪ (a ∩ c))
          (a ∩ (b △ c)) ≡≡ ((a ∩ b) △ (a ∩ c))

  intervalMultiProperty("Commutative Laws: The order of operations does not matter."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen): (a, b) =>
          (a ∪ b) ≡≡ (b ∪ a)
          (a ∩ b) ≡≡ (b ∩ a)
          (a △ b) ≡≡ (b △ a)

  intervalMultiProperty("De Morgan’s Laws: Relates the complement of unions and intersections."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen): (a, b) =>
          (a ∪ b).c ≡≡ (a.c ∩ b.c)
          (a ∩ b).c ≡≡ (a.c ∪ b.c)

  intervalMultiProperty("Absorption Laws: Rules for simplifying specific combinations."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen): (a, b) =>
          (a ∪ (a ∩ b)) ≡≡ a
          (a ∩ (a ∪ b)) ≡≡ a

  intervalMultiProperty("Idempotent Laws: A set combined with itself is the set itself."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen): a =>
          (a ∪ a) ≡≡ a
          (a ∩ a) ≡≡ a

  intervalMultiProperty("Nilpotent Laws: A set combined with itself is ∅."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen): a =>
          (a \ a) ≡≡ ∅ // difference self-inverse
          (a △ a) ≡≡ ∅ // symmetric difference self-inverse

  intervalMultiProperty("Identity Laws: Operations with ξ or ∅."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen): a =>
          (a ∪ ∅) ≡≡ a
          (a ∩ ξ) ≡≡ a
          (a △ ∅) ≡≡ a
          (a \ ∅) ≡≡ a

  intervalMultiProperty("Complement Laws: Operations with a set's complement."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen): a =>
          (a ∪ a.c) ≡≡ ξ
          (a ∩ a.c) ≡≡ ∅
          (a △ a.c) ≡≡ ξ
          a.c.c ≡≡ a // involution

  intervalMultiProperty("Domination Laws: Operations where ξ and ∅ dominate."):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen): a =>
          (a ∪ ξ) ≡≡ ξ
          (a ∩ ∅) ≡≡ ∅
          (∅ \ a) ≡≡ ∅

  intervalMultiProperty("Consistency Laws: Equivalent definitions based on set algebra, e.g., A ∩ B ≡ A \\ B'"):
    new IntervalShapePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalMultiGen: Gen[IntervalShape[D]]): Assertion =
        forAll(intervalMultiGen, intervalMultiGen): (a, b) =>
          a.c ≡≡ (ξ \ a) // Complement definition
          (b \ a.c) ≡≡ (a \ b.c) // Intersection definitions
          ((a \ b) ∪ (b \ a)) ≡≡ ((a ∪ b) \ (a ∩ b)) // Symmetric difference definitions
          (a ∪ b) ≡≡ ((a △ b) ∪ (a ∩ b)) // Inclusion-Exclusion identity

          (a ∩ b) ⊆ a shouldBe true
          (a ∩ b) ⊆ b shouldBe true
          a ⊆ (a ∪ b) shouldBe true
          b ⊆ (a ∪ b) shouldBe true
          (a \ b) ⊆ a shouldBe true
          (b \ a) ⊆ b shouldBe true
          a ⊆ ξ shouldBe true
          ∅ ⊆ a shouldBe true
