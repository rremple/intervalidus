package intervalidus.laws

import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
import intervalidus.Interval.{complement, compress, isDisjoint, recompress, unbounded}
import IntervalGenerator.*
import intervalidus.{ContinuousValue, DiscreteValue, DomainLike, Interval}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions

class IntervalLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 200 /*, workers = 2*/ )

  /**
    * Property tests that are applied to individual intervals in 1, 2, 3, and 4 dimensions.
    */
  trait ManyDimensionsPropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](intervalGen: Gen[Interval[D]]): Assertion

  /**
    * Property tests that are applied to collections of intervals in 1, 2, 3, and 4 dimensions.
    */
  trait ManyDimensionsIterablePropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion

  /**
    * Evaluate an interval property in 1, 2, 3, and 4 dimensions using both discrete and continuous value semantics.
    */
  def manyDimensionsProperty(propertyName: String)(testFun: ManyDimensionsPropertyTest): Unit =
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

  /**
    * Evaluate an iterable interval property in 1, 2, 3, and 4 dimensions using both discrete and continuous value
    * semantics.
    */
  def manyDimensionsIterableProperty(propertyName: String)(testFun: ManyDimensionsIterablePropertyTest): Unit =
    {
      import DiscreteValue.IntDiscreteValue
      property(s"4D Discrete   $propertyName")(testFun[Dim4](genNonIntersectingDim4))
      property(s"3D Discrete   $propertyName")(testFun[Dim3](genNonIntersectingDim3))
      property(s"2D Discrete   $propertyName")(testFun[Dim2](genNonIntersectingDim2))
      property(s"1D Discrete   $propertyName")(testFun[Dim1](genNonIntersectingDim1))
    }
    {
      import ContinuousValue.IntContinuousValue
      property(s"4D Continuous $propertyName")(testFun[Dim4](genNonIntersectingDim4))
      property(s"3D Continuous $propertyName")(testFun[Dim3](genNonIntersectingDim3))
      property(s"2D Continuous $propertyName")(testFun[Dim2](genNonIntersectingDim2))
      property(s"1D Continuous $propertyName")(testFun[Dim1](genNonIntersectingDim1))
    }

  /*
   * --- The actual property-based tests ---
   */
  def assertMatch[D <: NonEmptyTuple: DomainLike](
    lhs: Iterable[Interval[D]],
    rhs: Iterable[Interval[D]]
  ): Assertion = recompress(lhs, rhs) shouldBe recompress(rhs, lhs)

  manyDimensionsProperty("intersection is commutative: A ∩ B ≡ B ∩ A"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalGen: Gen[Interval[D]]): Assertion =
        forAll(intervalGen, intervalGen): (a, b) =>
          (a ∩ b) shouldBe (b ∩ a)

  manyDimensionsProperty("intersection with self is idempotent: A ∩ A ≡ A"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalGen: Gen[Interval[D]]): Assertion =
        forAll(intervalGen): a =>
          (a ∩ a) shouldBe Some(a)

  manyDimensionsProperty("union of adjacent or intersecting intervals is continuous (no gap)"):
    new ManyDimensionsPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalGen: Gen[Interval[D]]): Assertion =
        forAll(intervalGen, intervalGen): (a, b) =>
          val gap = a gapWith b
          if (a isAdjacentTo b) || (a intersects b) then gap shouldBe None else succeed

  manyDimensionsIterableProperty("recompression is idempotent: recompress(A) ≡ recompress(recompress(A))"):
    new ManyDimensionsIterablePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion =
        forAll(intervalsGen): uncompressed =>
          val a = compress(uncompressed) // some compressed, but not optimally compressed configuration
          recompress(a) shouldBe recompress(recompress(a))

  manyDimensionsIterableProperty("disjoint intervals union their complement covers the domain: A ∪ A' ≡ ξ"):
    new ManyDimensionsIterablePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion =
        forAll(intervalsGen): a =>
          assert(isDisjoint(a))
          val gapIntervals = complement(a)
          assert(isDisjoint(gapIntervals))
          val totalIntervals = a ++ gapIntervals
          assert(isDisjoint(totalIntervals))
          recompress(totalIntervals) shouldBe Iterable(unbounded)

  manyDimensionsIterableProperty("the universal set minus A is the complement of A: ξ \\ A ≡ A'"):
    new ManyDimensionsIterablePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion =
        forAll(intervalsGen): a =>
          val universeMinusTestIntervals = a.foldLeft(Seq(unbounded)): (prior, i) =>
            prior.flatMap: p =>
              if p intersects i then p.separateUsing(i).filterNot(_ ⊆ i) else Seq(p)
          assertMatch(universeMinusTestIntervals, complement(a))

  manyDimensionsIterableProperty("involutive complement of a complement: A ≡ (A')'"):
    new ManyDimensionsIterablePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion =
        forAll(intervalsGen): a =>
          assertMatch(a, complement(complement(a)))

  /**
    * UniqueIntervals preserve coverage, are strictly disjoint, and respect all input boundaries. The shards that
    * 'touch' the original must sum to the original. With respect to every test interval, all shards should either be a
    * subset or be entirely disjoint.
    */
  manyDimensionsIterableProperty("uniqueIntervals are well-behaved"):
    new ManyDimensionsIterablePropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](intervalsGen: Gen[Iterable[Interval[D]]]): Assertion =
        forAll(intervalsGen): a =>
          val shattered = Interval.uniqueIntervals(a)
          isDisjoint(shattered) shouldBe true
          val allSubsetsOrDisjoints = shattered.forall: s =>
            a.forall: i =>
              s ⊆ i || !(i intersects s)
          allSubsetsOrDisjoints shouldBe true
          val intersectingShards = shattered.filter(s => a.exists(_ intersects s))
          assertMatch(intersectingShards, a)
