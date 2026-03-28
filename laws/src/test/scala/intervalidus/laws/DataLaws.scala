package intervalidus.laws

import DataGenerator.*
import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
import intervalidus.DomainLike.given
import intervalidus.immutable.Data
import intervalidus.*
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.implicitConversions

class DataLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 200 /*, workers = 2*/ )

  /**
    * Property tests that are applied to data with intervals in 1, 2, 3, and 4 dimensions.
    */
  trait DataPropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion

  /**
    * Evaluate a random-valued immutable data property in 1, 2, 3, and 4 dimensions using both discrete and continuous
    * interval domain value semantics.
    */
  def dataProperty(propertyName: String)(testFun: DataPropertyTest): Unit =
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

  dataProperty("fill isolation: fills only add data where A is undefined"):
    new DataPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen, dataGen): (aRandom, bRandom) =>
          // change a and b to have a different fixed vals
          val a = aRandom.mapValues(_ => "a")
          val b = bRandom.mapValues(_ => "b")

          // For every interval in B, add it to A ONLY if A has no value there.
          val filled = b.getAll.foldLeft(a)(_.fill(_))

          // Invariant: Any value that was in A originally MUST still be there (unmodified)
          val stillThere = a.allIntervals.forall: i =>
            val aData = a.getIntersecting(i)
            val filledData = filled.getIntersecting(i)
            aData.size == 1 && filledData.size == 1 && aData == filledData

          stillThere shouldBe true

          // Invariant: Any new value in 'filled' must have come from B
          val newData = filled.filter(d => !(a intersects d.interval))
          (newData.values.isEmpty || newData.values.toList == List("b")) shouldBe true

          // Invariant: All data in 'filled' that intersect a should match a exactly
          filled.filter(d => a intersects d.interval) shouldBe a

          // Invariant: If it's a new piece of data, it is not in A and B had the value
          val newValuesFromB = newData.allIntervals.forall: i =>
            val aData = a.getIntersecting(i)
            val bData = b.getIntersecting(i)
            val bDataValues = bData.map(_.value)
            val filledData = filled.getIntersecting(i)
            val filledDataValues = filledData.map(_.value)

            aData.isEmpty && bData.nonEmpty && filledData.nonEmpty &&
            (bDataValues ++ filledDataValues).forall(_ == "b")

          newValuesFromB shouldBe true

  dataProperty("update isolation: updates only modify intersections"):
    new DataPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen, dataGen): (aRandom, bRandom) =>
          val a = aRandom.mapValues(_ => "a")
          val newVal = "b"
          val b = bRandom.mapValues(_ => newVal)
          // Perform an update where the function just sets the new value
          val updated = b.getAll.foldLeft(a)(_.update(_))

          // Invariant: updated must contain new values where A and B are both defined (i.e., they overlap)
          val updatesIntersectBoth = updated
            .intervals(newVal)
            .forall: i =>
              (a intersects i) && (b intersects i)
          updatesIntersectBoth shouldBe true

          // Invariant: updated must contain old values only where A is defined and B isn't (i.e., in A \ B)
          val nonUpdatesIntersectOnlyA = updated
            .filter(_.value != newVal)
            .allIntervals
            .forall: i =>
              (a intersects i) && !(b intersects i)
          nonUpdatesIntersectOnlyA shouldBe true

  dataProperty("unique interval atomicity: shards have exactly one or zero values"):
    new DataPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen): a =>
          val shattered = Interval.uniqueIntervals(a.allIntervals)
          val allUnique = shattered.forall: s =>
            a.getIntersecting(s).size <= 1
          allUnique shouldBe true

  dataProperty("fill idempotency: any A filled with itself is still A"):
    new DataPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen): a =>
          a.getAll.foldLeft(a)(_.fill (_)) shouldBe a.recompressAll()

  /**
    * This validates that the results of querying the core dataInSearchTree structure (i.e., the underlying box search
    * tree) using getIntersecting are consistent with the results of querying the core dataByStartAsc structure (i.e.,
    * the underlying tree map) using filter.
    */
  dataProperty("lookup consistency: intersection lookups are consistent with filtering"):
    new DataPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen, dataGen): (a, b) =>
          val aData = a.getAll.toList
          val result = b.allIntervals.forall: i =>
            val lhs = aData.filter(_.interval intersects i)
            val rhs = a.getIntersecting(i).toList.sorted
            lhs == rhs
          result shouldBe true
