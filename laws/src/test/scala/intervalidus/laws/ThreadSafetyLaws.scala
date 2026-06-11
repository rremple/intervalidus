package intervalidus.laws

import DataGenerator.*
import DomainGenerator.{Dim1, Dim2, Dim3, Dim4}
import intervalidus.DomainLike.given
import intervalidus.*
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{Assertion, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.util.Random

class ThreadSafetyLaws extends AnyPropSpec with ScalaCheckPropertyChecks with ParallelTestExecution with Matchers:
  // given PropertyCheckConfiguration(minSuccessful = 2000 /*, workers = 2*/ )

  /**
    * Property tests that are applied to data with intervals in 1, 2, 3, and 4 dimensions.
    */
  trait ThreadSafetyPropertyTest:
    def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion

  def threadSafetyPropertyInt(propertyName: String, testFun: ThreadSafetyPropertyTest, paddedName: String)(using
    DomainValueLike[Int]
  ): Unit =
    given CoreConfig[Dim4] = CoreConfig.default.withCompressOnUpdate(false)
    property(s"4D $paddedName $propertyName")(testFun[Dim4](genDim4))
    given CoreConfig[Dim3] = CoreConfig.default.withCompressOnUpdate(false)
    property(s"3D $paddedName $propertyName")(testFun[Dim3](genDim3))
    given CoreConfig[Dim2] = CoreConfig.default.withCompressOnUpdate(false)
    property(s"2D $paddedName $propertyName")(testFun[Dim2](genDim2))
    given CoreConfig[Dim1] = CoreConfig.default.withCompressOnUpdate(false)
    property(s"1D $paddedName $propertyName")(testFun[Dim1](genDim1))

  def threadSafetyDiscreteProperty(propertyName: String)(testFun: ThreadSafetyPropertyTest): Unit =
    threadSafetyPropertyInt(propertyName, testFun, "Discrete  ")(using DiscreteValue.IntDiscreteValue)

  def threadSafetyContinuousProperty(propertyName: String)(testFun: ThreadSafetyPropertyTest): Unit =
    threadSafetyPropertyInt(propertyName, testFun, "Continuous")(using ContinuousValue.IntContinuousValue)

  /**
    * Evaluate a random-valued immutable data property in 1, 2, 3, and 4 dimensions using both discrete and continuous
    * interval domain value semantics.
    */
  def threadSafetyProperty(propertyName: String)(testFun: ThreadSafetyPropertyTest): Unit =
    threadSafetyDiscreteProperty(propertyName)(testFun)
    threadSafetyContinuousProperty(propertyName)(testFun)

    /*
     * --- The actual property-based tests ---
     */

  // proves shuffling withing categories (something concurrent does by its nature) works even when not concurrent
  threadSafetyProperty("baseline: sync diff actions apply correctly when shuffled"):
    new ThreadSafetyPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen, dataGen): (a, b) =>
          val r = Random()
          val result = a.toMutable
          val diffActionsUnordered = b.diffActionsFrom(a)

          val maxSize = diffActionsUnordered.size
          val diffActions = diffActionsUnordered
            .map: // add a random sequence that preserves category order
              case action @ DiffAction.Delete(_) => (action, r.nextInt(maxSize)) // deletes first
              case action @ DiffAction.Update(_) => (action, maxSize + r.nextInt(maxSize)) // then updates
              case action @ DiffAction.Create(_) => (action, 2 * maxSize + r.nextInt(maxSize)) // then creates last
            .toList
            .sortBy(_._2)
            .map(_._1)

          diffActions.foreach:
            case DiffAction.Delete(start)                      => result.removeByKey(start)
            case DiffAction.Update(data: ValidData[String, D]) => result.replaceByKey(data.interval.start, data)
            case DiffAction.Create(data: ValidData[String, D]) => result.set(data)

          assert(result ≡ b)

  threadSafetyProperty("concurrent: async diff actions apply correctly (thread-safe)"):
    new ThreadSafetyPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen, dataGen): (a, b) =>
          val result = a.toMutable
          val diffActionsUnordered = b.diffActionsFrom(a)

          val deleteKeys: Iterable[D] = diffActionsUnordered.collect:
            case DiffAction.Delete(start) => start
          val updateData: Iterable[ValidData[String, D]] = diffActionsUnordered.collect:
            case DiffAction.Update(data: ValidData[String, D]) => data
          val createData: Iterable[ValidData[String, D]] = diffActionsUnordered.collect:
            case DiffAction.Create(data: ValidData[String, D]) => data

          val allActions = for
            _ <- Future.sequence(deleteKeys.map(key => Future(result.removeByKey(key))))
            _ <- Future.sequence(updateData.map(data => Future(result.replaceByKey(data.interval.start, data))))
            _ <- Future.sequence(createData.map(data => Future(result.set(data))))
          yield ()

          Await.ready(allActions, Duration.Inf)
          assert(result ≡ b)

  threadSafetyDiscreteProperty("readers are safely isolated from writers: torn read check"):
    new ThreadSafetyPropertyTest:
      override def apply[D <: NonEmptyTuple: DomainLike](dataGen: Gen[immutable.Data[String, D]]): Assertion =
        forAll(dataGen): initialData =>
          val result = initialData.toMutable
          val size = result.size
          val r = Random()
          val stopHammer = AtomicBoolean(false)

          // The Hammer: constant background mutation
          val asyncHammer = Future:
            // var i = 0
            // println(s"starting hammer...")
            while !stopHammer.get() do
              val sampleIndex = r.nextInt(size)
              result.getAll
                .slice(sampleIndex, sampleIndex + 1)
                .foreach: randomData =>
                  val smallerInterval = randomData.interval
                  val biggerInterval = Interval(smallerInterval.start.leftAdjacent, smallerInterval.end.rightAdjacent)
                  result.set(biggerInterval -> randomData.value)
                  result.remove(smallerInterval) // punches hole in the bigger interval, fragmenting data around it
              // i += 1
              Thread.sleep(1) // allows the Photographer to do its thing
            // println(s"hammer stopped after $i iterations...")

          // The Photographer: constant foreground consistency check
          // We run many snapshots. Every single one must be self-consistent.
          for _ <- 1 to 100 do
            Thread.sleep(1) // allows the Hammer to do its thing
            assert(result.isEquivalentTo(result))
          stopHammer.set(true)
          Await.ready(asyncHammer, Duration.Inf)
          succeed
