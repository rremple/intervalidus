package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.{Dim1, Dim2, Dim3, Dim4, Dim5}
import intervalidus.microbench.IntervalGenerator.*
import intervalidus.microbench.IntervalGenerator.IntDomainTuple.given
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions

object BenchTreeReorg:
  private val constantSeed: Long = 42 // Random().nextLong() //

  // should fill depth 3 with 256 capacity, less at 512 and 1024
  private val sampleDepth = 3
  private val sampleLeafCapacity = 256

  // --- --- --- --- --- --- --- Abstract State --- --- --- --- --- --- ---  ---

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkState[D <: NonEmptyTuple: DomainLike](
    leafCapacity: Int
  )(using IntDomainTuple[D], DomainValueLike[Int]):
    val d: Int = arity[D]

    // intervalsFor[d](c, k) = c*2^(d*k)
    println(
      s"\nInitializing ${d}d state, leafCapacity=$leafCapacity, seed=$constantSeed" +
        s", intervals/iteration ~ ${intervalsFor[D](sampleLeafCapacity, sampleDepth)} "
    )

    val capacityHint: Interval[D] = fullDomain[D]
    System.setProperty("INTERVALIDUS_TREE_NODE_CAPACITY", leafCapacity.toString)

    import scala.compiletime.uninitialized
    private var testDataIterator: Iterator[Array[Interval[D]]] = uninitialized

    var fragments: Iterator[ValidData[Int, D]] = uninitialized
    var dataNoHint: mutable.Data[Int, D] = uninitialized
    var dataWithHint: mutable.Data[Int, D] = uninitialized

    @Setup(Level.Iteration)
    def setUpIteration(): Unit =
      println(s"Set up iteration...")
      // start again with the same seed which will generate the same random test data
      given RandomNumbers = RandomNumbers.withSeed(constantSeed)
      testDataIterator = genFromOrigin[D](sampleLeafCapacity, sampleDepth).iterator
      println(s"Iteration set up.")

    @Setup(Level.Invocation)
    def setUpInvocation(): Unit =
      // println(s"Set up invocation...")
      val testData = testDataIterator.next()
      fragments = testData.iterator.zipWithIndex.map((i, v) => i -> v)
      dataNoHint = mutable.Data.empty
      dataWithHint = {
        given CoreConfig[D] = CoreConfig.default[D].withCapacityHint(capacityHint)
        mutable.Data.empty
      }

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkStressState(
    depth: Int // 0 to 7 => 256 to 4194304 intervals
  )(using DomainValueLike[Int]):
    type D = Dim2
    val d: Int = arity[D]
    val leafCapacity = 256
    val edgePoints = edgePointsFor[Dim2](leafCapacity, depth)

    println(
      s"\nInitializing ${d}d state, leafCapacity=$leafCapacity, seed=$constantSeed" +
        s", intervals/iteration ~ ${intervalsFor[D](leafCapacity, depth)} "
    )

    val capacityHint: Interval[D] = fullDomain[D]
    System.setProperty("INTERVALIDUS_TREE_NODE_CAPACITY", leafCapacity.toString)

    import scala.compiletime.uninitialized

    private var testDataIterator: Iterator[Array[Interval[D]]] = uninitialized

    var fragments: Iterator[ValidData[Int, D]] = uninitialized
    var dataNoHint: mutable.Data[Int, D] = uninitialized
    var dataWithHint: mutable.Data[Int, D] = uninitialized

    @Setup(Level.Iteration)
    def setUpIteration(): Unit =
      println(s"Set up iteration with edgePoints=$edgePoints...")

      // start again with the same seed which will generate the same random test data
      given RandomNumbers = RandomNumbers.withSeed(constantSeed)

      testDataIterator = genFromOrigin[D](edgePoints).iterator
      println(s"Iteration set up.")

    @Setup(Level.Invocation)
    def setUpInvocation(): Unit =
      println(s"Set up invocation...")
      val testData = testDataIterator.next()
      fragments = testData.iterator.zipWithIndex.map((i, v) => i -> v)
      dataNoHint = mutable.Data.empty
      dataWithHint = {
        given CoreConfig[D] = CoreConfig.default[D].withCapacityHint(capacityHint)
        mutable.Data.empty
      }
      println(s"Invocation set up.")

  // --- --- --- --- --- --- --- Concrete State --- --- --- --- --- --- ---  ---

  // TODO: someday I can run discrete and continuous together, but not today...
  import DiscreteValue.IntDiscreteValue
  // import ContinuousValue.IntContinuousValue
//  class BenchmarkState1d00256 extends GenericBenchmarkState[Dim1](256)
//  class BenchmarkState1d00512 extends GenericBenchmarkState[Dim1](512)
//  class BenchmarkState1d01024 extends GenericBenchmarkState[Dim1](1024)
//  class BenchmarkState2d00256 extends GenericBenchmarkState[Dim2](256)
//  class BenchmarkState2d00512 extends GenericBenchmarkState[Dim2](512)
//  class BenchmarkState2d01024 extends GenericBenchmarkState[Dim2](1024)
//  class BenchmarkState3d00256 extends GenericBenchmarkState[Dim3](256)
//  class BenchmarkState3d00512 extends GenericBenchmarkState[Dim3](512)
//  class BenchmarkState3d01024 extends GenericBenchmarkState[Dim3](1024)
//  class BenchmarkState4d00256 extends GenericBenchmarkState[Dim4](256)
//  class BenchmarkState4d00512 extends GenericBenchmarkState[Dim4](512)
//  class BenchmarkState4d01024 extends GenericBenchmarkState[Dim4](1024)
////  class BenchmarkState5d00256 extends GenericBenchmarkState[Dim5](256)
////  class BenchmarkState5d00512 extends GenericBenchmarkState[Dim5](512)
////  class BenchmarkState5d01024 extends GenericBenchmarkState[Dim5](1024)

//  class BenchmarkStressState0 extends GenericBenchmarkStressState(0)
//  class BenchmarkStressState1 extends GenericBenchmarkStressState(1)
//  class BenchmarkStressState2 extends GenericBenchmarkStressState(2)
//  class BenchmarkStressState3 extends GenericBenchmarkStressState(3)
//  class BenchmarkStressState4 extends GenericBenchmarkStressState(4)
//  class BenchmarkStressState5 extends GenericBenchmarkStressState(5)
  class BenchmarkStressState6 extends GenericBenchmarkStressState(6)
  class BenchmarkStressState7 extends GenericBenchmarkStressState(7)

  // --- --- --- --- --- --- --- Abstract Bench --- --- --- --- --- --- ---  ---

  @Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 15, timeUnit = TimeUnit.SECONDS)
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @State(Scope.Benchmark)
  @Fork(
    jvmArgsAppend = Array("-Xmx4g", "-XX:+HeapDumpOnOutOfMemoryError"),
    value = 1
  )
  abstract class GenericBench:
    def setF[D <: NonEmptyTuple: DomainLike](
      b: Blackhole,
      fragments: Iterator[ValidData[Int, D]],
      data: mutable.Data[Int, D]
//    ): Unit = b.consume:
//        data.setMany(fragments) // if setMany used IterableOnce (which it should)
    ): Unit = fragments.foreach: d =>
      b.consume:
        data.set(d)

  // --- --- --- --- --- --- --- Concrete Bench --- --- --- --- --- --- ---  ---

//  class BenchmarkLeafStress0 extends GenericBench:
//    private type S = BenchmarkStressState0
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeafStress1 extends GenericBench:
//    private type S = BenchmarkStressState1
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeafStress2 extends GenericBench:
//    private type S = BenchmarkStressState2
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeafStress3 extends GenericBench:
//    private type S = BenchmarkStressState3
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeafStress4 extends GenericBench:
//    private type S = BenchmarkStressState4
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeafStress5 extends GenericBench:
//    private type S = BenchmarkStressState5
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
  class BenchmarkLeafStress6 extends GenericBench:
    private type S = BenchmarkStressState6
    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
  class BenchmarkLeafStress7 extends GenericBench:
    private type S = BenchmarkStressState7
    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)

//  class BenchmarkLeaf1d00256 extends GenericBench:
//    private type S = BenchmarkState1d00256
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf1d00512 extends GenericBench:
//    private type S = BenchmarkState1d00512
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf1d01024 extends GenericBench:
//    private type S = BenchmarkState1d01024
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//
//  class BenchmarkLeaf2d00256 extends GenericBench:
//    private type S = BenchmarkState2d00256
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf2d00512 extends GenericBench:
//    private type S = BenchmarkState2d00512
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf2d01024 extends GenericBench:
//    private type S = BenchmarkState2d01024
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//
//  class BenchmarkLeaf3d00256 extends GenericBench:
//    private type S = BenchmarkState3d00256
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf3d00512 extends GenericBench:
//    private type S = BenchmarkState3d00512
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf3d01024 extends GenericBench:
//    private type S = BenchmarkState3d01024
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//
//  class BenchmarkLeaf4d00256 extends GenericBench:
//    private type S = BenchmarkState4d00256
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf4d00512 extends GenericBench:
//    private type S = BenchmarkState4d00512
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//  class BenchmarkLeaf4d01024 extends GenericBench:
//    private type S = BenchmarkState4d01024
//    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
//    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
//
////  class BenchmarkLeaf5d00256 extends GenericBench:
////    private type S = BenchmarkState5d00256
////    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
////    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
////  class BenchmarkLeaf5d00512 extends GenericBench:
////    private type S = BenchmarkState5d00512
////    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
////    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
////  class BenchmarkLeaf5d01024 extends GenericBench:
////    private type S = BenchmarkState5d01024
////    @Benchmark def setNoHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataNoHint)
////    @Benchmark def setWithHint(b: Blackhole, s: S): Unit = setF(b, s.fragments, s.dataWithHint)
