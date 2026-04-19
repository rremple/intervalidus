package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.IntervalShapeGenerator.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions

object BenchLeafCapacity:
  private val constantSeed: Long = 42 // Random().nextLong() //

  // --- --- --- --- --- --- --- Abstract State --- --- --- --- --- --- ---  ---

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkState[D <: NonEmptyTuple](
    leafCapacity: Int,
    testDataGen: () => RandomNumbers ?=> Gen[IntervalShape[D]]
  )(using domainLike: DomainLike[D]):
    println(s"\nInitializing ${domainLike.arity}d state, seed=$constantSeed...\n")
    val r = scala.util.Random()
    System.setProperty("INTERVALIDUS_TREE_NODE_CAPACITY", leafCapacity.toString)
    import scala.compiletime.uninitialized
    private var testDataIterator: Iterator[IntervalShape[D]] = uninitialized
    var aShape: IntervalShape[D] = uninitialized
    var bShape: IntervalShape[D] = uninitialized
    var aMonoid: immutable.DataMonoid[Unit, D] = uninitialized
    var bMonoid: immutable.DataMonoid[Unit, D] = uninitialized
    var fragments: IntervalShape[D] = uninitialized

    @Setup(Level.Iteration)
    def setUpIteration(): Unit =
      // println(s"setupIteration...")
      // start again with the same seed which will generate the same random test data
      given RandomNumbers = RandomNumbers.withSeed(constantSeed)
      testDataIterator = testDataGen().iterator
      // println(s"setupIteration.")

    @Setup(Level.Invocation)
    def setUpInvocation(): Unit =
      // println(s"setUpInvocation...")
      aShape = testDataIterator.next()
      bShape = testDataIterator.next()
      aMonoid = aShape.withMonoidValue(())
      bMonoid = bShape.withMonoidValue(())
      fragments = IntervalShape.withoutChecks(
        Interval.uniqueIntervals(aMonoid.allIntervals ++ bMonoid.allIntervals)
      )
      // println(s"setUpInvocation, fragments = ${fragments.size}.")

  // --- --- --- --- --- --- --- Concrete State --- --- --- --- --- --- ---  ---

  // TODO: someday I can run discrete and continuous together, but not today...
  import DiscreteValue.IntDiscreteValue
  // import ContinuousValue.IntContinuousValue
  class BenchmarkState1d00256 extends GenericBenchmarkState(256, () => genDim1)
  class BenchmarkState1d00512 extends GenericBenchmarkState(512, () => genDim1)
  class BenchmarkState1d01024 extends GenericBenchmarkState(1024, () => genDim1)
  class BenchmarkState1d02048 extends GenericBenchmarkState(2048, () => genDim1)
  class BenchmarkState1d04096 extends GenericBenchmarkState(4096, () => genDim1)
  class BenchmarkState2d00256 extends GenericBenchmarkState(256, () => genDim2)
  class BenchmarkState2d00512 extends GenericBenchmarkState(512, () => genDim2)
  class BenchmarkState2d01024 extends GenericBenchmarkState(1024, () => genDim2)
  class BenchmarkState2d02048 extends GenericBenchmarkState(2048, () => genDim2)
  class BenchmarkState2d04096 extends GenericBenchmarkState(4096, () => genDim2)
  class BenchmarkState3d00256 extends GenericBenchmarkState(256, () => genDim3)
  class BenchmarkState3d00512 extends GenericBenchmarkState(512, () => genDim3)
  class BenchmarkState3d01024 extends GenericBenchmarkState(1024, () => genDim3)
  class BenchmarkState3d02048 extends GenericBenchmarkState(2048, () => genDim3)
  class BenchmarkState3d04096 extends GenericBenchmarkState(4096, () => genDim3)
  class BenchmarkState4d00256 extends GenericBenchmarkState(256, () => genDim4)
  class BenchmarkState4d00512 extends GenericBenchmarkState(512, () => genDim4)
  class BenchmarkState4d01024 extends GenericBenchmarkState(1024, () => genDim4)
  class BenchmarkState4d02048 extends GenericBenchmarkState(2048, () => genDim4)
  class BenchmarkState4d04096 extends GenericBenchmarkState(4096, () => genDim4)
  class BenchmarkState5d00256 extends GenericBenchmarkState(256, () => genDim5)
  class BenchmarkState5d00512 extends GenericBenchmarkState(512, () => genDim5)
  class BenchmarkState5d01024 extends GenericBenchmarkState(1024, () => genDim5)
  class BenchmarkState5d02048 extends GenericBenchmarkState(2048, () => genDim5)
  class BenchmarkState5d04096 extends GenericBenchmarkState(4096, () => genDim5)

  // --- --- --- --- --- --- --- Abstract Bench --- --- --- --- --- --- ---  ---

  @Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  // @Measurement(iterations = 5, time = 25, timeUnit = TimeUnit.SECONDS)
  // @BenchmarkMode(Array(Mode.Throughput))
  // @OutputTimeUnit(TimeUnit.SECONDS)
  @State(Scope.Benchmark)
  @Fork(
    jvmArgsAppend = Array("-Xmx3g", "-XX:+HeapDumpOnOutOfMemoryError"),
    value = 1
  )
  abstract class GenericBench:
    def unionMonoidF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
      b.consume:
        s.aMonoid ∪ s.bMonoid

    def lookupMonoidF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
      s.fragments.allIntervals.foreach: i =>
        b.consume:
          s.fragments.intersection(i)

  // --- --- --- --- --- --- --- Concrete Bench --- --- --- --- --- --- ---  ---

  class BenchmarkLeaf1d00256 extends GenericBench:
    private type S = BenchmarkState1d00256
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf1d00512 extends GenericBench:
    private type S = BenchmarkState1d00512
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf1d01024 extends GenericBench:
    private type S = BenchmarkState1d01024
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf1d02048 extends GenericBench:
    private type S = BenchmarkState1d02048
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf1d04096 extends GenericBench:
    private type S = BenchmarkState1d04096
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)

  class BenchmarkLeaf2d00256 extends GenericBench:
    private type S = BenchmarkState2d00256
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf2d00512 extends GenericBench:
    private type S = BenchmarkState2d00512
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf2d01024 extends GenericBench:
    private type S = BenchmarkState2d01024
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf2d02048 extends GenericBench:
    private type S = BenchmarkState2d02048
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf2d04096 extends GenericBench:
    private type S = BenchmarkState2d04096
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)

  class BenchmarkLeaf3d00256 extends GenericBench:
    private type S = BenchmarkState3d00256
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf3d00512 extends GenericBench:
    private type S = BenchmarkState3d00512
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf3d01024 extends GenericBench:
    private type S = BenchmarkState3d01024
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf3d02048 extends GenericBench:
    private type S = BenchmarkState3d02048
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf3d04096 extends GenericBench:
    private type S = BenchmarkState3d04096
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)

  class BenchmarkLeaf4d00256 extends GenericBench:
    private type S = BenchmarkState4d00256
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf4d00512 extends GenericBench:
    private type S = BenchmarkState4d00512
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf4d01024 extends GenericBench:
    private type S = BenchmarkState4d01024
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf4d02048 extends GenericBench:
    private type S = BenchmarkState4d02048
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf4d04096 extends GenericBench:
    private type S = BenchmarkState4d04096
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)

  class BenchmarkLeaf5d00256 extends GenericBench:
    private type S = BenchmarkState5d00256
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf5d00512 extends GenericBench:
    private type S = BenchmarkState5d00512
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf5d01024 extends GenericBench:
    private type S = BenchmarkState5d01024
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf5d02048 extends GenericBench:
    private type S = BenchmarkState5d02048
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
  class BenchmarkLeaf5d04096 extends GenericBench:
    private type S = BenchmarkState5d04096
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = unionMonoidF(b, s)
    @Benchmark def lookupMonoid(b: Blackhole, s: S): Unit = lookupMonoidF(b, s)
