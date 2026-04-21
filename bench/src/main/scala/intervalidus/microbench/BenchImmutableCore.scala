package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.{Dim1, Dim2, Dim3, Dim4, Dim5}
import intervalidus.microbench.IntervalGenerator.*
import intervalidus.microbench.IntervalGenerator.IntDomainTuple.given
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized
import scala.language.implicitConversions

// Similar to BenchTreeReorg, but providing one size hint/depth/capacity, and using both mutable. and immutable.Data.
object BenchImmutableCore:
  private val constantSeed: Long = 42 // Random().nextLong() //
  val targetFragments = 4096

  // --- --- --- --- --- --- --- Abstract State --- --- --- --- --- --- ---  ---

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkState[D <: NonEmptyTuple: DomainLike](using IntDomainTuple[D], DomainValueLike[Int]):
    val d: Int = arity[D]
    val edgePoints = math.exp(math.log(targetFragments) / d).round.toInt
    val expectedFragments = math.pow(edgePoints + 1, d).toInt

    // intervalsFor[d](c, k) = c*2^(d*k)
    println(s"\nInitializing ${d}d state, seed=$constantSeed, intervals/iteration ~ $expectedFragments ")

    given CoreConfig[D] = CoreConfig.default[D].withCapacityHint(fullDomain[D])

    // set per iteration
    private var testDataIterator: Iterator[Array[Interval[D]]] = uninitialized

    // set per invocation
    var fragments: IterableOnce[ValidData[Int, D]] = uninitialized
    var dataImmutable: immutable.Data[Int, D] = uninitialized
    var dataMutable: mutable.Data[Int, D] = uninitialized

    @Setup(Level.Iteration)
    def setUpIteration(): Unit =
      println(s"Set up iteration...")
      // start again with the same seed which will generate the same random test data
      given RandomNumbers = RandomNumbers.withSeed(constantSeed)
      testDataIterator = genFromOrigin[D](edgePoints).iterator
      println(s"Iteration set up.")

    @Setup(Level.Invocation)
    def setUpInvocation(): Unit =
      // println(s"Set up invocation...")
      fragments = testDataIterator.next().iterator.zipWithIndex.map((i, v) => i -> v)
      dataImmutable = immutable.Data.empty
      dataMutable = mutable.Data.empty

  // --- --- --- --- --- --- --- Concrete State --- --- --- --- --- --- ---  ---

  // TODO: someday I can run discrete and continuous together, but not today...
  import DiscreteValue.IntDiscreteValue
  // import ContinuousValue.IntContinuousValue
  class BenchmarkState1d extends GenericBenchmarkState[Dim1]
  class BenchmarkState2d extends GenericBenchmarkState[Dim2]
  class BenchmarkState3d extends GenericBenchmarkState[Dim3]
  class BenchmarkState4d extends GenericBenchmarkState[Dim4]
  // class BenchmarkState5d extends GenericBenchmarkState[Dim5]

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
    def setImmutableF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
      b.consume:
        s.fragments.iterator.foldLeft(s.dataImmutable)(_.set(_))

    def setMutableF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
      b.consume:
        s.fragments.iterator.foreach(s.dataMutable.set(_))

  // --- --- --- --- --- --- --- Concrete Bench --- --- --- --- --- --- ---  ---

  class BenchmarkLeaf1d extends GenericBench:
    private type S = BenchmarkState1d
    @Benchmark def setImmutable(b: Blackhole, s: S): Unit = setImmutableF(b, s)
    @Benchmark def setMutable(b: Blackhole, s: S): Unit = setMutableF(b, s)
  class BenchmarkLeaf2d extends GenericBench:
    private type S = BenchmarkState2d
    @Benchmark def setImmutable(b: Blackhole, s: S): Unit = setImmutableF(b, s)
    @Benchmark def setMutable(b: Blackhole, s: S): Unit = setMutableF(b, s)
  class BenchmarkLeaf3d extends GenericBench:
    private type S = BenchmarkState3d
    @Benchmark def setImmutable(b: Blackhole, s: S): Unit = setImmutableF(b, s)
    @Benchmark def setMutable(b: Blackhole, s: S): Unit = setMutableF(b, s)
  class BenchmarkLeaf4d extends GenericBench:
    private type S = BenchmarkState4d
    @Benchmark def setImmutable(b: Blackhole, s: S): Unit = setImmutableF(b, s)
    @Benchmark def setMutable(b: Blackhole, s: S): Unit = setMutableF(b, s)
//  class BenchmarkLeaf5d extends GenericBench:
//    private type S = BenchmarkState5d
//    @Benchmark def setImmutable(b: Blackhole, s: S): Unit = setImmutableF(b, s)
//    @Benchmark def setMutable(b: Blackhole, s: S): Unit = setMutableF(b, s)
