package intervalidus.microbench

import intervalidus.DiscreteValue.given
import intervalidus.*
import intervalidus.Interval.Patterns.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.language.implicitConversions

/**
  * Benchmarks only the remove method to see what affect noBruteForceUpdate has on updateOrRemove performance.
  *
  * The earlier version of this benchmark was constructed similar to BenchSearchTree, but this was refactored to use
  * State instead. This was because, particularly in tests on mutable structures, as data were removed, the structure
  * would get depleted, and later operations would become (unrealistically) faster than earlier operations as the
  * structures got smaller and the miss rates increased. Also because the intervals to be removed were created randomly
  * along the way and not established up front, the differences between iterations were more random, leading to higher
  * error rates (which is still true in BenchSearchTree). Although using State makes the benchmark a bit more complex,
  * the results are more realistic with smaller errors.
  */
object BenchBruteForce extends BenchBase(baselineFeature = None, featuredFeature = None):
  // Sample results, where "baseline" is the new brute force algorithm and "featured" is the legacy logic. This shows
  // that the new brute force algorithm has about the same throughput on average as legacy for 2d and about 30% higher
  // for 3d, which is great (though the error rates are still pretty high relative to the scores)!
  //
  // Benchmark                                               Mode  Cnt      Score       Error  Units
  // BenchBruteForce.Immutable2dBench100x1k.baselineRemove  thrpt    3   3095.083 ±   441.526  ops/s
  // BenchBruteForce.Immutable2dBench100x1k.featuredRemove  thrpt    3   2821.159 ±   596.099  ops/s
  // BenchBruteForce.Immutable2dBench10x10k.baselineRemove  thrpt    3    245.656 ±   234.323  ops/s
  // BenchBruteForce.Immutable2dBench10x10k.featuredRemove  thrpt    3    235.570 ±    51.791  ops/s
  // BenchBruteForce.Immutable2dBench10x1k.baselineRemove   thrpt    3   3727.431 ±  1794.557  ops/s
  // BenchBruteForce.Immutable2dBench10x1k.featuredRemove   thrpt    3   3545.460 ±  2634.781  ops/s
  // BenchBruteForce.Immutable2dBench1x10k.baselineRemove   thrpt    3    225.502 ±   244.597  ops/s
  // BenchBruteForce.Immutable2dBench1x10k.featuredRemove   thrpt    3    237.361 ±   133.653  ops/s
  // BenchBruteForce.Immutable2dBench1x1k.baselineRemove    thrpt    3   3248.537 ±  8151.912  ops/s
  // BenchBruteForce.Immutable2dBench1x1k.featuredRemove    thrpt    3   3820.453 ±  2486.453  ops/s
  // BenchBruteForce.Immutable3dBench100x1k.baselineRemove  thrpt    3   4031.008 ±   815.005  ops/s
  // BenchBruteForce.Immutable3dBench100x1k.featuredRemove  thrpt    3   3801.927 ±  5715.230  ops/s
  // BenchBruteForce.Immutable3dBench10x10k.baselineRemove  thrpt    3    167.186 ±   401.543  ops/s
  // BenchBruteForce.Immutable3dBench10x10k.featuredRemove  thrpt    3    159.265 ±    37.928  ops/s
  // BenchBruteForce.Immutable3dBench10x1k.baselineRemove   thrpt    3   3292.757 ±  8985.938  ops/s
  // BenchBruteForce.Immutable3dBench10x1k.featuredRemove   thrpt    3   3063.075 ±  8911.468  ops/s
  // BenchBruteForce.Immutable3dBench1x10k.baselineRemove   thrpt    3    136.896 ±  1183.792  ops/s
  // BenchBruteForce.Immutable3dBench1x10k.featuredRemove   thrpt    3    195.974 ±   347.604  ops/s
  // BenchBruteForce.Immutable3dBench1x1k.baselineRemove    thrpt    3   3898.269 ±  4811.437  ops/s
  // BenchBruteForce.Immutable3dBench1x1k.featuredRemove    thrpt    3   3773.145 ±  1344.074  ops/s
  // BenchBruteForce.Mutable2dBench100x1k.baselineRemove    thrpt    3  26556.278 ±  1592.243  ops/s
  // BenchBruteForce.Mutable2dBench100x1k.featuredRemove    thrpt    3  27463.580 ± 16814.197  ops/s
  // BenchBruteForce.Mutable2dBench10x10k.baselineRemove    thrpt    3  24414.444 ±  3997.465  ops/s
  // BenchBruteForce.Mutable2dBench10x10k.featuredRemove    thrpt    3  25385.657 ±  3838.907  ops/s
  // BenchBruteForce.Mutable2dBench10x1k.baselineRemove     thrpt    3  34221.037 ±  1547.105  ops/s
  // BenchBruteForce.Mutable2dBench10x1k.featuredRemove     thrpt    3  33760.539 ±  4864.149  ops/s
  // BenchBruteForce.Mutable2dBench1x10k.baselineRemove     thrpt    3  24051.553 ± 23012.724  ops/s
  // BenchBruteForce.Mutable2dBench1x10k.featuredRemove     thrpt    3  23926.517 ± 29100.351  ops/s
  // BenchBruteForce.Mutable2dBench1x1k.baselineRemove      thrpt    3  38223.987 ± 13898.599  ops/s
  // BenchBruteForce.Mutable2dBench1x1k.featuredRemove      thrpt    3  36679.529 ± 12825.836  ops/s
  // BenchBruteForce.Mutable3dBench100x1k.baselineRemove    thrpt    3  45686.135 ± 19168.964  ops/s
  // BenchBruteForce.Mutable3dBench100x1k.featuredRemove    thrpt    3  32260.005 ±  2994.092  ops/s
  // BenchBruteForce.Mutable3dBench10x10k.baselineRemove    thrpt    3  14751.838 ± 11272.689  ops/s
  // BenchBruteForce.Mutable3dBench10x10k.featuredRemove    thrpt    3  10589.908 ±  7093.991  ops/s
  // BenchBruteForce.Mutable3dBench10x1k.baselineRemove     thrpt    3  47007.348 ±  7869.783  ops/s
  // BenchBruteForce.Mutable3dBench10x1k.featuredRemove     thrpt    3  34541.463 ± 21038.057  ops/s
  // BenchBruteForce.Mutable3dBench1x10k.baselineRemove     thrpt    3  15467.590 ±  4635.707  ops/s
  // BenchBruteForce.Mutable3dBench1x10k.featuredRemove     thrpt    3   9567.279 ± 21137.488  ops/s
  // BenchBruteForce.Mutable3dBench1x1k.baselineRemove      thrpt    3  40664.705 ±  2540.047  ops/s
  // BenchBruteForce.Mutable3dBench1x1k.featuredRemove      thrpt    3  33470.399 ± 41855.520  ops/s

  // Returns an interval overlapping an existing interval in some random way
  def randSubinterval(existing: Interval1D[Int]): Interval1D[Int] =
    existing match
      case Interval1D(Domain1D.Point(start), Domain1D.Point(end)) =>
        val size = end - start + 1
        // offset 0 half the time to not oversample "splits"
        val startOffset = if rand.nextBoolean() then 0 else rand.nextInt(size)
        val endOffset = if rand.nextBoolean() then 0 else rand.nextInt(size - startOffset)
        Interval1D.interval(start + startOffset, end + endOffset)
      case whoops => throw new Exception(s"why not a point? $whoops")

//  private def removeIntervals1D(dataIntervals: Vector[Interval.In1D[Int]]): Vector[Interval.In1D[Int]] =
//    dataIntervals.map: i =>
//      Interval.in1D(randSubinterval(i.headInterval1D))

  private def removeIntervals2D(
    dataIntervals: Vector[Interval.In2D[Int, Int]]
  ): Vector[Interval.In2D[Int, Int]] = dataIntervals.collect:
    case horizontal :+|: vertical =>
      randSubinterval(horizontal) x randSubinterval(vertical)

  private def removeIntervals3D(
    dataIntervals: Vector[Interval.In3D[Int, Int, Int]]
  ): Vector[Interval.In3D[Int, Int, Int]] = dataIntervals.collect:
    case horizontal :+: vertical :+|: depth =>
      randSubinterval(horizontal) x randSubinterval(vertical) x randSubinterval(depth)

  lazy val testIntervalsIn2d100x1k = removeIntervals2D(baselineMutable2d100x1k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn2d10x10k = removeIntervals2D(baselineMutable2d10x10k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn2d10x1k = removeIntervals2D(baselineMutable2d10x1k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn2d1x10k = removeIntervals2D(baselineMutable2d1x10k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn2d1x1k = removeIntervals2D(baselineMutable2d1x1k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn3d100x1k = removeIntervals3D(baselineMutable3d100x1k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn3d10x10k = removeIntervals3D(baselineMutable3d10x10k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn3d10x1k = removeIntervals3D(baselineMutable3d10x1k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn3d1x10k = removeIntervals3D(baselineMutable3d1x10k.getAll.map(_.interval).toVector)
  lazy val testIntervalsIn3d1x1k = removeIntervals3D(baselineMutable3d1x1k.getAll.map(_.interval).toVector)

  @State(Scope.Benchmark) abstract class GenericBenchmarkState[I, DimData](
    testIntervals: Vector[I],
    newBaselineData: () => DimData,
    newFeaturedData: () => DimData
  ):
    var testIntervalIndex: Int = 0

    def testInterval(): I = testIntervals(testIntervalIndex)

    var baselineData: DimData = newBaselineData()
    var featuredData: DimData = newFeaturedData()

    def newData(): Unit =
      baselineData = newBaselineData()
      featuredData = newFeaturedData()

    @Setup(Level.Iteration) def setUpIteration(): Unit =
      newData()
      testIntervalIndex = -1 // incremented to 0 by setUpInvocation

    @Setup(Level.Invocation) def setUpInvocation(): Unit =
      testIntervalIndex += 1
      if testIntervalIndex == testIntervals.length then
        newData()
        testIntervalIndex = 0

  //// --- Concrete Benchmark State Classes ---

  // Mutable

  class BenchmarkStateMutable2d100x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d100x1k,
      () => baselineMutable2d100x1k.copy,
      () => featuredMutable2d100x1k.copy
    )

  class BenchmarkStateMutable2d10x10k
    extends GenericBenchmarkState(
      testIntervalsIn2d10x10k,
      () => baselineMutable2d10x10k.copy,
      () => featuredMutable2d10x10k.copy
    )

  class BenchmarkStateMutable2d10x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d10x1k,
      () => baselineMutable2d10x1k.copy,
      () => featuredMutable2d10x1k.copy
    )

  class BenchmarkStateMutable2d1x10k
    extends GenericBenchmarkState(
      testIntervalsIn2d1x10k,
      () => baselineMutable2d1x10k.copy,
      () => featuredMutable2d1x10k.copy
    )

  class BenchmarkStateMutable2d1x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d1x1k,
      () => baselineMutable2d1x1k.copy,
      () => featuredMutable2d1x1k.copy
    )

  class BenchmarkStateMutable3d100x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d100x1k,
      () => baselineMutable3d100x1k.copy,
      () => featuredMutable3d100x1k.copy
    )

  class BenchmarkStateMutable3d10x10k
    extends GenericBenchmarkState(
      testIntervalsIn3d10x10k,
      () => baselineMutable3d10x10k.copy,
      () => featuredMutable3d10x10k.copy
    )

  class BenchmarkStateMutable3d10x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d10x1k,
      () => baselineMutable3d10x1k.copy,
      () => featuredMutable3d10x1k.copy
    )

  class BenchmarkStateMutable3d1x10k
    extends GenericBenchmarkState(
      testIntervalsIn3d1x10k,
      () => baselineMutable3d1x10k.copy,
      () => featuredMutable3d1x10k.copy
    )

  class BenchmarkStateMutable3d1x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d1x1k,
      () => baselineMutable3d1x1k.copy,
      () => featuredMutable3d1x1k.copy
    )

  // Immutable

  class BenchmarkStateImmutable2d100x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d100x1k,
      () => baselineImmutable2d100x1k.copy,
      () => featuredImmutable2d100x1k.copy
    )

  class BenchmarkStateImmutable2d10x10k
    extends GenericBenchmarkState(
      testIntervalsIn2d10x10k,
      () => baselineImmutable2d10x10k.copy,
      () => featuredImmutable2d10x10k.copy
    )

  class BenchmarkStateImmutable2d10x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d10x1k,
      () => baselineImmutable2d10x1k.copy,
      () => featuredImmutable2d10x1k.copy
    )

  class BenchmarkStateImmutable2d1x10k
    extends GenericBenchmarkState(
      testIntervalsIn2d1x10k,
      () => baselineImmutable2d1x10k.copy,
      () => featuredImmutable2d1x10k.copy
    )

  class BenchmarkStateImmutable2d1x1k
    extends GenericBenchmarkState(
      testIntervalsIn2d1x1k,
      () => baselineImmutable2d1x1k.copy,
      () => featuredImmutable2d1x1k.copy
    )

  class BenchmarkStateImmutable3d100x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d100x1k,
      () => baselineImmutable3d100x1k.copy,
      () => featuredImmutable3d100x1k.copy
    )

  class BenchmarkStateImmutable3d10x10k
    extends GenericBenchmarkState(
      testIntervalsIn3d10x10k,
      () => baselineImmutable3d10x10k.copy,
      () => featuredImmutable3d10x10k.copy
    )

  class BenchmarkStateImmutable3d10x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d10x1k,
      () => baselineImmutable3d10x1k.copy,
      () => featuredImmutable3d10x1k.copy
    )

  class BenchmarkStateImmutable3d1x10k
    extends GenericBenchmarkState(
      testIntervalsIn3d1x10k,
      () => baselineImmutable3d1x10k.copy,
      () => featuredImmutable3d1x10k.copy
    )

  class BenchmarkStateImmutable3d1x1k
    extends GenericBenchmarkState(
      testIntervalsIn3d1x1k,
      () => baselineImmutable3d1x1k.copy,
      () => featuredImmutable3d1x1k.copy
    )

  //// --- Concrete Benchmark Classes ---

  // Mutable

  class Mutable2dBench100x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d100x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d100x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable2dBench10x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d10x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d10x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable2dBench10x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d10x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d10x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable2dBench1x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d1x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d1x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable2dBench1x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d1x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable2d1x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable3dBench100x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d100x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d100x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable3dBench10x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d10x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d10x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable3dBench10x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d10x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d10x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable3dBench1x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d1x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d1x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Mutable3dBench1x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d1x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateMutable3d1x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  // Immutable

  class Immutable2dBench100x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d100x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d100x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable2dBench10x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d10x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d10x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable2dBench10x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d10x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d10x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable2dBench1x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d1x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d1x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable2dBench1x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d1x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable2d1x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable3dBench100x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d100x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d100x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable3dBench10x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d10x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d10x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable3dBench10x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d10x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d10x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable3dBench1x10k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d1x10k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d1x10k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))

  class Immutable3dBench1x1k extends GenericBench:
    // @Benchmark def baselineRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d1x1k): Unit =
    //   blackhole.consume(state.baselineData.remove(state.testInterval()))
    @Benchmark def featuredRemove(blackhole: Blackhole, state: BenchmarkStateImmutable3d1x1k): Unit =
      blackhole.consume(state.featuredData.remove(state.testInterval()))
