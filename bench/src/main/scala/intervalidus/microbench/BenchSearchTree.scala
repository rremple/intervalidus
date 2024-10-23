package intervalidus.microbench

import intervalidus.*
import intervalidus.DiscreteInterval1D.interval
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions

object BenchSearchTree extends BenchBase(baselineFeature = Some("noSearchTree"), featuredFeature = None):

  // benchmark only the remove method to see what affect bruteForceUpdate has on updateOrRemove performance.

  //// --- Mutable Bases ---

  @Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @State(Scope.Benchmark)
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  @Fork(
    jvmArgsAppend = Array("-Xmx3g", "-XX:+HeapDumpOnOutOfMemoryError"),
    value = 1
  )
  abstract class GenericMutableBench[
    D <: DiscreteDomainLike[D],
    I <: DiscreteIntervalLike[D, I],
    ValidData <: ValidDataLike[String, D, I, ValidData],
    DiffAction <: DiffActionLike[String, D, I, ValidData, DiffAction],
    DimData <: mutable.MutableBase[String, D, I, ValidData, DiffAction, DimData] & DimensionalBase[String, D, I, ValidData, DiffAction, _]
  ](
    intervalRange: Int,
    data: Vector[ValidData],
    baselineData: => DimData,
    featuredData: => DimData,
    randDomain: () => D,
    randInterval: Int => I,
    randValue: Int => ValidData,
    randValueWithKey: ValidData => ValidData
  ):
    // For replace and replaceByKey, as using totally random data may have unrealistically low hit rates.
    // (vector random access should be superfast)
    val dataSize = data.size

    def useExisting(): ValidData = data(rand.nextInt(dataSize))

    //// Baseline

    @Benchmark
    def baselineGetAt: Option[String] = baselineData.getAt(randDomain())

    @Benchmark
    def baselineIntersects: Boolean = baselineData.intersects(randInterval(intervalRange))

    @Benchmark
    def baselineGetIntersecting: Iterable[ValidData] =
      baselineData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def baselineSet: Unit = baselineData.set(randValue(intervalRange))

    @Benchmark
    def baselineSetIfNoConflict: Boolean = baselineData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def baselineUpdate: Unit = baselineData.update(randValue(intervalRange))

    // The problem with the remove benchmark was that, by using standard intervals in 1D, it winds up removing
    // all the data in warmup. So the benchmark winds up being unrealistically fast because it is running against
    // an empty structure. Here we limit the size of the intervals removed to allow for more data to be leftover.
    @Benchmark
    def baselineRemove: Unit = baselineData.remove(randInterval(1)) // tiny interval

    @Benchmark
    def baselineReplace: Unit =
      val existing = useExisting()
      baselineData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def baselineReplaceByKey: Unit =
      val existing = useExisting()
      baselineData.replaceByKey(existing.key, randValueWithKey(existing))

    //// Featured

    @Benchmark
    def featuredGetAt: Option[String] = featuredData.getAt(randDomain())

    @Benchmark
    def featuredIntersects: Boolean = featuredData.intersects(randInterval(intervalRange))

    @Benchmark
    def featuredGetIntersecting: Iterable[ValidData] = featuredData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def featuredSet: Unit = featuredData.set(randValue(intervalRange))

    @Benchmark
    def featuredSetIfNoConflict: Boolean = featuredData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def featuredUpdate: Unit = featuredData.update(randValue(intervalRange))

    @Benchmark
    def featuredRemove: Unit = featuredData.remove(randInterval(1))

    @Benchmark
    def featuredReplace: Unit =
      val existing = useExisting()
      featuredData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def featuredReplaceByKey: Unit =
      val existing = useExisting()
      featuredData.replaceByKey(existing.key, randValueWithKey(existing))

  abstract class GenericMutable1dBench(
    intervalRange: Int,
    data: Vector[ValidData1D[String, Int]],
    baselineData: => mutable.DataIn1D[String, Int],
    featuredData: => mutable.DataIn1D[String, Int]
  ) extends GenericMutableBench[
      DiscreteDomain1D[Int],
      DiscreteInterval1D[Int],
      ValidData1D[String, Int],
      DiffAction1D[String, Int],
      mutable.DataIn1D[String, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain1d, randInterval1d, randValue1d, randValue1dWithKey):
    val fullRangeInterval = interval(fullRangeMin, fullRangeMax)
    val hits = data.map(_.interval)
    val hitsSize = hits.size
    val gaps = DiscreteInterval1D.complement(hits).flatMap(_.intersectionWith(fullRangeInterval)).toVector
    val gapsSize = gaps.size
    println(s"hit intervals=$hitsSize; gap intervals=$gapsSize")

    def randDomainInInterval1d(i: DiscreteInterval1D[Int]): DiscreteDomain1D[Int] = i match
      case DiscreteInterval1D(DiscreteDomain1D.Point(start), DiscreteDomain1D.Point(end)) =>
        start + rand.nextInt(end - start + 1)
      case unexpected => throw new Exception(s"unexpected result: $unexpected")

    def randHit(): DiscreteDomain1D[Int] = randDomainInInterval1d(hits(rand.nextInt(hitsSize)))

    def randMiss(): DiscreteDomain1D[Int] = randDomainInInterval1d(gaps(rand.nextInt(gapsSize)))

    @Benchmark
    def baselineGetAtHit: Option[String] = baselineData.getAt(randHit())

    @Benchmark
    def baselineGetAtMiss: Option[String] = baselineData.getAt(randMiss())

    @Benchmark
    def featuredGetAtHit: Option[String] = featuredData.getAt(randHit())

    @Benchmark
    def featuredGetAtMiss: Option[String] = featuredData.getAt(randMiss())

  abstract class GenericMutable2dBench(
    intervalRange: Int,
    data: Vector[ValidData2D[String, Int, Int]],
    baselineData: => mutable.DataIn2D[String, Int, Int],
    featuredData: => mutable.DataIn2D[String, Int, Int]
  ) extends GenericMutableBench[
      DiscreteDomain2D[Int, Int],
      DiscreteInterval2D[Int, Int],
      ValidData2D[String, Int, Int],
      DiffAction2D[String, Int, Int],
      mutable.DataIn2D[String, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain2d, randInterval2d, randValue2d, randValue2dWithKey):

    @Benchmark
    def baselineGetByHorizontalIndex: mutable.DataIn1D[String, Int] = baselineData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def baselineGetByVerticalIndex: mutable.DataIn1D[String, Int] = baselineData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def featuredGetByHorizontalIndex: mutable.DataIn1D[String, Int] = featuredData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def featuredGetByVerticalIndex: mutable.DataIn1D[String, Int] = featuredData.getByVerticalIndex(randDomain1d())

  abstract class GenericMutable3dBench(
    intervalRange: Int,
    data: Vector[ValidData3D[String, Int, Int, Int]],
    baselineData: => mutable.DataIn3D[String, Int, Int, Int],
    featuredData: => mutable.DataIn3D[String, Int, Int, Int]
  ) extends GenericMutableBench[
      DiscreteDomain3D[Int, Int, Int],
      DiscreteInterval3D[Int, Int, Int],
      ValidData3D[String, Int, Int, Int],
      DiffAction3D[String, Int, Int, Int],
      mutable.DataIn3D[String, Int, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain3d, randInterval3d, randValue3d, randValue3dWithKey):

    @Benchmark
    def baselineGetByHorizontalIndex: mutable.DataIn2D[String, Int, Int] =
      baselineData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def baselineGetByVerticalIndex: mutable.DataIn2D[String, Int, Int] = baselineData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def baselineGetByDepthIndex: mutable.DataIn2D[String, Int, Int] = baselineData.getByDepthIndex(randDomain1d())

    @Benchmark
    def featuredGetByHorizontalIndex: mutable.DataIn2D[String, Int, Int] =
      featuredData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def featuredGetByVerticalIndex: mutable.DataIn2D[String, Int, Int] = featuredData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def featuredGetByDepthIndex: mutable.DataIn2D[String, Int, Int] = featuredData.getByDepthIndex(randDomain1d())

  //// --- Immutable Bases ---

  @Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @State(Scope.Benchmark)
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  @Fork(
    jvmArgsAppend = Array("-Xmx3g", "-XX:+HeapDumpOnOutOfMemoryError"),
    value = 1
  )
  abstract class GenericImmutableBench[
    D <: DiscreteDomainLike[D],
    I <: DiscreteIntervalLike[D, I],
    ValidData <: ValidDataLike[String, D, I, ValidData],
    DiffAction <: DiffActionLike[String, D, I, ValidData, DiffAction], 
    DimData <: immutable.ImmutableBase[String, D, I, ValidData, DiffAction, DimData]
  ](
    intervalRange: Int,
    data: Vector[ValidData],
    baselineData: => DimData,
    featuredData: => DimData,
    randDomain: () => D,
    randInterval: Int => I,
    randValue: Int => ValidData,
    randValueWithKey: ValidData => ValidData
  ):
    // For replace and replaceByKey, as using totally random data may have unrealistically low hit rates.
    // (vector random access should be superfast)
    val dataSize = data.size

    def useExisting(): ValidData = data(rand.nextInt(dataSize))

    //// Baseline

    @Benchmark
    def baselineGetAt: Option[String] = baselineData.getAt(randDomain())

    @Benchmark
    def baselineIntersects: Boolean = baselineData.intersects(randInterval(intervalRange))

    @Benchmark
    def baselineGetIntersecting: Iterable[ValidData] = baselineData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def baselineSet: DimData = baselineData.set(randValue(intervalRange))

    @Benchmark
    def baselineSetIfNoConflict: Option[DimData] =
      baselineData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def baselineUpdate: DimData = baselineData.update(randValue(intervalRange))

    // The problem with the remove benchmark was that, by using standard intervals in 1D, it winds up removing
    // all the data in warmup. So the benchmark winds up being unrealistically fast because it is running against
    // an empty structure. Here we limit the size of the intervals removed to allow for more data to be leftover.
    @Benchmark
    def baselineRemove: DimData = baselineData.remove(randInterval(1)) // tiny interval

    @Benchmark
    def baselineReplace: DimData =
      val existing = useExisting()
      baselineData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def baselineReplaceByKey: DimData =
      val existing = useExisting()
      baselineData.replaceByKey(existing.key, randValueWithKey(existing))

    //// Featured

    @Benchmark
    def featuredGetAt: Option[String] = featuredData.getAt(randDomain())

    @Benchmark
    def featuredIntersects: Boolean = featuredData.intersects(randInterval(intervalRange))

    @Benchmark
    def featuredGetIntersecting: Iterable[ValidData] = featuredData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def featuredSet: DimData = featuredData.set(randValue(intervalRange))

    @Benchmark
    def featuredSetIfNoConflict: Option[DimData] =
      featuredData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def featuredUpdate: DimData = featuredData.update(randValue(intervalRange))

    @Benchmark
    def featuredRemove: DimData = featuredData.remove(randInterval(1))

    @Benchmark
    def featuredReplace: DimData =
      val existing = useExisting()
      featuredData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def featuredReplaceByKey: DimData =
      val existing = useExisting()
      featuredData.replaceByKey(existing.key, randValueWithKey(existing))

  abstract class GenericImmutable1dBench(
    intervalRange: Int,
    data: Vector[ValidData1D[String, Int]],
    baselineData: => immutable.DataIn1D[String, Int],
    featuredData: => immutable.DataIn1D[String, Int]
  ) extends GenericImmutableBench[
      DiscreteDomain1D[Int],
      DiscreteInterval1D[Int],
      ValidData1D[String, Int],
      DiffAction1D[String, Int],
      immutable.DataIn1D[String, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain1d, randInterval1d, randValue1d, randValue1dWithKey):
    val fullRangeInterval = interval(fullRangeMin, fullRangeMax)
    val hits = data.map(_.interval)
    val hitsSize = hits.size
    val gaps = DiscreteInterval1D.complement(hits).flatMap(_.intersectionWith(fullRangeInterval)).toVector
    val gapsSize = gaps.size
    println(s"hit intervals=$hitsSize; gap intervals=$gapsSize")

    def randDomainInInterval1d(i: DiscreteInterval1D[Int]): DiscreteDomain1D[Int] = i match
      case DiscreteInterval1D(DiscreteDomain1D.Point(start), DiscreteDomain1D.Point(end)) =>
        start + rand.nextInt(end - start + 1)
      case unexpected => throw new Exception(s"unexpected result: $unexpected")

    def randHit(): DiscreteDomain1D[Int] = randDomainInInterval1d(hits(rand.nextInt(hitsSize)))

    def randMiss(): DiscreteDomain1D[Int] = randDomainInInterval1d(gaps(rand.nextInt(gapsSize)))

    @Benchmark
    def baselineGetAtHit: Option[String] = baselineData.getAt(randHit())

    @Benchmark
    def baselineGetAtMiss: Option[String] = baselineData.getAt(randMiss())

    @Benchmark
    def featuredGetAtHit: Option[String] = featuredData.getAt(randHit())

    @Benchmark
    def featuredGetAtMiss: Option[String] = featuredData.getAt(randMiss())

  abstract class GenericImmutable2dBench(
    intervalRange: Int,
    data: Vector[ValidData2D[String, Int, Int]],
    baselineData: => immutable.DataIn2D[String, Int, Int],
    featuredData: => immutable.DataIn2D[String, Int, Int]
  ) extends GenericImmutableBench[
      DiscreteDomain2D[Int, Int],
      DiscreteInterval2D[Int, Int],
      ValidData2D[String, Int, Int],
      DiffAction2D[String, Int, Int],
      immutable.DataIn2D[String, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain2d, randInterval2d, randValue2d, randValue2dWithKey):

    @Benchmark
    def baselineGetByHorizontalIndex: immutable.DataIn1D[String, Int] =
      baselineData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def baselineGetByVerticalIndex: immutable.DataIn1D[String, Int] = baselineData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def featuredGetByHorizontalIndex: immutable.DataIn1D[String, Int] =
      featuredData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def featuredGetByVerticalIndex: immutable.DataIn1D[String, Int] = featuredData.getByVerticalIndex(randDomain1d())

  abstract class GenericImmutable3dBench(
    intervalRange: Int,
    data: Vector[ValidData3D[String, Int, Int, Int]],
    baselineData: => immutable.DataIn3D[String, Int, Int, Int],
    featuredData: => immutable.DataIn3D[String, Int, Int, Int]
  ) extends GenericImmutableBench[
      DiscreteDomain3D[Int, Int, Int],
      DiscreteInterval3D[Int, Int, Int],
      ValidData3D[String, Int, Int, Int],
      DiffAction3D[String, Int, Int, Int],
      immutable.DataIn3D[String, Int, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain3d, randInterval3d, randValue3d, randValue3dWithKey):

    @Benchmark
    def baselineGetByHorizontalIndex: immutable.DataIn2D[String, Int, Int] =
      baselineData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def baselineGetByVerticalIndex: immutable.DataIn2D[String, Int, Int] =
      baselineData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def baselineGetByDepthIndex: immutable.DataIn2D[String, Int, Int] = baselineData.getByDepthIndex(randDomain1d())

    @Benchmark
    def featuredGetByHorizontalIndex: immutable.DataIn2D[String, Int, Int] =
      featuredData.getByHorizontalIndex(randDomain1d())

    @Benchmark
    def featuredGetByVerticalIndex: immutable.DataIn2D[String, Int, Int] =
      featuredData.getByVerticalIndex(randDomain1d())

    @Benchmark
    def featuredGetByDepthIndex: immutable.DataIn2D[String, Int, Int] = featuredData.getByDepthIndex(randDomain1d())

  //// --- Concrete Benchmark Classes ---

  // Mutable

  class Mutable1dBench100x1k
    extends GenericMutable1dBench(100 * 100, validDataIn1d100x1k, baselineMutable1d100x1k, featuredMutable1d100x1k)
  class Mutable1dBench10x10k
    extends GenericMutable1dBench(10 * 100, validDataIn1d10x10k, baselineMutable1d10x10k, featuredMutable1d10x10k)
  class Mutable1dBench10x1k
    extends GenericMutable1dBench(10 * 100, validDataIn1d10x1k, baselineMutable1d10x1k, featuredMutable1d10x1k)
  class Mutable1dBench1x10k
    extends GenericMutable1dBench(1 * 100, validDataIn1d1x10k, baselineMutable1d1x10k, featuredMutable1d1x10k)
  class Mutable1dBench1x1k
    extends GenericMutable1dBench(1 * 100, validDataIn1d1x1k, baselineMutable1d1x1k, featuredMutable1d1x1k)

  class Mutable2dBench100x1k
    extends GenericMutable2dBench(100 * 100, validDataIn2d100x1k, baselineMutable2d100x1k, featuredMutable2d100x1k)
  class Mutable2dBench10x10k
    extends GenericMutable2dBench(10 * 100, validDataIn2d10x10k, baselineMutable2d10x10k, featuredMutable2d10x10k)
  class Mutable2dBench10x1k
    extends GenericMutable2dBench(10 * 100, validDataIn2d10x1k, baselineMutable2d10x1k, featuredMutable2d10x1k)
  class Mutable2dBench1x10k
    extends GenericMutable2dBench(1 * 100, validDataIn2d1x10k, baselineMutable2d1x10k, featuredMutable2d1x10k)
  class Mutable2dBench1x1k
    extends GenericMutable2dBench(1 * 100, validDataIn2d1x1k, baselineMutable2d1x1k, featuredMutable2d1x1k)

  class Mutable3dBench100x1k
    extends GenericMutable3dBench(100 * 100, validDataIn3d100x1k, baselineMutable3d100x1k, featuredMutable3d100x1k)
  class Mutable3dBench10x10k
    extends GenericMutable3dBench(10 * 100, validDataIn3d10x10k, baselineMutable3d10x10k, featuredMutable3d10x10k)
  class Mutable3dBench10x1k
    extends GenericMutable3dBench(10 * 100, validDataIn3d10x1k, baselineMutable3d10x1k, featuredMutable3d10x1k)
  class Mutable3dBench1x10k
    extends GenericMutable3dBench(1 * 100, validDataIn3d1x10k, baselineMutable3d1x10k, featuredMutable3d1x10k)
  class Mutable3dBench1x1k
    extends GenericMutable3dBench(1 * 100, validDataIn3d1x1k, baselineMutable3d1x1k, featuredMutable3d1x1k)

  // Immutable

  class Immutable1dBench100x1k
    extends GenericImmutable1dBench(
      100 * 100,
      validDataIn1d100x1k,
      baselineImmutable1d100x1k,
      featuredImmutable1d100x1k
    )
  class Immutable1dBench10x10k
    extends GenericImmutable1dBench(10 * 100, validDataIn1d10x10k, baselineImmutable1d10x10k, featuredImmutable1d10x10k)
  class Immutable1dBench10x1k
    extends GenericImmutable1dBench(10 * 100, validDataIn1d10x1k, baselineImmutable1d10x1k, featuredImmutable1d10x1k)
  class Immutable1dBench1x10k
    extends GenericImmutable1dBench(1 * 100, validDataIn1d1x10k, baselineImmutable1d1x10k, featuredImmutable1d1x10k)
  class Immutable1dBench1x1k
    extends GenericImmutable1dBench(1 * 100, validDataIn1d1x1k, baselineImmutable1d1x1k, featuredImmutable1d1x1k)

  class Immutable2dBench100x1k
    extends GenericImmutable2dBench(
      100 * 100,
      validDataIn2d100x1k,
      baselineImmutable2d100x1k,
      featuredImmutable2d100x1k
    )
  class Immutable2dBench10x10k
    extends GenericImmutable2dBench(10 * 100, validDataIn2d10x10k, baselineImmutable2d10x10k, featuredImmutable2d10x10k)
  class Immutable2dBench10x1k
    extends GenericImmutable2dBench(10 * 100, validDataIn2d10x1k, baselineImmutable2d10x1k, featuredImmutable2d10x1k)
  class Immutable2dBench1x10k
    extends GenericImmutable2dBench(1 * 100, validDataIn2d1x10k, baselineImmutable2d1x10k, featuredImmutable2d1x10k)
  class Immutable2dBench1x1k
    extends GenericImmutable2dBench(1 * 100, validDataIn2d1x1k, baselineImmutable2d1x1k, featuredImmutable2d1x1k)

  class Immutable3dBench100x1k
    extends GenericImmutable3dBench(
      100 * 100,
      validDataIn3d100x1k,
      baselineImmutable3d100x1k,
      featuredImmutable3d100x1k
    )
  class Immutable3dBench10x10k
    extends GenericImmutable3dBench(10 * 100, validDataIn3d10x10k, baselineImmutable3d10x10k, featuredImmutable3d10x10k)
  class Immutable3dBench10x1k
    extends GenericImmutable3dBench(10 * 100, validDataIn3d10x1k, baselineImmutable3d10x1k, featuredImmutable3d10x1k)
  class Immutable3dBench1x10k
    extends GenericImmutable3dBench(1 * 100, validDataIn3d1x10k, baselineImmutable3d1x10k, featuredImmutable3d1x10k)
  class Immutable3dBench1x1k
    extends GenericImmutable3dBench(1 * 100, validDataIn3d1x1k, baselineImmutable3d1x1k, featuredImmutable3d1x1k)
