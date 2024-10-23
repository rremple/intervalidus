package intervalidus.microbench

import intervalidus.*
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions

object BenchBruteForce extends BenchBase(baselineFeature = None, featuredFeature = Some("bruteForceUpdate")):

  // benchmark only the remove method to see what affect bruteForceUpdate has on updateOrRemove performance.

  // In general, bench all the applicable methods
  //
  // Not applicable to test data set:
  //  def get: V
  //  def getOption: Option[V]
  //
  // Unlikely to be important:
  //  def toImmutable: DataIn2D[V, R1, R2]
  //  def toMutable: DataIn2D[V, R1, R2]
  //  def copy: DataIn2D[V, R1, R2]
  //  def applyDiffActions(diffActions: Iterable[DiffAction2D[V, R1, R2]]): Unit
  //  def diffActionsFrom(old: DataIn2DBase[V, R1, R2]): Iterable[DiffAction2D[V, R1, R2]]
  //  def syncWith(that: DataIn2D[V, R1, R2]): Unit
  //  def isDefinedAt(key: DiscreteDomain2D[R1, R2]): Boolean // same profile as getAt
  //
  // Operates on the full structure every time -- unlikely to be important, but maybe validate that later:
  //  def getAll: Iterable[ValidData2D[V, R1, R2]]
  //  def compress(value: V): Unit
  //  def compressAll(): Unit
  //  def recompressAll(): Unit
  //  def domain: Iterable[DiscreteInterval2D[R1, R2]]
  //  def flip: DataIn2D[V, R2, R1]
  //  def map(f: (ValidData2D[V, R1, R2]) => ValidData2D[V, R1, R2]): Unit
  //  def mapValues(f: V => V): Unit
  //  def flatMap(f: (ValidData2D[V, R1, R2]) =>
  //    DimensionalBase[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2], ValidData2D[V, R1, R2]]): Unit
  //  def filter(p: (ValidData2D[V, R1, R2]) => Boolean): Unit
  //  def foldLeft[B](z: B)(op: (B, ValidData2D[V, R1, R2]) => B): B
  //  def zip[B](that: DataIn2DBase[B, R1, R2]): DataIn2D[(V, B), R1, R2]
  //  def zipAll[B](that: DataIn2DBase[B, R1, R2], thisElem: V, thatElem: B): DataIn2D[(V, B), R1, R2]
  //
  // Targeting these for benchmarking (note that mutable signatures are shown below):
  //  def getAt(domainIndex: DiscreteDomain2D[R1, R2]): Option[V]
  //  def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn1D[V, R2]
  //  def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn1D[V, R1]
  //  def intersects(interval: DiscreteInterval2D[R1, R2]): Boolean
  //  def getIntersecting(interval: DiscreteInterval2D[R1, R2]): Iterable[ValidData2D[V, R1, R2]]
  //  def remove(interval: DiscreteInterval2D[R1, R2]): Unit
  //  def replace(oldData: ValidData2D[V, R1, R2], newData: ValidData2D[V, R1, R2]): Unit
  //  def replaceByKey(key: DiscreteDomain2D[R1, R2], newData: ValidData2D[V, R1, R2]): Unit
  //  def set(newData: ValidData2D[V, R1, R2]): Unit
  //  def setIfNoConflict(newData: ValidData2D[V, R1, R2]): Boolean
  //  def update(data: ValidData2D[V, R1, R2]): Unit

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

    def randExistingTiny(): I

    def randPoint(existing: DiscreteInterval1D[Int]): DiscreteInterval1D[Int] =
      existing match
        case DiscreteInterval1D(DiscreteDomain1D.Point(start), DiscreteDomain1D.Point(end)) =>
          val size = end - start + 1
          DiscreteInterval1D.intervalAt(rand.nextInt(size) + start)
        case whoops => throw new Exception(s"why not a point? $whoops")

    @Benchmark
    def baselineRemove: Unit = baselineData.remove(randExistingTiny())

    @Benchmark
    def featuredRemove: Unit = featuredData.remove(randExistingTiny())

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

    override def randExistingTiny(): DiscreteInterval1D[Int] =
      randPoint(useExisting().interval)

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

    override def randExistingTiny(): DiscreteInterval2D[Int, Int] =
      val existing = useExisting().interval
      DiscreteInterval2D(randPoint(existing.horizontal), randPoint(existing.vertical))

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

    override def randExistingTiny(): DiscreteInterval3D[Int, Int, Int] =
      val existing = useExisting().interval
      DiscreteInterval3D(randPoint(existing.horizontal), randPoint(existing.vertical), randPoint(existing.depth))

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

    def randExistingTiny(): I

    def randPoint(existing: DiscreteInterval1D[Int]): DiscreteInterval1D[Int] =
      existing match
        case DiscreteInterval1D(DiscreteDomain1D.Point(start), DiscreteDomain1D.Point(end)) =>
          val size = end - start + 1
          DiscreteInterval1D.intervalAt(rand.nextInt(size) + start)
        case whoops => throw new Exception(s"why not a point? $whoops")

    @Benchmark
    def baselineRemove: DimData = baselineData.remove(randExistingTiny())

    @Benchmark
    def featuredRemove: DimData = featuredData.remove(randExistingTiny())

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

    override def randExistingTiny(): DiscreteInterval1D[Int] =
      randPoint(useExisting().interval)

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

    override def randExistingTiny(): DiscreteInterval2D[Int, Int] =
      val existing = useExisting().interval
      DiscreteInterval2D(randPoint(existing.horizontal), randPoint(existing.vertical))

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

    override def randExistingTiny(): DiscreteInterval3D[Int, Int, Int] =
      val existing = useExisting().interval
      DiscreteInterval3D(randPoint(existing.horizontal), randPoint(existing.vertical), randPoint(existing.depth))

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
