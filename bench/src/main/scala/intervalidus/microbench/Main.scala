package intervalidus.microbench

import intervalidus.*
import intervalidus.DimensionalBase.{DataLike, DomainLike, IntervalLike}
import intervalidus.DiscreteInterval1D.interval
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

object Main:
  val rand = Random()
  val (fullRangeMin, fullRangeMax) = (-500_000, 500_000)
  def fullRangeSize = fullRangeMax - fullRangeMin

  def randDomain1d(): DiscreteDomain1D[Int] = rand.nextInt(fullRangeSize) + fullRangeMin
  def randDomain2d(): DiscreteDomain2D[Int, Int] = randDomain1d() x randDomain1d()
  def randDomain3d(): DiscreteDomain3D[Int, Int, Int] = randDomain1d() x randDomain1d() x randDomain1d()

  def randInterval1d(intervalRange: Int): DiscreteInterval1D[Int] =
    val start = rand.nextInt(fullRangeSize) + fullRangeMin
    val size = rand.nextInt(intervalRange) + 1
    val end = math.min(fullRangeMax, start + size)
    interval(start, end)
  def randInterval2d(intervalRange: Int): DiscreteInterval2D[Int, Int] =
    randInterval1d(intervalRange) x randInterval1d(intervalRange)
  def randInterval3d(intervalRange: Int): DiscreteInterval3D[Int, Int, Int] =
    randInterval1d(intervalRange) x randInterval1d(intervalRange) x randInterval1d(intervalRange)

  def randValue1d(intervalRange: Int): ValidData1D[String, Int] =
    val interval = randInterval1d(intervalRange)
    interval -> interval.toString
  def randValue2d(intervalRange: Int): ValidData2D[String, Int, Int] =
    val interval = randInterval2d(intervalRange)
    interval -> interval.toString
  def randValue3d(intervalRange: Int): ValidData3D[String, Int, Int, Int] =
    val interval = randInterval3d(intervalRange)
    interval -> interval.toString

  def shorten(existing: DiscreteInterval1D[Int]) =
    interval(existing.start, existing.end.predecessor max existing.start)

  def randValue1dWithKey(existing: ValidData1D[String, Int]) =
    val newInterval = shorten(existing.interval)
    newInterval -> newInterval.toString

  def randValue2dWithKey(existing: ValidData2D[String, Int, Int]) =
    val newInterval = shorten(existing.interval.horizontal) x shorten(existing.interval.vertical)
    newInterval -> newInterval.toString

  def randValue3dWithKey(existing: ValidData3D[String, Int, Int, Int]) =
    val newInterval =
      shorten(existing.interval.horizontal) x shorten(existing.interval.vertical) x shorten(existing.interval.depth)
    newInterval -> newInterval.toString

  // faster to construct disjoint valid data ranges using mutable data structures
  def validDataIn1d(intervalRange: Int, intervals: Int): Vector[ValidData1D[String, Int]] =
    println(s"Random 1D 1-$intervalRange size x ${intervals / 1000}k intervals...")
    val mutableFixture = mutable.DataIn1D[String, Int]()
    (1 to intervals).foreach(_ => mutableFixture.set(randValue1d(intervalRange)))
    val data = mutableFixture.getAll.toVector
    println(s"Random 1D 1-$intervalRange size x ${intervals / 1000}k intervals: ${data.size}")
    data

  def validDataIn2d(intervalRange: Int, intervals: Int): Vector[ValidData2D[String, Int, Int]] =
    println(s"Random 2D 1-$intervalRange size x ${intervals / 1000}k intervals...")
    val mutableFixture = mutable.DataIn2D[String, Int, Int]()
    (1 to intervals).foreach(_ => mutableFixture.set(randValue2d(intervalRange)))
    val data = mutableFixture.getAll.toVector
    println(s"Random 2D 1-$intervalRange size x ${intervals / 1000}k intervals: ${data.size}")
    data

  def validDataIn3d(intervalRange: Int, intervals: Int): Vector[ValidData3D[String, Int, Int, Int]] =
    println(s"Random 3D 1-$intervalRange size x ${intervals / 1000}k intervals...")
    val mutableFixture = mutable.DataIn3D[String, Int, Int, Int]()
    (1 to intervals).foreach(_ => mutableFixture.set(randValue3d(intervalRange)))
    val data = mutableFixture.getAll.toVector
    println(s"Random 3D 1-$intervalRange size x ${intervals / 1000}k intervals: ${data.size}")
    data

  lazy val validDataIn1d100x1k = validDataIn1d(100 * 100, 1 * 1_000)
  lazy val validDataIn1d10x10k = validDataIn1d(10 * 100, 10 * 1_000)
  lazy val validDataIn1d10x1k = validDataIn1d(10 * 100, 1 * 1_000)
  lazy val validDataIn1d1x10k = validDataIn1d(1 * 100, 10 * 1_000)
  lazy val validDataIn1d1x1k = validDataIn1d(1 * 100, 1 * 1_000)

  lazy val validDataIn2d100x1k = validDataIn2d(100 * 100, 1 * 1_000)
  lazy val validDataIn2d10x10k = validDataIn2d(10 * 100, 10 * 1_000)
  lazy val validDataIn2d10x1k = validDataIn2d(10 * 100, 1 * 1_000)
  lazy val validDataIn2d1x10k = validDataIn2d(1 * 100, 10 * 1_000)
  lazy val validDataIn2d1x1k = validDataIn2d(1 * 100, 1 * 1_000)

  lazy val validDataIn3d100x1k = validDataIn3d(100 * 100, 1 * 1_000)
  lazy val validDataIn3d10x10k = validDataIn3d(10 * 100, 10 * 1_000)
  lazy val validDataIn3d10x1k = validDataIn3d(10 * 100, 1 * 1_000)
  lazy val validDataIn3d1x10k = validDataIn3d(1 * 100, 10 * 1_000)
  lazy val validDataIn3d1x1k = validDataIn3d(1 * 100, 1 * 1_000)

  val baselineFeature = "noSearchTree"

  // note the ?=> is important so the context parameter given gets passed along
  def baselineFromData[T, D](buildFrom: Experimental ?=> Iterable[D] => T, validData: Iterable[D]): T =
    println("Creating baseline fixture...")
    val start = System.currentTimeMillis()
    given Experimental = Experimental(baselineFeature)
    val baselineFixture: T = buildFrom(validData)
    println(s"Baseline fixture created, ${System.currentTimeMillis() - start} ms.")
    baselineFixture

  // note the ?=> is important so the context parameter given gets passed along
  def featuredFromData[T, D](buildFrom: Experimental ?=> Iterable[D] => T, validData: Iterable[D]): T =
    println("Creating featured fixture...")
    val start = System.currentTimeMillis()
    given Experimental = Experimental.none
    val featuredFixture = buildFrom(validData)
    println(s"Featured fixture created, ${System.currentTimeMillis() - start} ms.")
    featuredFixture

  val buildMutable1d: Experimental ?=> Iterable[ValidData1D[String, Int]] => mutable.DataIn1D[String, Int] =
    mutable.DataIn1D[String, Int](_)
  val buildMutable2d: Experimental ?=> Iterable[ValidData2D[String, Int, Int]] => mutable.DataIn2D[String, Int, Int] =
    mutable.DataIn2D[String, Int, Int](_)
  val buildMutable3d
    : Experimental ?=> Iterable[ValidData3D[String, Int, Int, Int]] => mutable.DataIn3D[String, Int, Int, Int] =
    mutable.DataIn3D[String, Int, Int, Int](_)
  val buildImmutable1d: Experimental ?=> Iterable[ValidData1D[String, Int]] => immutable.DataIn1D[String, Int] =
    immutable.DataIn1D[String, Int](_)
  val buildImmutable2d
    : Experimental ?=> Iterable[ValidData2D[String, Int, Int]] => immutable.DataIn2D[String, Int, Int] =
    immutable.DataIn2D[String, Int, Int](_)
  val buildImmutable3d
    : Experimental ?=> Iterable[ValidData3D[String, Int, Int, Int]] => immutable.DataIn3D[String, Int, Int, Int] =
    immutable.DataIn3D[String, Int, Int, Int](_)

  // hundreds by thousands, all lazy so we only create the data we use
  lazy val baselineMutable1d100x1k = baselineFromData(buildMutable1d, validDataIn1d100x1k)
  lazy val baselineMutable1d10x10k = baselineFromData(buildMutable1d, validDataIn1d10x10k)
  lazy val baselineMutable1d10x1k = baselineFromData(buildMutable1d, validDataIn1d10x1k)
  lazy val baselineMutable1d1x10k = baselineFromData(buildMutable1d, validDataIn1d1x10k)
  lazy val baselineMutable1d1x1k = baselineFromData(buildMutable1d, validDataIn1d1x1k)

  lazy val baselineMutable2d100x1k = baselineFromData(buildMutable2d, validDataIn2d100x1k)
  lazy val baselineMutable2d10x10k = baselineFromData(buildMutable2d, validDataIn2d10x10k)
  lazy val baselineMutable2d10x1k = baselineFromData(buildMutable2d, validDataIn2d10x1k)
  lazy val baselineMutable2d1x10k = baselineFromData(buildMutable2d, validDataIn2d1x10k)
  lazy val baselineMutable2d1x1k = baselineFromData(buildMutable2d, validDataIn2d1x1k)

  lazy val baselineMutable3d100x1k = baselineFromData(buildMutable3d, validDataIn3d100x1k)
  lazy val baselineMutable3d10x10k = baselineFromData(buildMutable3d, validDataIn3d10x10k)
  lazy val baselineMutable3d10x1k = baselineFromData(buildMutable3d, validDataIn3d10x1k)
  lazy val baselineMutable3d1x10k = baselineFromData(buildMutable3d, validDataIn3d1x10k)
  lazy val baselineMutable3d1x1k = baselineFromData(buildMutable3d, validDataIn3d1x1k)

  lazy val baselineImmutable1d100x1k = baselineFromData(buildImmutable1d, validDataIn1d100x1k)
  lazy val baselineImmutable1d10x10k = baselineFromData(buildImmutable1d, validDataIn1d10x10k)
  lazy val baselineImmutable1d10x1k = baselineFromData(buildImmutable1d, validDataIn1d10x1k)
  lazy val baselineImmutable1d1x10k = baselineFromData(buildImmutable1d, validDataIn1d1x10k)
  lazy val baselineImmutable1d1x1k = baselineFromData(buildImmutable1d, validDataIn1d1x1k)

  lazy val baselineImmutable2d100x1k = baselineFromData(buildImmutable2d, validDataIn2d100x1k)
  lazy val baselineImmutable2d10x10k = baselineFromData(buildImmutable2d, validDataIn2d10x10k)
  lazy val baselineImmutable2d10x1k = baselineFromData(buildImmutable2d, validDataIn2d10x1k)
  lazy val baselineImmutable2d1x10k = baselineFromData(buildImmutable2d, validDataIn2d1x10k)
  lazy val baselineImmutable2d1x1k = baselineFromData(buildImmutable2d, validDataIn2d1x1k)

  lazy val baselineImmutable3d100x1k = baselineFromData(buildImmutable3d, validDataIn3d100x1k)
  lazy val baselineImmutable3d10x10k = baselineFromData(buildImmutable3d, validDataIn3d10x10k)
  lazy val baselineImmutable3d10x1k = baselineFromData(buildImmutable3d, validDataIn3d10x1k)
  lazy val baselineImmutable3d1x10k = baselineFromData(buildImmutable3d, validDataIn3d1x10k)
  lazy val baselineImmutable3d1x1k = baselineFromData(buildImmutable3d, validDataIn3d1x1k)

  lazy val featuredMutable1d100x1k = featuredFromData(buildMutable1d, validDataIn1d100x1k)
  lazy val featuredMutable1d10x10k = featuredFromData(buildMutable1d, validDataIn1d10x10k)
  lazy val featuredMutable1d10x1k = featuredFromData(buildMutable1d, validDataIn1d10x1k)
  lazy val featuredMutable1d1x10k = featuredFromData(buildMutable1d, validDataIn1d1x10k)
  lazy val featuredMutable1d1x1k = featuredFromData(buildMutable1d, validDataIn1d1x1k)

  lazy val featuredMutable2d100x1k = featuredFromData(buildMutable2d, validDataIn2d100x1k)
  lazy val featuredMutable2d10x10k = featuredFromData(buildMutable2d, validDataIn2d10x10k)
  lazy val featuredMutable2d10x1k = featuredFromData(buildMutable2d, validDataIn2d10x1k)
  lazy val featuredMutable2d1x10k = featuredFromData(buildMutable2d, validDataIn2d1x10k)
  lazy val featuredMutable2d1x1k = featuredFromData(buildMutable2d, validDataIn2d1x1k)

  lazy val featuredMutable3d100x1k = featuredFromData(buildMutable3d, validDataIn3d100x1k)
  lazy val featuredMutable3d10x10k = featuredFromData(buildMutable3d, validDataIn3d10x10k)
  lazy val featuredMutable3d10x1k = featuredFromData(buildMutable3d, validDataIn3d10x1k)
  lazy val featuredMutable3d1x10k = featuredFromData(buildMutable3d, validDataIn3d1x10k)
  lazy val featuredMutable3d1x1k = featuredFromData(buildMutable3d, validDataIn3d1x1k)

  lazy val featuredImmutable1d100x1k = featuredFromData(buildImmutable1d, validDataIn1d100x1k)
  lazy val featuredImmutable1d10x10k = featuredFromData(buildImmutable1d, validDataIn1d10x10k)
  lazy val featuredImmutable1d10x1k = featuredFromData(buildImmutable1d, validDataIn1d10x1k)
  lazy val featuredImmutable1d1x10k = featuredFromData(buildImmutable1d, validDataIn1d1x10k)
  lazy val featuredImmutable1d1x1k = featuredFromData(buildImmutable1d, validDataIn1d1x1k)

  lazy val featuredImmutable2d100x1k = featuredFromData(buildImmutable2d, validDataIn2d100x1k)
  lazy val featuredImmutable2d10x10k = featuredFromData(buildImmutable2d, validDataIn2d10x10k)
  lazy val featuredImmutable2d10x1k = featuredFromData(buildImmutable2d, validDataIn2d10x1k)
  lazy val featuredImmutable2d1x10k = featuredFromData(buildImmutable2d, validDataIn2d1x10k)
  lazy val featuredImmutable2d1x1k = featuredFromData(buildImmutable2d, validDataIn2d1x1k)

  lazy val featuredImmutable3d100x1k = featuredFromData(buildImmutable3d, validDataIn3d100x1k)
  lazy val featuredImmutable3d10x10k = featuredFromData(buildImmutable3d, validDataIn3d10x10k)
  lazy val featuredImmutable3d10x1k = featuredFromData(buildImmutable3d, validDataIn3d10x1k)
  lazy val featuredImmutable3d1x10k = featuredFromData(buildImmutable3d, validDataIn3d1x10k)
  lazy val featuredImmutable3d1x1k = featuredFromData(buildImmutable3d, validDataIn3d1x1k)

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
    D <: DomainLike[D],
    I <: IntervalLike[D, I],
    ValidData <: DataLike[String, D, I, ValidData],
    DimData <: mutable.MutableBase[String, D, I, ValidData, DimData] & DimensionalBase[String, D, I, ValidData, _]
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
    D <: DomainLike[D],
    I <: IntervalLike[D, I],
    ValidData <: DataLike[String, D, I, ValidData],
    DimData <: immutable.ImmutableBase[String, D, I, ValidData, DimData]
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
