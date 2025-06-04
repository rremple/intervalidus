package intervalidus.microbench

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.Interval1D.interval
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

/**
  * Common data and methods useful in benchmarking intervalidus
  */
trait BenchBase(baselineFeature: Option[String], featuredFeature: Option[String]):
  @Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @Measurement(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
  @State(Scope.Benchmark)
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  @Fork(
    jvmArgsAppend = Array("-Xmx3g", "-XX:+HeapDumpOnOutOfMemoryError"),
    value = 1
  )
  abstract class GenericBench

  val rand = Random()
  val (fullRangeMin, fullRangeMax) = (-500_000, 500_000)
  def fullRangeSize = fullRangeMax - fullRangeMin

  def randDomain1d(): Domain1D[Int] = rand.nextInt(fullRangeSize) + fullRangeMin
  def randDomain2d(): Domain2D[Int, Int] = randDomain1d() x randDomain1d()
  def randDomain3d(): Domain3D[Int, Int, Int] = randDomain1d() x randDomain1d() x randDomain1d()

  def randInterval1d(intervalRange: Int): Interval1D[Int] =
    val start = rand.nextInt(fullRangeSize) + fullRangeMin
    val size = rand.nextInt(intervalRange) + 1
    val end = math.min(fullRangeMax, start + size)
    interval(start, end)
  def randInterval2d(intervalRange: Int): Interval2D[Int, Int] =
    randInterval1d(intervalRange) x randInterval1d(intervalRange)
  def randInterval3d(intervalRange: Int): Interval3D[Int, Int, Int] =
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

  def shorten(existing: Interval1D[Int]) =
    interval(existing.start, existing.end.leftAdjacent max existing.start)

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

  // note the ?=> is important, so the given context parameter gets passed along
  type BuildFrom[F, T] = Experimental ?=> Iterable[F] => T

  def baselineFromData[T, D](
    buildFrom: BuildFrom[D, T],
    validData: Iterable[D]
  ): T =
    println("Creating baseline fixture...")
    val start = System.currentTimeMillis()
    given Experimental = baselineFeature match
      case Some(feature) => Experimental(feature)
      case None          => Experimental.none
    val baselineFixture: T = buildFrom(validData)
    println(s"Baseline fixture created, ${System.currentTimeMillis() - start} ms.")
    baselineFixture

  def featuredFromData[T, D](
    buildFrom: BuildFrom[D, T],
    validData: Iterable[D]
  ): T =
    println("Creating featured fixture...")
    val start = System.currentTimeMillis()
    given Experimental = featuredFeature match
      case Some(feature) => Experimental(feature)
      case None          => Experimental.none
    val featuredFixture = buildFrom(validData)
    println(s"Featured fixture created, ${System.currentTimeMillis() - start} ms.")
    featuredFixture

  val buildMutable1d: BuildFrom[ValidData1D[String, Int], mutable.DataIn1D[String, Int]] =
    mutable.DataIn1D[String, Int](_)
  val buildMutable2d: BuildFrom[ValidData2D[String, Int, Int], mutable.DataIn2D[String, Int, Int]] =
    mutable.DataIn2D[String, Int, Int](_)
  val buildMutable3d: BuildFrom[ValidData3D[String, Int, Int, Int], mutable.DataIn3D[String, Int, Int, Int]] =
    mutable.DataIn3D[String, Int, Int, Int](_)
  val buildImmutable1d: BuildFrom[ValidData1D[String, Int], immutable.DataIn1D[String, Int]] =
    immutable.DataIn1D[String, Int](_)
  val buildImmutable2d: BuildFrom[ValidData2D[String, Int, Int], immutable.DataIn2D[String, Int, Int]] =
    immutable.DataIn2D[String, Int, Int](_)
  val buildImmutable3d: BuildFrom[ValidData3D[String, Int, Int, Int], immutable.DataIn3D[String, Int, Int, Int]] =
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
