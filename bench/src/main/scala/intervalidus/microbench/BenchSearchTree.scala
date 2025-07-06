package intervalidus.microbench

import intervalidus.DiscreteValue.given
import intervalidus.*
import intervalidus.Interval1D.interval
import org.openjdk.jmh.annotations.*

import scala.language.implicitConversions

object BenchSearchTree extends BenchBase(baselineFeature = Some("noSearchTree"), featuredFeature = None):

  // benchmark only the remove method to see what affect noSearchTree has on updateOrRemove performance.

  //// --- Mutable Bases ---

  abstract class GenericMutableBench[D <: NonEmptyTuple: DomainLike, DimData <: mutable.Data[String, D]](
    intervalRange: Int,
    data: Vector[ValidData[String, D]],
    baselineData: => DimData,
    featuredData: => DimData,
    randDomain: () => D,
    randInterval: Int => Interval[D],
    randValue: Int => ValidData[String, D],
    randValueWithKey: ValidData[String, D] => ValidData[String, D]
  ) extends GenericBench:
    // For replace and replaceByKey, as using totally random data may have unrealistically low hit rates.
    // (vector random access should be superfast)
    val dataSize = data.size

    def useExisting(): ValidData[String, D] = data(rand.nextInt(dataSize))

    //// Baseline

    @Benchmark
    def baselineGetAt: Option[String] = baselineData.getAt(randDomain())

    @Benchmark
    def baselineIntersects: Boolean = baselineData.intersects(randInterval(intervalRange))

    @Benchmark
    def baselineGetIntersecting: Iterable[ValidData[String, D]] =
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
      baselineData.replaceByKey(existing.interval.start, randValueWithKey(existing))

    //// Featured

    @Benchmark
    def featuredGetAt: Option[String] = featuredData.getAt(randDomain())

    @Benchmark
    def featuredIntersects: Boolean = featuredData.intersects(randInterval(intervalRange))

    @Benchmark
    def featuredGetIntersecting: Iterable[ValidData[String, D]] =
      featuredData.getIntersecting(randInterval(intervalRange))

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
      featuredData.replaceByKey(existing.interval.start, randValueWithKey(existing))

  abstract class GenericMutable1dBench(
    intervalRange: Int,
    data: Vector[ValidData.In1D[String, Int]],
    baselineData: => mutable.Data.In1D[String, Int],
    featuredData: => mutable.Data.In1D[String, Int]
  ) extends GenericMutableBench[
      Domain.In1D[Int],
      mutable.Data.In1D[String, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain1d, randInterval1d, randValue1d, randValue1dWithKey):
    val fullRangeInterval = interval(fullRangeMin, fullRangeMax)
    val hits = data.map(_.interval)
    val hitsSize = hits.size
    val gaps = Interval.complement(hits).flatMap(_.intersectionWith(fullRangeInterval)).toVector
    val gapsSize = gaps.size
    println(s"hit intervals=$hitsSize; gap intervals=$gapsSize")

    def randDomainInInterval1d(i: Interval.In1D[Int]): Domain1D[Int] = i.headInterval1D[Int] match
      case Interval1D(Domain1D.Point(start), Domain1D.Point(end)) => start + rand.nextInt(end - start + 1)
      case unexpected => throw new Exception(s"unexpected result: $unexpected")

    def randHit(): Domain1D[Int] = randDomainInInterval1d(hits(rand.nextInt(hitsSize)))

    def randMiss(): Domain1D[Int] = randDomainInInterval1d(gaps(rand.nextInt(gapsSize)))

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
    data: Vector[ValidData.In2D[String, Int, Int]],
    baselineData: => mutable.Data.In2D[String, Int, Int],
    featuredData: => mutable.Data.In2D[String, Int, Int]
  ) extends GenericMutableBench[
      Domain.In2D[Int, Int],
      mutable.Data.In2D[String, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain2d, randInterval2d, randValue2d, randValue2dWithKey):

    @Benchmark
    def baselineGetByHeadIndex: mutable.Data.In1D[String, Int] = baselineData.getByHeadIndex(randDomain1d())

  abstract class GenericMutable3dBench(
    intervalRange: Int,
    data: Vector[ValidData.In3D[String, Int, Int, Int]],
    baselineData: => mutable.Data.In3D[String, Int, Int, Int],
    featuredData: => mutable.Data.In3D[String, Int, Int, Int]
  ) extends GenericMutableBench[
      Domain.In3D[Int, Int, Int],
      mutable.Data.In3D[String, Int, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain3d, randInterval3d, randValue3d, randValue3dWithKey):

    @Benchmark
    def baselineGetByHeadIndex: mutable.Data.In2D[String, Int, Int] =
      baselineData.getByHeadIndex(randDomain1d())

    @Benchmark
    def featuredGetByHeadIndex: mutable.Data.In2D[String, Int, Int] =
      featuredData.getByHeadIndex(randDomain1d())

  //// --- Immutable Bases ---

  abstract class GenericImmutableBench[
    D <: NonEmptyTuple: DomainLike,
    DimData <: immutable.Data[String, D]
  ](
    intervalRange: Int,
    data: Vector[ValidData[String, D]],
    baselineData: => DimData,
    featuredData: => DimData,
    randDomain: () => D,
    randInterval: Int => Interval[D],
    randValue: Int => ValidData[String, D],
    randValueWithKey: ValidData[String, D] => ValidData[String, D]
  ) extends GenericBench:
    // For replace and replaceByKey, as using totally random data may have unrealistically low hit rates.
    // (vector random access should be superfast)
    val dataSize = data.size

    def useExisting(): ValidData[String, D] = data(rand.nextInt(dataSize))

    //// Baseline

    @Benchmark
    def baselineGetAt: Option[String] = baselineData.getAt(randDomain())

    @Benchmark
    def baselineIntersects: Boolean = baselineData.intersects(randInterval(intervalRange))

    @Benchmark
    def baselineGetIntersecting: Iterable[ValidData[String, D]] =
      baselineData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def baselineSet: immutable.Data[String, D] = baselineData.set(randValue(intervalRange))

    @Benchmark
    def baselineSetIfNoConflict: Option[immutable.Data[String, D]] =
      baselineData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def baselineUpdate: immutable.Data[String, D] = baselineData.update(randValue(intervalRange))

    // The problem with the remove benchmark was that, by using standard intervals in 1D, it winds up removing
    // all the data in warmup. So the benchmark winds up being unrealistically fast because it is running against
    // an empty structure. Here we limit the size of the intervals removed to allow for more data to be leftover.
    @Benchmark
    def baselineRemove: immutable.Data[String, D] = baselineData.remove(randInterval(1)) // tiny interval

    @Benchmark
    def baselineReplace: immutable.Data[String, D] =
      val existing = useExisting()
      baselineData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def baselineReplaceByKey: immutable.Data[String, D] =
      val existing = useExisting()
      baselineData.replaceByKey(existing.interval.start, randValueWithKey(existing))

    //// Featured

    @Benchmark
    def featuredGetAt: Option[String] = featuredData.getAt(randDomain())

    @Benchmark
    def featuredIntersects: Boolean = featuredData.intersects(randInterval(intervalRange))

    @Benchmark
    def featuredGetIntersecting: Iterable[ValidData[String, D]] =
      featuredData.getIntersecting(randInterval(intervalRange))

    @Benchmark
    def featuredSet: immutable.Data[String, D] = featuredData.set(randValue(intervalRange))

    @Benchmark
    def featuredSetIfNoConflict: Option[immutable.Data[String, D]] =
      featuredData.setIfNoConflict(randValue(intervalRange))

    @Benchmark
    def featuredUpdate: immutable.Data[String, D] = featuredData.update(randValue(intervalRange))

    @Benchmark
    def featuredRemove: immutable.Data[String, D] = featuredData.remove(randInterval(1))

    @Benchmark
    def featuredReplace: immutable.Data[String, D] =
      val existing = useExisting()
      featuredData.replace(existing, randValueWithKey(existing))

    @Benchmark
    def featuredReplaceByKey: immutable.Data[String, D] =
      val existing = useExisting()
      featuredData.replaceByKey(existing.interval.start, randValueWithKey(existing))

  abstract class GenericImmutable1dBench(
    intervalRange: Int,
    data: Vector[ValidData.In1D[String, Int]],
    baselineData: => immutable.Data.In1D[String, Int],
    featuredData: => immutable.Data.In1D[String, Int]
  ) extends GenericImmutableBench[
      Domain.In1D[Int],
      immutable.Data.In1D[String, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain1d, randInterval1d, randValue1d, randValue1dWithKey):
    val fullRangeInterval = interval(fullRangeMin, fullRangeMax)
    val hits = data.map(_.interval)
    val hitsSize = hits.size
    val gaps = Interval.complement(hits).flatMap(_.intersectionWith(fullRangeInterval)).toVector
    val gapsSize = gaps.size
    println(s"hit intervals=$hitsSize; gap intervals=$gapsSize")

    def randDomainInInterval1d(i: Interval.In1D[Int]): Domain1D[Int] = i.headInterval1D[Int] match
      case Interval1D(Domain1D.Point(start), Domain1D.Point(end)) => start + rand.nextInt(end - start + 1)
      case unexpected => throw new Exception(s"unexpected result: $unexpected")

    def randHit(): Domain1D[Int] = randDomainInInterval1d(hits(rand.nextInt(hitsSize)))

    def randMiss(): Domain1D[Int] = randDomainInInterval1d(gaps(rand.nextInt(gapsSize)))

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
    data: Vector[ValidData.In2D[String, Int, Int]],
    baselineData: => immutable.Data.In2D[String, Int, Int],
    featuredData: => immutable.Data.In2D[String, Int, Int]
  ) extends GenericImmutableBench[
      Domain.In2D[Int, Int],
      immutable.Data.In2D[String, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain2d, randInterval2d, randValue2d, randValue2dWithKey):

    @Benchmark
    def baselineGetByHeadIndex: immutable.Data.In1D[String, Int] =
      baselineData.getByHeadIndex(randDomain1d())

    @Benchmark
    def featuredGetByHeadIndex: immutable.Data.In1D[String, Int] =
      featuredData.getByHeadIndex(randDomain1d())

  abstract class GenericImmutable3dBench(
    intervalRange: Int,
    data: Vector[ValidData.In3D[String, Int, Int, Int]],
    baselineData: => immutable.Data.In3D[String, Int, Int, Int],
    featuredData: => immutable.Data.In3D[String, Int, Int, Int]
  ) extends GenericImmutableBench[
      Domain.In3D[Int, Int, Int],
      immutable.Data.In3D[String, Int, Int, Int]
    ](intervalRange, data, baselineData, featuredData, randDomain3d, randInterval3d, randValue3d, randValue3dWithKey):

    @Benchmark
    def baselineGetByHeadIndex: immutable.Data.In2D[String, Int, Int] =
      baselineData.getByHeadIndex[Int](randDomain1d())

    @Benchmark
    def featuredGetByHeadIndex: immutable.Data.In2D[String, Int, Int] =
      featuredData.getByHeadIndex(randDomain1d())

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
