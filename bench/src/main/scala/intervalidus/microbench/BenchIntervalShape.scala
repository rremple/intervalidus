package intervalidus.microbench

import intervalidus.*
import IntervalShapeGenerator.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.language.implicitConversions

object BenchIntervalShape:
  private val constantSeed: Long = 42 // Random().nextLong() //

  // --- --- --- --- --- --- --- Abstract State --- --- --- --- --- --- ---  ---

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkState[D <: NonEmptyTuple](
    testDataGen: () => RandomNumbers ?=> Gen[IntervalShape[D]]
  )(using domainLike: DomainLike[D]):
    println(s"\nInitializing ${domainLike.arity}d state, seed=$constantSeed...\n")
    val r = scala.util.Random()
    import scala.compiletime.uninitialized
    private var testDataIterator: Iterator[IntervalShape[D]] = uninitialized
    var aShape: IntervalShape[D] = uninitialized
    var bShape: IntervalShape[D] = uninitialized
    var aMonoid: immutable.DataMonoid[Unit, D] = uninitialized
    var bMonoid: immutable.DataMonoid[Unit, D] = uninitialized
    var i: Interval[D] = uninitialized
    // var valuesUnit: Iterable[Unit] = uninitialized
    // var valuesRandom: Iterable[Int] = uninitialized
    // var unique: Iterable[Interval[D]] = uninitialized

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
      i = bShape.allIntervals.head

  //  valuesUnit = (a.getAll.map(_.value) ++ b.getAll.map(_.value)).flatMap: _ =>
  //    Iterable.fill(1000)(())
  //  valuesRandom = valuesUnit.map(_ => r.nextInt(valuesUnit.size))

  // println(s"setUpInvocation...")

  //  println(s"/ ${a.size}/ ${a.getAll.headOption.map(_.toCodeLikeString)} /")
  //  println(s"  ${b.size}/ ${b.getAll.headOption.map(_.toCodeLikeString)} /")

  // --- --- --- --- --- --- --- Concrete State --- --- --- --- --- --- ---  ---

  // TODO: someday I can run discrete and continuous together, but not today...
  import DiscreteValue.IntDiscreteValue
  // import ContinuousValue.IntContinuousValue
  class BenchmarkState1d extends GenericBenchmarkState(() => genDim1)
  class BenchmarkState2d extends GenericBenchmarkState(() => genDim2)
  class BenchmarkState3d extends GenericBenchmarkState(() => genDim3)
  class BenchmarkState4d extends GenericBenchmarkState(() => genDim4)
  class BenchmarkState5d extends GenericBenchmarkState(() => genDim5)

//  def limit(c: Int, dim: Int, leafSize: Int = 256): Int = math.pow(2.0, dim).toInt * leafSize * c
//
//  class BenchmarkState2dSpecial extends GenericBenchmarkState(() => genDim2Special(10000))
//  class BenchmarkState3dSpecial extends GenericBenchmarkState(() => genDim2Special(10000))

//  class BenchmarkState2dSpecialA extends GenericBenchmarkState(() => genDim2Special(limit(160, 2)))
//  class BenchmarkState2dSpecialB extends GenericBenchmarkState(() => genDim2Special(limit(192, 2)))
//  class BenchmarkState2dSpecialC extends GenericBenchmarkState(() => genDim2Special(limit(224, 2)))
//  class BenchmarkState2dSpecialD extends GenericBenchmarkState(() => genDim2Special(limit(256, 2)))
//  class BenchmarkState2dSpecialE extends GenericBenchmarkState(() => genDim2Special(limit(288, 2)))
//  class BenchmarkState2dSpecialF extends GenericBenchmarkState(() => genDim2Special(limit(320, 2)))
//
//  class BenchmarkState3dSpecialA extends GenericBenchmarkState(() => genDim2Special(limit(20, 3)))
//  class BenchmarkState3dSpecialB extends GenericBenchmarkState(() => genDim2Special(limit(30, 3)))
//  class BenchmarkState3dSpecialC extends GenericBenchmarkState(() => genDim2Special(limit(40, 3)))
//  class BenchmarkState3dSpecialD extends GenericBenchmarkState(() => genDim3Special(limit(128, 3)))
//  class BenchmarkState3dSpecialE extends GenericBenchmarkState(() => genDim2Special(limit(80, 3)))

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
  abstract class GenericBench // :
//    extension [V: Monoid, D <: NonEmptyTuple: DomainLike](data: Iterable[ValidData[V, D]])
//      def asShape: immutable.Shape[V, D] = immutable.Shape[V, D](data)
//
//    def zipDataUnionF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      val data = s.aShape.zipAllDataGeneric(
//        s.bShape,
//        whenBothMissing = None,
//        whenOnlyThis = Some(_),
//        whenOnlyThat = Some(_),
//        whenBothPresent = (thisValue, thatValue) => Some(thisValue combineWith thatValue)
//      )
//      immutable.Shape(data).compressInPlace(())
//
//    def mergeUnionF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      s.aShape.merge(s.bShape, _ combineWith _)
//    // s.aShape.intersection(s.bShape)
//
//    def zipDataDifferenceF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      val data = s.aShape.zipAllDataGeneric(
//        s.bShape,
//        whenBothMissing = None,
//        whenOnlyThis = Some(_),
//        whenOnlyThat = _ => None,
//        whenBothPresent = (_, _) => None
//      )
//      immutable.Shape(data)
//
//    def removeManyDifferenceF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      s.aShape.removeMany(s.bShape.allIntervals)
//
//    def zipDataFullIntersectionF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume(s.aShape.zipData(s.bShape, _ combineWith _))
//
//    def zipDataTreeIntersectionF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume(s.aShape.zipDataTree(s.bShape, _ combineWith _))
//
//    // def lookupF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//    //  s.unique.foreach: i =>
//    //      val aRes = s.aShape.getIntersecting(i)
//    //      val bRes = s.bShape.getIntersecting(i)
//
//    def uniqueF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      Interval.uniqueIntervals(s.aShape.allIntervals ++ s.bShape.allIntervals)
//
//    def zipF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](s: S): Unit =
//      s.aShape.zip(s.bShape)
//
//    def existingCompressF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
//      b.consume(s.aShape.compressInPlace(()))
//
////    def altCompressF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S ): Unit =
////      b.consume(s.aShape.compressInPlaceAlt(()))
//
//    def fromDataComplementF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S): Unit =
//      b.consume(s.aShape.complement.toShape)
//
//    def fromDomainComplementF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](b: Blackhole, s: S)
  //    : Unit =
//      b.consume(s.aShape.domainComplement.toShape)
//
//    def zipDataSymmetricF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume:
//      s.aShape.symmetricDifferenceData(s.bShape).asShape
//
//    def diffUnionSymmetricF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume:
//      (s.aShape \ s.bShape) ∪ (s.bShape \ s.aShape)
//
//    def diffIntersectionSymmetricF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume:
//      (s.aShape ∪ s.bShape) \ (s.aShape ∩ s.bShape)
//
//    def diffRawDataSymmetricF[D <: NonEmptyTuple: DomainLike, S <: GenericBenchmarkState[D]](
//      b: Blackhole,
//      s: S
//    ): Unit = b.consume:
//      val result = s.aShape.copy
//      val filledValues = s.bShape.getAll.map: d =>
//        result.fillInPlaceNoCompress(d)
//        d.value
//      val removedValues = s.aShape.zipData(s.bShape, (_, _) => ()).flatMap: d =>
//        result.updateOrRemoveNoCompress(d.interval, _ => None)
//      (filledValues.iterator ++ removedValues).distinct.foreach(result.compressInPlace)
//
//
//      val intersectingIntervals: Iterable[Interval[D]] = for
//        a <- s.aShape.getAll
//        b <- s.bShape.getIntersecting(a.interval)
//      yield b.interval ∩ a.interval match {
//        case Some(interval) => interval
//        case None           => throw Exception("Unexpectedly missing intersection")
//      }
//      (s.aShape ∪ s.bShape).removeMany(intersectingIntervals)

  // --- --- --- --- --- --- --- Concrete Bench --- --- --- --- --- --- ---  ---

  class BenchmarkBasline1d extends GenericBench:
    private type S = BenchmarkState1d
    @Benchmark def unionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∪ s.bShape
    @Benchmark def intersectionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.bShape
    @Benchmark def complementShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape.c
    @Benchmark def diffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape \ s.bShape
    @Benchmark def symDiffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape △ s.bShape
    @Benchmark def clipShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.i
    @Benchmark def subsetShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ⊆ s.bShape
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∪ s.bMonoid
    @Benchmark def intersectionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.bMonoid
    @Benchmark def complementMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid.c
    @Benchmark def diffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid \ s.bMonoid
    @Benchmark def symDiffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid △ s.bMonoid
    @Benchmark def clipMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.i
    @Benchmark def subsetMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ⊆ s.bMonoid

  class BenchmarkBasline2d extends GenericBench:
    private type S = BenchmarkState2d
    @Benchmark def unionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∪ s.bShape
    @Benchmark def intersectionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.bShape
    @Benchmark def complementShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape.c
    @Benchmark def diffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape \ s.bShape
    @Benchmark def symDiffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape △ s.bShape
    @Benchmark def clipShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.i
    @Benchmark def subsetShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ⊆ s.bShape
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∪ s.bMonoid
    @Benchmark def intersectionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.bMonoid
    @Benchmark def complementMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid.c
    @Benchmark def diffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid \ s.bMonoid
    @Benchmark def symDiffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid △ s.bMonoid
    @Benchmark def clipMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.i
    @Benchmark def subsetMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ⊆ s.bMonoid

  class BenchmarkBasline3d extends GenericBench:
    private type S = BenchmarkState3d
    @Benchmark def unionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∪ s.bShape
    @Benchmark def intersectionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.bShape
    @Benchmark def complementShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape.c
    @Benchmark def diffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape \ s.bShape
    @Benchmark def symDiffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape △ s.bShape
    @Benchmark def clipShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.i
    @Benchmark def subsetShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ⊆ s.bShape
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∪ s.bMonoid
    @Benchmark def intersectionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.bMonoid
    @Benchmark def complementMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid.c
    @Benchmark def diffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid \ s.bMonoid
    @Benchmark def symDiffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid △ s.bMonoid
    @Benchmark def clipMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.i
    @Benchmark def subsetMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ⊆ s.bMonoid

  class BenchmarkBasline4d extends GenericBench:
    private type S = BenchmarkState4d
    @Benchmark def unionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∪ s.bShape
    @Benchmark def intersectionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.bShape
    @Benchmark def complementShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape.c
    @Benchmark def diffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape \ s.bShape
    @Benchmark def symDiffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape △ s.bShape
    @Benchmark def clipShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.i
    @Benchmark def subsetShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ⊆ s.bShape
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∪ s.bMonoid
    @Benchmark def intersectionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.bMonoid
    @Benchmark def complementMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid.c
    @Benchmark def diffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid \ s.bMonoid
    @Benchmark def symDiffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid △ s.bMonoid
    @Benchmark def clipMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.i
    @Benchmark def subsetMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ⊆ s.bMonoid

  class BenchmarkBasline5d extends GenericBench:
    private type S = BenchmarkState5d
    @Benchmark def unionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∪ s.bShape
    @Benchmark def intersectionShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.bShape
    @Benchmark def complementShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape.c
    @Benchmark def diffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape \ s.bShape
    @Benchmark def symDiffShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape △ s.bShape
    @Benchmark def clipShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ∩ s.i
    @Benchmark def subsetShape(b: Blackhole, s: S): Unit = b.consume:
      s.aShape ⊆ s.bShape
    @Benchmark def unionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∪ s.bMonoid
    @Benchmark def intersectionMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.bMonoid
    @Benchmark def complementMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid.c
    @Benchmark def diffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid \ s.bMonoid
    @Benchmark def symDiffMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid △ s.bMonoid
    @Benchmark def clipMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ∩ s.i
    @Benchmark def subsetMonoid(b: Blackhole, s: S): Unit = b.consume:
      s.aMonoid ⊆ s.bMonoid

//  class Benchmark1d extends GenericBench:
//    private type S = BenchmarkState1d
//    @Benchmark def zipDataSymmetric(b: Blackhole, s: S): Unit = zipDataSymmetricF(b, s)
//    @Benchmark def diffUnionSymmetric(b: Blackhole, s: S): Unit = diffUnionSymmetricF(b, s)
//    @Benchmark def diffIntersectionSymmetric(b: Blackhole, s: S): Unit = diffIntersectionSymmetricF(b, s)
//
//  class Benchmark2d extends GenericBench:
//    private type S = BenchmarkState2d
//    @Benchmark def zipDataSymmetric(b: Blackhole, s: S): Unit = zipDataSymmetricF(b, s)
//    @Benchmark def diffUnionSymmetric(b: Blackhole, s: S): Unit = diffUnionSymmetricF(b, s)
//    @Benchmark def diffIntersectionSymmetric(b: Blackhole, s: S): Unit = diffIntersectionSymmetricF(b, s)
//    @Benchmark def diffRawDataSymmetric(b: Blackhole, s: S): Unit = diffRawDataSymmetricF(b, s)
//
//  class Benchmark3d extends GenericBench:
//    private type S = BenchmarkState3d
//    @Benchmark def zipDataSymmetric(b: Blackhole, s: S): Unit = zipDataSymmetricF(b, s)
//    @Benchmark def diffUnionSymmetric(b: Blackhole, s: S): Unit = diffUnionSymmetricF(b, s)
//    @Benchmark def diffIntersectionSymmetric(b: Blackhole, s: S): Unit = diffIntersectionSymmetricF(b, s)
//    @Benchmark def diffRawDataSymmetric(b: Blackhole, s: S): Unit = diffRawDataSymmetricF(b, s)
//
//
//  class Benchmark4d extends GenericBench:
//    private type S = BenchmarkState4d
//    @Benchmark def zipDataSymmetric(b: Blackhole, s: S): Unit = zipDataSymmetricF(b, s)
//    @Benchmark def diffUnionSymmetric(b: Blackhole, s: S): Unit = diffUnionSymmetricF(b, s)
//    @Benchmark def diffIntersectionSymmetric(b: Blackhole, s: S): Unit = diffIntersectionSymmetricF(b, s)
//    @Benchmark def diffRawDataSymmetric(b: Blackhole, s: S): Unit = diffRawDataSymmetricF(b, s)
//
//  class Benchmark5d extends GenericBench:
//    private type S = BenchmarkState5d
//    @Benchmark def zipDataSymmetric(b: Blackhole, s: S): Unit = zipDataSymmetricF(b, s)
//    @Benchmark def diffUnionSymmetric(b: Blackhole, s: S): Unit = diffUnionSymmetricF(b, s)
//    @Benchmark def diffIntersectionSymmetric(b: Blackhole, s: S): Unit = diffIntersectionSymmetricF(b, s)
//    @Benchmark def diffRawDataSymmetric(b: Blackhole, s: S): Unit = diffRawDataSymmetricF(b, s)

//  class Benchmark2dSpecial extends GenericBench:
//    private type S = BenchmarkState2dSpecial
//    @Benchmark def existingCompress(b: Blackhole, s: S): Unit = existingCompressF(b, s)
//    @Benchmark def altCompress(b: Blackhole, s: S): Unit = altCompressF(b, s)
//
//  class Benchmark3dSpecial extends GenericBench:
//    private type S = BenchmarkState3dSpecial
//    @Benchmark def existingCompress(b: Blackhole, s: S): Unit = existingCompressF(b, s)
//    @Benchmark def altCompress(b: Blackhole, s: S): Unit = altCompressF(b, s)

//    @Benchmark def listUnitDistinct(b: Blackhole, s: S): Unit = s.valuesUnit.toList.distinct.foreach(b.consume)
//    @Benchmark def iteratorUnitDistinct(b: Blackhole, s: S): Unit = s.valuesUnit.iterator.distinct.foreach(b
//    .consume)
//    @Benchmark def setUnitDistinct(b: Blackhole, s: S): Unit = s.valuesUnit.toSet.foreach(b.consume)
//    @Benchmark def listRandomDistinct(b: Blackhole, s: S): Unit = s.valuesRandom.toList.distinct.foreach(b.consume)
//    @Benchmark def iteratorRandomDistinct(b: Blackhole, s: S): Unit = s.valuesRandom.iterator.distinct.foreach(b
//    .consume)
//    @Benchmark def setRandomDistinct(b: Blackhole, s: S): Unit = s.valuesRandom.toSet.foreach(b.consume)

//  class Benchmark2dSpecialA extends GenericBench:
//    private type S = BenchmarkState2dSpecialA
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark2dSpecialB extends GenericBench:
//    private type S = BenchmarkState2dSpecialB
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark2dSpecialC extends GenericBench:
//    private type S = BenchmarkState2dSpecialC
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark2dSpecialD extends GenericBench:
//    private type S = BenchmarkState2dSpecialD
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark2dSpecialE extends GenericBench:
//    private type S = BenchmarkState2dSpecialE
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark2dSpecialF extends GenericBench:
//    private type S = BenchmarkState2dSpecialF
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark3dSpecialA extends GenericBench:
//    private type S = BenchmarkState3dSpecialA
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark3dSpecialB extends GenericBench:
//    private type S = BenchmarkState3dSpecialB
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark3dSpecialC extends GenericBench:
//    private type S = BenchmarkState3dSpecialC
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark3dSpecialD extends GenericBench:
//    private type S = BenchmarkState3dSpecialD
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)
//
//  class Benchmark3dSpecialE extends GenericBench:
//    private type S = BenchmarkState3dSpecialE
//    @Benchmark def zipDataFullIntersection(b: Blackhole, s: S): Unit = zipDataFullIntersectionF(b, s)
//    @Benchmark def zipDataTreeIntersection(b: Blackhole, s: S): Unit = zipDataTreeIntersectionF(b, s)

//class Benchmark1d extends GenericBench:
//  private type S = BenchmarkState1d
//
//  @Benchmark def zipDataDifference(b: Blackhole, s: S): Unit = b.consume(zipDataDifferenceF(s))
//
//  @Benchmark def removeManyDifference(b: Blackhole, s: S): Unit = b.consume(removeManyDifferenceF(s))
//
//class Benchmark2d extends GenericBench:
//  private type S = BenchmarkState2d
//
//  @Benchmark def zipDataDifference(b: Blackhole, s: S): Unit = b.consume(zipDataDifferenceF(s))
//
//  @Benchmark def removeManyDifference(b: Blackhole, s: S): Unit = b.consume(removeManyDifferenceF(s))
//
//class Benchmark3d extends GenericBench:
//  private type S = BenchmarkState3d
//
//  @Benchmark def zipDataDifference(b: Blackhole, s: S): Unit = b.consume(zipDataDifferenceF(s))
//
//  @Benchmark def removeManyDifference(b: Blackhole, s: S): Unit = b.consume(removeManyDifferenceF(s))
//
//class Benchmark4d extends GenericBench:
//  private type S = BenchmarkState4d
//
//  @Benchmark def zipDataDifference(b: Blackhole, s: S): Unit = b.consume(zipDataDifferenceF(s))
//
//  @Benchmark def removeManyDifference(b: Blackhole, s: S): Unit = b.consume(removeManyDifferenceF(s))
