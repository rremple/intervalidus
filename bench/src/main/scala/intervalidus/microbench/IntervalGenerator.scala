package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.*
import intervalidus.Interval.unbounded

import java.util
import scala.language.implicitConversions

// Generates intervals of any dimension
object IntervalGenerator:

  private def gen[D <: NonEmptyTuple: DomainLike](
    genStart: Gen[D],
    genEnd: D => Gen[D]
  ): Gen[Interval[D]] = for
    start <- genStart
    end <- genEnd(start)
  yield Interval(start, end)

  def genDim1(using RandomNumbers, DomainValueLike[Int]): Gen[Interval[Dim1]] =
    gen(genStartDim1, genEndDim1)
  def genDim2(using RandomNumbers, DomainValueLike[Int]): Gen[Interval[Dim2]] =
    gen(genStartDim2, genEndDim2)
  def genDim3(using RandomNumbers, DomainValueLike[Int]): Gen[Interval[Dim3]] =
    gen(genStartDim3, genEndDim3)
  def genDim4(using RandomNumbers, DomainValueLike[Int]): Gen[Interval[Dim4]] =
    gen(genStartDim4, genEndDim4)
  def genDim5(using RandomNumbers, DomainValueLike[Int]): Gen[Interval[Dim5]] =
    gen(genStartDim5, genEndDim5)

  private def genNonIntersecting[D <: NonEmptyTuple: DomainLike](
    gen: Gen[Interval[D]],
    minInitialSize: Int,
    maxInitialSize: Int,
    minFinalSize: Int = 290,
    maxFinalSize: Int = 310,
    keepAtLeast: Double = 0.5
  )(using RandomNumbers): Gen[Iterable[Interval[D]]] = for
    size <- Gen.choose(minInitialSize, maxInitialSize)
    raw <- Gen.listOfN(size, gen)
    shards = Interval.uniqueIntervals(unbounded +: raw)
    subset <- Gen.someOf(shards, keepAtLeast)
    finalSize <- Gen.choose(minFinalSize, maxFinalSize)
  yield subset.take(finalSize)

  def genNonIntersectingDim1(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim1]]] =
    genNonIntersecting(genDim1, 290, 310)
  def genNonIntersectingDim2(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim2]]] =
    genNonIntersecting(genDim2, 20, 60)
  def genNonIntersectingDim3(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim3]]] =
    genNonIntersecting(genDim3, 4, 6)
  def genNonIntersectingDim4(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim4]]] =
    genNonIntersecting(genDim4, 3, 5)
  def genNonIntersectingDim5(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim5]]] =
    genNonIntersecting(genDim5, 3, 4)

  def genNonIntersectingDim2Special(
    limit: Int
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim2]]] =
    val edgePoints = math.sqrt(limit).toInt
    val ops = summon[IntDomainTuple[Dim2]]
    def produceSequence(): Seq[Interval[Dim2]] = ops.shattered(edgePoints).map(_._1).toSeq // ignore Euclidean distances
    Gen(Iterator.continually(produceSequence())).flatMap(is => Gen.someOf(is, atLeast = 0.5))

  def genNonIntersectingDim3Special(
    limit: Int
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim3]]] =
    val edgePoints = math.exp(math.log(limit) / 3.0).toInt
    val ops = summon[IntDomainTuple[Dim3]]
    def produceSequence(): Seq[Interval[Dim3]] = ops.shattered(edgePoints).map(_._1).toSeq // ignore Euclidean distances
    Gen(Iterator.continually(produceSequence())).flatMap(is => Gen.someOf(is, atLeast = 0.5))

  trait IntDomainTuple[D <: NonEmptyTuple]:
    def arity: Int

    def fullDomain: Interval[D]

    // returns tuple of interval and the squared Euclidean distance from origin of the start and end points
    def shattered(edgePoints: Int)(using RandomNumbers): Iterator[(Interval[D], Double, Double)]

  object IntDomainTuple:
    private type OneDimDomain = Domain1D[Int] *: EmptyTuple
    private type MultiDimDomain[DomainTail <: NonEmptyTuple] = Domain1D[Int] *: DomainTail

    private def intervals1d(edgePoints: Int)(using
      rand: RandomNumbers
    )(using DomainValueLike[Int]): Iterator[Interval1D[Int]] =
      val sizeMax = intRange.size * 2 / edgePoints - 1
      val lattice = (1 to edgePoints).foldLeft(List.empty[Int]): (acc, _) =>
        val size = rand.int(1, sizeMax)
        acc match
          case Nil                                                  => List(intRange.end - size)
          case prior :: tail if intRange.contains(prior - size - 1) => (prior - size) :: prior :: tail
          case cannotAddAnymore                                     => cannotAddAnymore

      val first = Interval1D.intervalToBefore(lattice.head)
      val last = Interval1D.intervalFrom(lattice.last)
      val middle = lattice
        .sliding(2)
        .map:
          case start :: nextStart :: _ => Interval1D.intervalFrom(start).toBefore(nextStart)
          case theUnexpected           => throw Exception(s"Didn't expect $theUnexpected")
      Iterator(first) ++ middle ++ Iterator(last)

    // Base case - one dimension
    given IntDomainTupleOneDimOps(using DomainValueLike[Int]): IntDomainTuple[OneDimDomain] with
      inline override def arity: Int = 1

      inline def fullDomain: Interval[OneDimDomain] = Interval1D.interval(intRange.start, intRange.end).tupled

      inline def shattered(edgePoints: Int)(using RandomNumbers): Iterator[(Interval[OneDimDomain], Double, Double)] =
        intervals1d(edgePoints).map: i =>
          (i.tupled, math.pow(i.start.orderedHashFixed, 2), math.pow(i.end.orderedHashFixed, 2))

    // Inductive case - multiple dimensions
    given IntDomainTupleMultiDimOps[DomainTail <: NonEmptyTuple](using
      applyToTail: IntDomainTuple[DomainTail]
    )(using
      DomainLike[MultiDimDomain[DomainTail]],
      DomainValueLike[Int]
    ): IntDomainTuple[MultiDimDomain[DomainTail]] with
      inline override def arity: Int = 1 + applyToTail.arity

      inline def fullDomain: Interval[MultiDimDomain[DomainTail]] =
        applyToTail.fullDomain.withHead(Interval1D.interval(intRange.start, intRange.end))

      inline def shattered(
        edgePoints: Int
      )(using RandomNumbers): Iterator[(Interval[MultiDimDomain[DomainTail]], Double, Double)] =
        for
          (head1d, headStartMetric, headEndMetric) <- intervals1d(edgePoints).map: i =>
            (i, math.pow(i.start.orderedHashFixed, 2), math.pow(i.end.orderedHashFixed, 2))
          (tail1d, tailStartMetric, tailEndMetric) <- applyToTail.shattered(edgePoints)
        yield (tail1d.withHead(head1d), headStartMetric + tailStartMetric, headEndMetric + tailEndMetric)

  // c*2^(d*k)
  // so intervalsFor[d](c, k -1) = c*2^(d*(k-1)) = c*2^(d*k-d) = (c*2^(d*k))/(2^d) = intervalsFor[d](c, k)/(2^d)
  def intervalsFor[D <: NonEmptyTuple: DomainLike](leafCapacity: Int, treeDepth: Int)(using
    ops: IntDomainTuple[D]
  ): Int = leafCapacity * math.pow(2, ops.arity * treeDepth).toInt

  def edgePointsFor[D <: NonEmptyTuple: DomainLike](leafCapacity: Int, treeDepth: Int)(using
    ops: IntDomainTuple[D]
  ): Int = (math.exp(math.log(leafCapacity) / ops.arity) * math.pow(2, treeDepth)).round.toInt

  def fullDomain[D <: NonEmptyTuple: DomainLike](using ops: IntDomainTuple[D]): Interval[D] = ops.fullDomain

  def arity[D <: NonEmptyTuple: DomainLike](using ops: IntDomainTuple[D]): Int = ops.arity

  def genFromOrigin[D <: NonEmptyTuple: DomainLike](edgePoints: Int)(using
    ops: IntDomainTuple[D]
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Array[Interval[D]]] =
    def produceSequence() =
      // println("shattering...")
      val arr = ops
        .shattered(edgePoints)
        .map((i, startMetric, endMetric) => (i, math.max(startMetric, endMetric)))
        .toArray
      // println("sorting...")
      util.Arrays.parallelSort(arr, (a, b) => java.lang.Double.compare(a._2, b._2))
      // println("removing metric...")
      arr.map(_._1)
    Gen(Iterator.continually(produceSequence()))

  def genFromOrigin[D <: NonEmptyTuple: DomainLike](leafCapacity: Int, treeDepth: Int)(using
    ops: IntDomainTuple[D]
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Array[Interval[D]]] =
    genFromOrigin[D](edgePointsFor[D](leafCapacity, treeDepth))
