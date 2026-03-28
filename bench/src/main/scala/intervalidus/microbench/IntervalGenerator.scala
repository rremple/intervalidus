package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.*
import intervalidus.Interval.unbounded

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

  private def intervals1d(edgePoints: Int)(using
    rand: RandomNumbers
  )(using DomainValueLike[Int]): Seq[Interval1D[Int]] =
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
    List(first) ++ middle ++ List(last)

  def genNonIntersectingDim2Special(
    limit: Int
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim2]]] =
    val edgePoints = math.sqrt(limit).toInt
    def produceSequence() = for
      horizontal <- intervals1d(edgePoints)
      vertical <- intervals1d(edgePoints)
    yield horizontal x vertical
    Gen(Iterator.continually(produceSequence())).flatMap(is => Gen.someOf(is, atLeast = 0.5))

  def genNonIntersectingDim3Special(
    limit: Int
  )(using RandomNumbers, DomainValueLike[Int]): Gen[Iterable[Interval[Dim3]]] =
    val edgePoints = math.exp(math.log(limit) / 3.0).toInt

    def produceSequence() = for
      horizontal <- intervals1d(edgePoints)
      vertical <- intervals1d(edgePoints)
      depth <- intervals1d(edgePoints)
    yield horizontal x vertical x depth

    Gen(Iterator.continually(produceSequence())).flatMap(is => Gen.someOf(is, atLeast = 0.5))
