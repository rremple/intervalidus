package intervalidus.laws

import DomainGenerator.*
import intervalidus.Interval.unbounded
import intervalidus.{DomainLike, DomainValueLike, Interval}
import org.scalacheck.Gen

// Generates intervals of any dimension
object IntervalGenerator:

  private def gen[D <: NonEmptyTuple: DomainLike](
    genStart: Gen[D],
    genEnd: D => Gen[D]
  ): Gen[Interval[D]] = for
    start <- genStart
    end <- genEnd(start)
  yield Interval(start, end)

  def genDim1(using DomainValueLike[Int]): Gen[Interval[Dim1]] =
    gen(genStartDim1, genEndDim1)
  def genDim2(using DomainValueLike[Int]): Gen[Interval[Dim2]] =
    gen(genStartDim2, genEndDim2)
  def genDim3(using DomainValueLike[Int]): Gen[Interval[Dim3]] =
    gen(genStartDim3, genEndDim3)
  def genDim4(using DomainValueLike[Int]): Gen[Interval[Dim4]] =
    gen(genStartDim4, genEndDim4)

  private def genNonIntersecting[D <: NonEmptyTuple: DomainLike](
    minSize: Int,
    maxSize: Int,
    gen: Gen[Interval[D]]
  ): Gen[Iterable[Interval[D]]] = for
    size <- Gen.choose(minSize, maxSize)
    raw <- Gen.listOfN(size, gen)
    shards = Interval.uniqueIntervals(unbounded +: raw)
    subset <- Gen.someOf(shards)
    limitSize <- Gen.choose(100, 200)
  yield subset.take(limitSize)

  def genNonIntersectingDim1(using DomainValueLike[Int]): Gen[Iterable[Interval[Dim1]]] =
    genNonIntersecting(100, 200, genDim1)
  def genNonIntersectingDim2(using DomainValueLike[Int]): Gen[Iterable[Interval[Dim2]]] =
    genNonIntersecting(10, 30, genDim2)
  def genNonIntersectingDim3(using DomainValueLike[Int]): Gen[Iterable[Interval[Dim3]]] =
    genNonIntersecting(3, 5, genDim3)
  def genNonIntersectingDim4(using DomainValueLike[Int]): Gen[Iterable[Interval[Dim4]]] =
    genNonIntersecting(2, 3, genDim4)
