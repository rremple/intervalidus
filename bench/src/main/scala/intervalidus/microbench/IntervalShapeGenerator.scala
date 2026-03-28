package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.*
import intervalidus.microbench.IntervalGenerator.*

// Generates IntervalShape with intervals of any dimension
object IntervalShapeGenerator:

  def gen[D <: NonEmptyTuple: DomainLike](
    genIntervals: Gen[Iterable[Interval[D]]]
  ): Gen[IntervalShape[D]] =
    for initialData <- genIntervals
    yield IntervalShape.withoutChecks(initialData)

  def genDim1(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim1]] =
    gen(genNonIntersectingDim1)
  def genDim2(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim2]] =
    gen(genNonIntersectingDim2)
  def genDim3(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim3]] =
    gen(genNonIntersectingDim3)
  def genDim4(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim4]] =
    gen(genNonIntersectingDim4)
  def genDim5(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim5]] =
    gen(genNonIntersectingDim5)

  def genDim2Special(limit: Int)(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim2]] =
    for initialData <- genNonIntersectingDim2Special(limit)
    yield IntervalShape.withoutChecks(initialData)

  def genDim3Special(limit: Int)(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim3]] =
    for initialData <- genNonIntersectingDim3Special(limit)
    yield IntervalShape.withoutChecks(initialData)
