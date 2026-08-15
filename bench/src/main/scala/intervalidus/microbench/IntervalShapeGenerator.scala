package intervalidus.microbench

import intervalidus.*
import intervalidus.microbench.DomainGenerator.*
import intervalidus.microbench.IntervalGenerator.*

// Generates IntervalShape with intervals of any dimension
object IntervalShapeGenerator:

  def gen[D <: NonEmptyTuple: DomainLike: GenDomainOps](using RandomNumbers): Gen[IntervalShape[D]] =
    for initialData <- genNonIntersecting[D]
    yield IntervalShape.withoutChecks(initialData)

  def genDim2Special(limit: Int)(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim2]] =
    for initialData <- genNonIntersectingDim2Special(limit)
    yield IntervalShape.withoutChecks(initialData)

  def genDim3Special(limit: Int)(using RandomNumbers, DomainValueLike[Int]): Gen[IntervalShape[Dim3]] =
    for initialData <- genNonIntersectingDim3Special(limit)
    yield IntervalShape.withoutChecks(initialData)
