package intervalidus.laws

import DomainGenerator.*
import IntervalGenerator.*
import intervalidus.{CoreConfig, DomainLike, DomainValueLike, Interval, IntervalShape}
import org.scalacheck.Gen

// Generates IntervalShape with intervals of any dimension
object IntervalShapeGenerator:

  private def gen[D <: NonEmptyTuple: DomainLike](
    genIntervals: Gen[Iterable[Interval[D]]]
  )(using CoreConfig[D]): Gen[IntervalShape[D]] =
    for initialData <- genIntervals
    yield IntervalShape(initialData)

  def genDim1(using config: CoreConfig[Dim1])(using DomainValueLike[Int]): Gen[IntervalShape[Dim1]] =
    gen(genNonIntersectingDim1)
  def genDim2(using config: CoreConfig[Dim2])(using DomainValueLike[Int]): Gen[IntervalShape[Dim2]] =
    gen(genNonIntersectingDim2)
  def genDim3(using config: CoreConfig[Dim3])(using DomainValueLike[Int]): Gen[IntervalShape[Dim3]] =
    gen(genNonIntersectingDim3)
  def genDim4(using config: CoreConfig[Dim4])(using DomainValueLike[Int]): Gen[IntervalShape[Dim4]] =
    gen(genNonIntersectingDim4)
