package intervalidus.laws

import DomainGenerator.*
import IntervalGenerator.*
import intervalidus.{CoreConfig, DomainLike, IntervalShape}
import org.scalacheck.Gen

// Generates IntervalShape with intervals of any dimension
object IntervalShapeGenerator:

  def gen[D <: NonEmptyTuple: DomainLike: GenDomainOps](using config: CoreConfig[D]): Gen[IntervalShape[D]] =
    for initialData <- genNonIntersecting[D]
    yield IntervalShape(initialData)
