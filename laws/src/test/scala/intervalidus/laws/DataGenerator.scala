package intervalidus.laws

import DomainGenerator.*
import IntervalGenerator.*
import intervalidus.{CoreConfig, DomainLike, DomainValueLike, Interval, immutable}
import org.scalacheck.Gen

// Generates Data with intervals of any dimension and either fixed or random values
object DataGenerator:

  /**
    * Generate an immutable.Data structure where the values randomly-generated strings.
    */
  private def gen[D <: NonEmptyTuple: DomainLike](
    genIntervals: Gen[Iterable[Interval[D]]]
  )(using CoreConfig[D]): Gen[immutable.Data[String, D]] = for
    paletteSize <- Gen.choose(2, 8)
    palette = (1 to paletteSize).map(i => s"R$i")
    intervals <- genIntervals
    values <- Gen.listOfN(intervals.size, Gen.oneOf(palette))
  yield immutable.Data(intervals.zip(values).map(_ -> _))

  def genDim1(using DomainValueLike[Int], CoreConfig[Dim1]): Gen[immutable.Data[String, Dim1]] =
    gen(genNonIntersectingDim1)
  def genDim2(using DomainValueLike[Int], CoreConfig[Dim2]): Gen[immutable.Data[String, Dim2]] =
    gen(genNonIntersectingDim2)
  def genDim3(using DomainValueLike[Int], CoreConfig[Dim3]): Gen[immutable.Data[String, Dim3]] =
    gen(genNonIntersectingDim3)
  def genDim4(using DomainValueLike[Int], CoreConfig[Dim4]): Gen[immutable.Data[String, Dim4]] =
    gen(genNonIntersectingDim4)
