package intervalidus.laws

import DomainGenerator.*
import IntervalGenerator.*
import intervalidus.{CoreConfig, DomainLike, immutable}
import org.scalacheck.Gen

// Generates Data with intervals of any dimension and either fixed or random values
object DataGenerator:
  def testCoreConfig[D <: NonEmptyTuple: DomainLike]: CoreConfig[D] =
    CoreConfig.default // .withCompressOnUpdate(false)

  /**
    * Generate an immutable.Data structure where the values randomly-generated strings.
    */
  def gen[D <: NonEmptyTuple: DomainLike: GenDomainOps](using config: CoreConfig[D]): Gen[immutable.Data[String, D]] =
    for
      paletteSize <- Gen.choose(2, 8)
      palette = (1 to paletteSize).map(i => s"R$i")
      intervals <- genNonIntersecting[D]
      values <- Gen.listOfN(intervals.size, Gen.oneOf(palette))
    yield immutable.Data(intervals.zip(values).map(_ -> _))
