package intervalidus.microbench

import intervalidus.*

// Generates domains of any dimension, specialized for Ints (hopefully much faster this way)
object DomainGenerator:
  val intRange: Range.Inclusive = -1000 to 1000

  private def gen1D(using RandomNumbers, DomainValueLike[Int]): Gen[Domain1D[Int]] = summon[DomainValueLike[Int]] match
    case _: DiscreteValue[?] =>
      Gen.frequency(
        80 -> Gen.choose(intRange.start, intRange.end).map(Domain1D.domain),
        10 -> Gen.const(Domain1D.Bottom),
        10 -> Gen.const(Domain1D.Top)
      )

    case _: ContinuousValue[?] =>
      Gen.frequency(
        40 -> Gen.choose(intRange.start, intRange.end).map(Domain1D.domain),
        40 -> Gen.choose(intRange.start, intRange.end).map(Domain1D.open),
        10 -> Gen.const(Domain1D.Bottom),
        10 -> Gen.const(Domain1D.Top)
      )

  private def genStart1D(using RandomNumbers, DomainValueLike[Int]): Gen[Domain1D[Int]] =
    summon[DomainValueLike[Int]] match
      case _: DiscreteValue[?] =>
        Gen.frequency(
          80 -> Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.domain),
          20 -> Gen.const(Domain1D.Bottom)
        )

      case _: ContinuousValue[?] =>
        Gen.frequency(
          40 -> Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.domain),
          40 -> Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.open),
          20 -> Gen.const(Domain1D.Bottom)
        )

  private def genEnd1D(start: Domain1D[Int])(using RandomNumbers, DomainValueLike[Int]): Gen[Domain1D[Int]] =
    summon[DomainValueLike[Int]] match
      case _: DiscreteValue[?] =>
        val startInt = start match
          case Domain1D.Point(value: Int) => value
          case Domain1D.Bottom            => intRange.start
          case _                          => throw Exception("Can't start after top")
        Gen.frequency(
          80 -> Gen.choose(startInt, intRange.end).map(Domain1D.domain),
          20 -> Gen.const(Domain1D.Top)
        )

      case _: ContinuousValue[?] =>
        val startInt = start match
          case Domain1D.Point(value: Int)     => value
          case Domain1D.OpenPoint(value: Int) => value + 1
          case Domain1D.Bottom                => intRange.start
          case _                              => throw Exception("Can't start after top")
        Gen.frequency(
          40 -> Gen.choose(startInt, intRange.end).map(Domain1D.domain),
          40 -> Gen.choose(startInt + 1, intRange.end).map(Domain1D.open),
          20 -> Gen.const(Domain1D.Top)
        )

  type Dim1 = Domain.In1D[Int]
  type Dim2 = Domain.In2D[Int, Int]
  type Dim3 = Domain.In3D[Int, Int, Int]
  type Dim4 = Domain.In4D[Int, Int, Int, Int]
  type Dim5 = Domain1D[Int] *: Domain.In4D[Int, Int, Int, Int]

  def genDim1(using RandomNumbers, DomainValueLike[Int]): Gen[Dim1] = gen1D.map(_ *: EmptyTuple)
  def genDim2(using RandomNumbers, DomainValueLike[Int]): Gen[Dim2] = Gen.zip(gen1D, gen1D)
  def genDim3(using RandomNumbers, DomainValueLike[Int]): Gen[Dim3] =
    Gen.zip(gen1D, gen1D, gen1D)
  def genDim4(using RandomNumbers, DomainValueLike[Int]): Gen[Dim4] =
    Gen.zip(gen1D, gen1D, gen1D, gen1D)
  def genDim5(using RandomNumbers, DomainValueLike[Int]): Gen[Dim5] =
    Gen.zip(gen1D, gen1D, gen1D, gen1D, gen1D)

  def genStartDim1(using RandomNumbers, DomainValueLike[Int]): Gen[Dim1] =
    genStart1D.map(_ *: EmptyTuple)
  def genStartDim2(using RandomNumbers, DomainValueLike[Int]): Gen[Dim2] =
    Gen.zip(genStart1D, genStart1D)
  def genStartDim3(using RandomNumbers, DomainValueLike[Int]): Gen[Dim3] =
    Gen.zip(genStart1D, genStart1D, genStart1D)
  def genStartDim4(using RandomNumbers, DomainValueLike[Int]): Gen[Dim4] = Gen.zip(
    genStart1D,
    genStart1D,
    genStart1D,
    genStart1D
  )
  def genStartDim5(using RandomNumbers, DomainValueLike[Int]): Gen[Dim5] = Gen.zip(
    genStart1D,
    genStart1D,
    genStart1D,
    genStart1D,
    genStart1D
  )

  def genEndDim1(after: Dim1)(using RandomNumbers, DomainValueLike[Int]): Gen[Dim1] =
    genEnd1D(after(0)).map(_ *: EmptyTuple)
  def genEndDim2(after: Dim2)(using RandomNumbers, DomainValueLike[Int]): Gen[Dim2] =
    Gen.zip(genEnd1D(after(0)), genEnd1D(after(1)))
  def genEndDim3(after: Dim3)(using RandomNumbers, DomainValueLike[Int]): Gen[Dim3] = Gen.zip(
    genEnd1D(after(0)),
    genEnd1D(after(1)),
    genEnd1D(after(2))
  )
  def genEndDim4(after: Dim4)(using RandomNumbers, DomainValueLike[Int]): Gen[Dim4] = Gen.zip(
    genEnd1D(after(0)),
    genEnd1D(after(1)),
    genEnd1D(after(2)),
    genEnd1D(after(3))
  )

  def genEndDim5(after: Dim5)(using RandomNumbers, DomainValueLike[Int]): Gen[Dim5] = Gen.zip(
    genEnd1D(after(0)),
    genEnd1D(after(1)),
    genEnd1D(after(2)),
    genEnd1D(after(3)),
    genEnd1D(after(4))
  )
