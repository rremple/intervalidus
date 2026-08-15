package intervalidus.microbench

import intervalidus.*

// Generates domains of any dimension, specialized for Ints (hopefully much faster this way)
object DomainGenerator:
  val intRange: Range.Inclusive = -1_000_000 to 1_000_000

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
          case _                          => throw IllegalArgumentException("Can't start after top")
        Gen.frequency(
          80 -> Gen.choose(startInt, intRange.end).map(Domain1D.domain),
          20 -> Gen.const(Domain1D.Top)
        )

      case _: ContinuousValue[?] =>
        val startInt = start match
          case Domain1D.Point(value: Int)     => value
          case Domain1D.OpenPoint(value: Int) => value + 1
          case Domain1D.Bottom                => intRange.start
          case _                              => throw IllegalArgumentException("Can't start after top")
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

  // Generates domains of any dimension, specialized for Ints
  trait GenDomainOps[D <: NonEmptyTuple]:
    def arity: Int
    def gen: Gen[D]
    def genStart: Gen[D]
    def genEnd(after: D): Gen[D]

  private type OneDimDomain = Domain.In1D[Int]
  private type MultiDimDomain[DomainTail <: NonEmptyTuple] = Domain1D[Int] *: DomainTail

  def arity[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D])(using RandomNumbers): Int = genDomainOps.arity
  def gen[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D])(using RandomNumbers): Gen[D] = genDomainOps.gen
  def genStart[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D])(using RandomNumbers): Gen[D] =
    genDomainOps.genStart
  def genEnd[D <: NonEmptyTuple](after: D)(using genDomainOps: GenDomainOps[D])(using RandomNumbers): Gen[D] =
    genDomainOps.genEnd(after)

  /**
    * Base case, for a one-dimensional domain (empty tail)
    */
  given GenDomainOneDimOps(using RandomNumbers, DomainValueLike[Int]): GenDomainOps[OneDimDomain] with
    inline override def arity: Int = 1
    inline override def gen: Gen[OneDimDomain] = gen1D.map(_.tupled)
    inline override def genStart: Gen[OneDimDomain] = genStart1D.map(_.tupled)
    inline override def genEnd(after: OneDimDomain): Gen[OneDimDomain] = genEnd1D(after(0)).map(_.tupled)

  /**
    * Inductive case for a domain with two or more dimensions (non-empty tail)
    */
  given GenDomainMultiDimOps[DomainTail <: NonEmptyTuple](using
    applyToTail: GenDomainOps[DomainTail]
  )(using RandomNumbers, DomainValueLike[Int]): GenDomainOps[Domain1D[Int] *: DomainTail] with

    extension (tailGen: Gen[DomainTail])
      def withHead(headGen: Gen[Domain1D[Int]]): Gen[MultiDimDomain[DomainTail]] =
        for
          head <- headGen
          tail <- tailGen
        yield head *: tail

    inline override def arity: Int = applyToTail.arity + 1
    inline override def gen: Gen[MultiDimDomain[DomainTail]] = applyToTail.gen.withHead(gen1D)
    inline override def genStart: Gen[MultiDimDomain[DomainTail]] = applyToTail.genStart.withHead(genStart1D)
    inline override def genEnd(after: MultiDimDomain[DomainTail]): Gen[MultiDimDomain[DomainTail]] =
      applyToTail.genEnd(after.tail).withHead(genEnd1D(after.head))
