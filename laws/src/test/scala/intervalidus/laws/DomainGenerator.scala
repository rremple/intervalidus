package intervalidus.laws

import intervalidus.*
import org.scalacheck.Gen

// Generates domains of any dimension, specialized for Ints (hopefully much faster this way)
object DomainGenerator:
  val intRange: Range.Inclusive = -1000 to 1000

  private def gen1D(using DomainValueLike[Int]): Gen[Domain1D[Int]] = summon[DomainValueLike[Int]] match
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

  private def genBounded1D(using DomainValueLike[Int]): Gen[Domain1D[Int]] =
    summon[DomainValueLike[Int]] match
      case _: DiscreteValue[?] =>
        Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.domain)

      case _: ContinuousValue[?] =>
        Gen.frequency(
          50 -> Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.domain),
          50 -> Gen.choose(intRange.start, intRange.end - 2).map(Domain1D.open)
        )

  private def genStart1D(using DomainValueLike[Int]): Gen[Domain1D[Int]] =
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

  private def genEnd1D(start: Domain1D[Int])(using DomainValueLike[Int]): Gen[Domain1D[Int]] =
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

  private def genBoundedEnd1D(start: Domain1D[Int])(using DomainValueLike[Int]): Gen[Domain1D[Int]] =
    summon[DomainValueLike[Int]] match
      case _: DiscreteValue[?] =>
        val startInt = start match
          case Domain1D.Point(value: Int) => value
          case Domain1D.Bottom            => intRange.start
          case _                          => throw IllegalArgumentException("Can't start after top")
        Gen.choose(startInt, intRange.end).map(Domain1D.domain)

      case _: ContinuousValue[?] =>
        val startInt = start match
          case Domain1D.Point(value: Int)     => value
          case Domain1D.OpenPoint(value: Int) => value + 1
          case Domain1D.Bottom                => intRange.start
          case _                              => throw IllegalArgumentException("Can't start after top")
        Gen.frequency(
          50 -> Gen.choose(startInt, intRange.end).map(Domain1D.domain),
          50 -> Gen.choose(startInt + 1, intRange.end).map(Domain1D.open)
        )

  type Dim1 = Domain.In1D[Int]
  type Dim2 = Domain.In2D[Int, Int]
  type Dim3 = Domain.In3D[Int, Int, Int]
  type Dim4 = Domain.In4D[Int, Int, Int, Int]

  // Generates domains of any dimension, specialized for Ints
  trait GenDomainOps[D <: NonEmptyTuple]:
    def arity: Int
    def gen: Gen[D]
    def genStart: Gen[D]
    def genBoundedStart: Gen[D]
    def genEnd(after: D): Gen[D]
    def genBoundedEnd(after: D): Gen[D]

  private type OneDimDomain = Domain.In1D[Int]
  private type MultiDimDomain[DomainTail <: NonEmptyTuple] = Domain1D[Int] *: DomainTail

  inline def arity[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D]): Int = genDomainOps.arity
  inline def gen[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D]): Gen[D] = genDomainOps.gen
  inline def genStart[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D]): Gen[D] = genDomainOps.genStart
  inline def genBoundedStart[D <: NonEmptyTuple](using genDomainOps: GenDomainOps[D]): Gen[D] =
    genDomainOps.genBoundedStart
  inline def genEnd[D <: NonEmptyTuple](after: D)(using genDomainOps: GenDomainOps[D]): Gen[D] =
    genDomainOps.genEnd(after)
  inline def genBoundedEnd[D <: NonEmptyTuple](after: D)(using genDomainOps: GenDomainOps[D]): Gen[D] =
    genDomainOps.genBoundedEnd(after)

  /**
    * Base case, for a one-dimensional domain (empty tail)
    */
  given GenDomainOneDimOps(using DomainValueLike[Int]): GenDomainOps[OneDimDomain] with
    inline override def arity: Int = 1
    override lazy val gen: Gen[OneDimDomain] = gen1D.map(_.tupled)
    override lazy val genStart: Gen[OneDimDomain] = genStart1D.map(_.tupled)
    override lazy val genBoundedStart: Gen[OneDimDomain] = genBounded1D.map(_.tupled)
    inline override def genEnd(after: OneDimDomain): Gen[OneDimDomain] = genEnd1D(after(0)).map(_.tupled)
    inline override def genBoundedEnd(after: OneDimDomain): Gen[OneDimDomain] = genBoundedEnd1D(after(0)).map(_.tupled)

  /**
    * Inductive case for a domain with two or more dimensions (non-empty tail)
    */
  given GenDomainMultiDimOps[DomainTail <: NonEmptyTuple](using
    applyToTail: GenDomainOps[DomainTail]
  )(using DomainValueLike[Int]): GenDomainOps[Domain1D[Int] *: DomainTail] with

    extension (tailGen: Gen[DomainTail])
      inline def withHead(headGen: Gen[Domain1D[Int]]): Gen[MultiDimDomain[DomainTail]] =
        Gen.zip(headGen, tailGen).map(_ *: _)

    inline override def arity: Int = applyToTail.arity + 1
    override lazy val gen: Gen[MultiDimDomain[DomainTail]] = applyToTail.gen.withHead(gen1D)
    override lazy val genStart: Gen[MultiDimDomain[DomainTail]] = applyToTail.genStart.withHead(genStart1D)
    override lazy val genBoundedStart: Gen[MultiDimDomain[DomainTail]] =
      applyToTail.genBoundedStart.withHead(genBounded1D)
    inline override def genEnd(after: MultiDimDomain[DomainTail]): Gen[MultiDimDomain[DomainTail]] =
      applyToTail.genEnd(after.tail).withHead(genEnd1D(after.head))
    inline override def genBoundedEnd(after: MultiDimDomain[DomainTail]): Gen[MultiDimDomain[DomainTail]] =
      applyToTail.genBoundedEnd(after.tail).withHead(genBoundedEnd1D(after.head))
