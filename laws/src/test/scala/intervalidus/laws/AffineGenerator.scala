package intervalidus.laws

import intervalidus.laws.DomainGenerator.*
import intervalidus.*
import org.scalacheck.Gen

import scala.annotation.nowarn

// Generates IntervalShape with intervals of any dimension
object AffineGenerator:
  // A tuple type with the same number of elements as T, but every element is Double
  type TupleOfDoubles[T <: NonEmptyTuple] <: NonEmptyTuple = T match
    case ? *: EmptyTuple => Double *: EmptyTuple
    case ? *: tail       => Double *: TupleOfDoubles[tail]

  // A tuple type with the same number of elements as T, but every element is Int
  type TupleOfInts[T <: NonEmptyTuple] <: NonEmptyTuple = T match
    case ? *: EmptyTuple => Int *: EmptyTuple
    case ? *: tail       => Int *: TupleOfInts[tail]

  // for displacements -- all Ints
  inline def mapDisplacement[T <: Tuple](b: T, f: Int => Int): T =
    @nowarn("msg=pattern selector should be an instance of Matchable")
    def c[T <: Tuple](x: T): Tuple = x match
      case EmptyTuple          => EmptyTuple
      case (head: Int) *: tail => f(head) *: c(tail)
      case other *: tail       => other *: c(tail)
    c(b).asInstanceOf[T]

  inline def negated[T <: Tuple](b: T): T = mapDisplacement(b, i => -i)

  // for scalars -- all Doubles
  inline def mapScalar[T <: Tuple](b: T, f: Double => Double): T =
    @nowarn("msg=pattern selector should be an instance of Matchable")
    def c[T <: Tuple](x: T): Tuple = x match
      case EmptyTuple             => EmptyTuple
      case (head: Double) *: tail => f(head) *: c(tail)
      case other *: tail          => other *: c(tail)
    c(b).asInstanceOf[T]

  inline def inverted[T <: Tuple](b: T): T = mapScalar(b, d => 1 / d)

  // adjust Int displacements using Double scalars
  inline def mapScaledDisplacement[D <: Tuple, S <: Tuple](d: D, s: S, f: (Int, Double) => Int): D =
    @nowarn("msg=pattern selector should be an instance of Matchable")
    def c[D <: Tuple, S <: Tuple](d: D, s: S): Tuple = (d, s) match
      case (EmptyTuple, EmptyTuple)                          => EmptyTuple
      case ((headD: Int) *: tailD, (headS: Double) *: tailS) => f(headD, headS) *: c(tailD, tailS)
      case (otherD *: tailD, _ *: tailS)                     => otherD *: c(tailD, tailS)
      case theUnexpected => throw IllegalArgumentException(s"didn't expect $theUnexpected")
    c(d, s).asInstanceOf[D]

  private def genDisplacemen1D: Gen[Int] = Gen.choose(intRange.start, intRange.end)

  private def genScalar1D: Gen[Double] = Gen.oneOf(List(-4.0, -2.0, -1.0, 1.0, 2.0, 4.0))

  // Generates domains of any dimension, specialized for Ints
  trait GenAffineOps[D <: NonEmptyTuple]:
    def genDisplacement: Gen[TupleOfInts[D]]
    def genScalar: Gen[TupleOfDoubles[D]]

  private type OneDimTuple[T] = T *: EmptyTuple
  private type MultiDimTuple[T, DomainTail <: NonEmptyTuple] = T *: DomainTail

  inline def genDisplacement[D <: NonEmptyTuple](using genAffineOps: GenAffineOps[D]): Gen[TupleOfInts[D]] =
    genAffineOps.genDisplacement
  inline def genScalar[D <: NonEmptyTuple](using genAffineOps: GenAffineOps[D]): Gen[TupleOfDoubles[D]] =
    genAffineOps.genScalar

  /**
    * Base case, for a one-dimensional domain (empty tail)
    */
  given GenAffineOneDimOps[T]: GenAffineOps[OneDimTuple[T]] with
    inline override def genDisplacement: Gen[TupleOfInts[OneDimTuple[T]]] = genDisplacemen1D.map(_ *: EmptyTuple)
    inline override def genScalar: Gen[TupleOfDoubles[OneDimTuple[T]]] = genScalar1D.map(_ *: EmptyTuple)

  /**
    * Inductive case for a domain with two or more dimensions (non-empty tail)
    */
  given GenAffineMultiDimOps[T, DomainTail <: NonEmptyTuple](using
    applyToTail: GenAffineOps[DomainTail]
  ): GenAffineOps[MultiDimTuple[T, DomainTail]] with
    inline override def genDisplacement: Gen[TupleOfInts[MultiDimTuple[T, DomainTail]]] =
      for
        head <- genDisplacemen1D
        tail <- applyToTail.genDisplacement
      yield head *: tail

    inline override def genScalar: Gen[TupleOfDoubles[MultiDimTuple[T, DomainTail]]] =
      for
        head <- genScalar1D
        tail <- applyToTail.genScalar
      yield head *: tail
