package intervalidus.laws

import intervalidus.laws.DomainGenerator.*
import intervalidus.*
import org.scalacheck.Gen

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
    def c[T <: Tuple](x: T): Tuple = x match
      case EmptyTuple          => EmptyTuple
      case (head: Int) *: tail => f(head) *: c(tail)
      case other *: tail       => other *: c(tail)
    c(b).asInstanceOf[T]

  inline def negated[T <: Tuple](b: T): T = mapDisplacement(b, i => -i)

  // for scalars -- all Doubles
  inline def mapScalar[T <: Tuple](b: T, f: Double => Double): T =
    def c[T <: Tuple](x: T): Tuple = x match
      case EmptyTuple             => EmptyTuple
      case (head: Double) *: tail => f(head) *: c(tail)
      case other *: tail          => other *: c(tail)
    c(b).asInstanceOf[T]

  inline def inverted[T <: Tuple](b: T): T = mapScalar(b, d => 1 / d)

  // adjust Int displacements using Double scalars
  inline def mapScaledDisplacement[D <: Tuple, S <: Tuple](d: D, s: S, f: (Int, Double) => Int): D =
    def c[D <: Tuple, S <: Tuple](d: D, s: S): Tuple = (d, s) match
      case (EmptyTuple, EmptyTuple)                          => EmptyTuple
      case ((headD: Int) *: tailD, (headS: Double) *: tailS) => f(headD, headS) *: c(tailD, tailS)
      case (otherD *: tailD, _ *: tailS)                     => otherD *: c(tailD, tailS)
      case theUnexpected                                     => throw Exception(s"didn't expect $theUnexpected")
    c(d, s).asInstanceOf[D]

  private def genDisplacemen1D: Gen[Int] = Gen.choose(intRange.start, intRange.end)

  def genDisplacemenDim1: Gen[TupleOfInts[Dim1]] =
    genDisplacemen1D.map(_ *: EmptyTuple)
  def genDisplacemenDim2: Gen[TupleOfInts[Dim2]] =
    Gen.zip(genDisplacemen1D, genDisplacemen1D)
  def genDisplacemenDim3: Gen[TupleOfInts[Dim3]] =
    Gen.zip(genDisplacemen1D, genDisplacemen1D, genDisplacemen1D)
  def genDisplacemenDim4: Gen[TupleOfInts[Dim4]] =
    Gen.zip(genDisplacemen1D, genDisplacemen1D, genDisplacemen1D, genDisplacemen1D)

  private def genScalar1D: Gen[Double] = Gen.oneOf(List(-4.0, -2.0, -1.0, 1.0, 2.0, 4.0))

  def genScalarDim1: Gen[TupleOfDoubles[Dim1]] =
    genScalar1D.map(_ *: EmptyTuple)
  def genScalarDim2: Gen[TupleOfDoubles[Dim2]] =
    Gen.zip(genScalar1D, genScalar1D)
  def genScalarDim3: Gen[TupleOfDoubles[Dim3]] =
    Gen.zip(genScalar1D, genScalar1D, genScalar1D)
  def genScalarDim4: Gen[TupleOfDoubles[Dim4]] =
    Gen.zip(genScalar1D, genScalar1D, genScalar1D, genScalar1D)
