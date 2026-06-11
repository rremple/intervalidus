package intervalidus

import scala.Tuple.{Append, Concat, Drop, Elem, Head, Size, Tail, Take}
import scala.annotation.implicitNotFound
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.{<, >=}

/**
  * Common definitions for multidimensional domains.
  */
object Domain:
  /**
    * A numeric reference to a specific dimension. Dimensions are indexed starting at 0. For example, a
    * three-dimensional domain would have dimensions indexed by 0, 1, and 2.
    *
    * @note
    *   Dimension indexes are integer singletons, i.e. literal values that must be known at compile time.
    */
  type DimensionIndex = Int & Singleton

  /**
    * NonEmptyTail[D] is only valid when a multidimensional domain has a non-empty tail, i.e., it has at least two
    * dimensions.
    *
    * @tparam D
    *   a multidimensional domain
    */
  type NonEmptyTail[D <: NonEmptyTuple] = Tail[D] & NonEmptyTuple

  /**
    * A multidimensional domain with an appended one-dimensional domain.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam X
    *   appended domain value type
    */
  type Appended[D <: NonEmptyTuple, X] = Append[D, Domain1D[X]]

  /**
    * Witnesses that a multidimensional domain has a non-empty tail, i.e., it has at least two dimensions.
    *
    * @tparam D
    *   a multidimensional domain
    */
  @implicitNotFound(
    "Cannot prove that ${D} is at least two-dimensional." +
      "\nAdding the parameter `using Domain.IsAtLeastTwoDimensional[${D}]` may resolve this issue. "
  )
  type IsAtLeastTwoDimensional[D <: NonEmptyTuple] =
    Tail[D] =:= NonEmptyTail[D]

  /**
    * Witnesses that a multidimensional domain has the specified dimension.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I
    *   index of a one-dimensional domain
    */
  @implicitNotFound(
    "Cannot prove that ${D} has a dimension indexed by ${I}." +
      "\nAdding the parameter `using Domain.HasIndex[${D}, ${I}]` may resolve this issue. "
  )
  type HasIndex[D <: NonEmptyTuple, I <: DimensionIndex] = (I >= 0 && I < Size[D]) =:= true

  /**
    * Witnesses that a multidimensional domain has the specified dimensions that can be swapped.
    * @note
    *   the first dimension must be strictly less than the second one.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I1
    *   the lesser index of a one-dimensional domain
    * @tparam I1
    *   the greater index of a one-dimensional domain
    */
  @implicitNotFound(
    "Cannot prove that ${D} has swappable dimensions indexed by ${I1} < ${I2}." +
      "\nAdding the parameter `using Domain.HasSwappableIndexes[${D}, ${I1}, ${I2}]` may resolve this issue. "
  )
  type HasSwappableIndexes[D <: NonEmptyTuple, I1 <: DimensionIndex, I2 <: DimensionIndex] =
    HasIndex[D, I1] & HasIndex[D, I2] & I1 < I2 =:= true

  /**
    * Witnesses that a multidimensional domain can be updated by concatenating the elements before the indexed
    * one-dimensional domain, the indexed one-dimensional domain itself, and the elements after the indexed
    * one-dimensional domain.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I
    *   index of the updated one-dimensional domain
    * @tparam H
    *   indexed domain value type
    */
  @implicitNotFound(
    "Cannot prove that ${D} is updatable using Domain1D[${H}] at index ${I}." +
      "\nAdding the parameter `using Domain.IsUpdatableAtIndex[${D}, ${I}]` may resolve this issue. "
  )
  type IsUpdatableAtIndex[D <: NonEmptyTuple, I <: DimensionIndex, H] =
    Concat[Take[D, I], Domain1D[H] *: Drop[Drop[D, I], 1]] =:= D

  /**
    * Witnesses that a multidimensional domain can be updated by concatenating the head one-dimensional domain with the
    * multidimensional tail.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam H
    *   head domain value type
    */
  @implicitNotFound(
    "Cannot prove that ${D} is updatable using Domain1D[${H}] at head." +
      "\nAdding the parameter `using Domain.IsUpdatableAtHead[${D}, ${H}]` may resolve this issue. "
  )
  type IsUpdatableAtHead[D <: NonEmptyTuple, H] =
    Domain1D[H] *: Tail[D] =:= D

  /**
    * Witnesses that the specified result can be constructed by concatenating the elements before the insertion point,
    * the provided one-dimensional domain, and the elements after the insertion point.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I
    *   index after which the provided one-dimensional domain will be inserted
    * @tparam H
    *   indexed domain value type
    * @tparam R
    *   the multidimensional domain result after the one-dimensional domain is inserted
    */
  @implicitNotFound(
    "Cannot prove that ${D} with Domain1D[${H}] inserted at index ${I} results in ${R}." +
      "\nAdding the parameter `using Domain.IsInsertedInResult[${D}, ${I}, ${H}, ${R}]` may resolve this issue. "
  )
  type IsInsertedInResult[D <: NonEmptyTuple, I <: DimensionIndex, H, R <: NonEmptyTuple] =
    Concat[Take[D, I], Domain1D[H] *: Drop[D, I]] =:= R

  /**
    * Witnesses that the specified result can be constructed by swapping the one-dimensional domains at the indexed
    * dimensions.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I1
    *   the lesser index of a one-dimensional domain
    * @tparam I1
    *   the greater index of a one-dimensional domain
    * @tparam R
    *   the multidimensional domain result after the one-dimensional domains are swapped
    */
  @implicitNotFound(
    "Cannot prove that ${D} with swapped dimensions indexed by ${I1} < ${I2} results in ${R}." +
      "\nAdding the parameter `using Domain.IsSwappedInResult[${D}, ${I1}, ${I2}]` may resolve this issue. "
  )
  type IsSwappedInResult[D <: NonEmptyTuple, I1 <: DimensionIndex, I2 <: DimensionIndex, R <: NonEmptyTuple] =
    Concat[
      Take[D, I1], // The prefix before the first index.
      Concat[
        Elem[D, I2] *: Drop[Drop[Take[D, I2], I1], 1], // The second element moved up, followed by the middle segment.
        Elem[D, I1] *: Drop[Drop[D, I2], 1] // The first element moved down, followed by the remaining suffix.
      ]
    ] =:= R

  /**
    * Witnesses that the specified result can be constructed by concatenating the elements before and after the dropped
    * one-dimensional domain.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I
    *   index of the one-dimensional domain that will be dropped
    * @tparam R
    *   the multidimensional domain result after dropping the indexed one-dimensional domain
    */
  @implicitNotFound(
    "Cannot prove that ${D} with domain dropped at index ${I} results in ${R}." +
      "\nAdding the parameter `using Domain.IsDroppedInResult[${D}, ${I}, ${R}]` may resolve this issue. "
  )
  type IsDroppedInResult[D <: NonEmptyTuple, I <: DimensionIndex, R <: NonEmptyTuple] =
    Concat[Take[D, I], Drop[Drop[D, I], 1]] =:= R

  /**
    * Witnesses that a multidimensional domain contains a specific one-dimensional domain of the specified domain value
    * type at the specified index.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam I
    *   index of the one-dimensional domain
    * @tparam H
    *   indexed domain value type
    */
  @implicitNotFound(
    "Cannot prove that ${D} has Domain1D[${H}] at index ${I}." +
      "\nAdding the parameter `using Domain.IsAtIndex[${D}, ${I}, ${H}]` may resolve this issue. "
  )
  type IsAtIndex[D <: NonEmptyTuple, I <: DimensionIndex, H] =
    Elem[D, I] =:= Domain1D[H]

  /**
    * Witnesses that a multidimensional domain contains a specific one-dimensional domain of the specified domain value
    * type at the head.
    *
    * @tparam D
    *   a multidimensional domain
    * @tparam H
    *   head domain value type
    */
  @implicitNotFound(
    "Cannot prove that the head of ${D} is Domain1D[${H}]." +
      "\nAdding the parameter `using Domain.IsAtHead[${D}, ${H}]` may resolve this issue. "
  )
  type IsAtHead[D <: NonEmptyTuple, H] =
    Head[D] =:= Domain1D[H]

  /**
    * Uses recursive structural refinement to evaluate path-dependent types in an affine domain type D to witness that S
    * is a tuple of its displacement types. Like `=:=`, typically used infix as a witness, e.g.,
    * `using D HasDisplacementType S`.
    */
  @implicitNotFound("The displacement types in ${S} do not align with displacement types in the affine domain ${D}")
  sealed infix trait HasDisplacementType[D <: Tuple, S <: Tuple]

  object HasDisplacementType:
    given emptyProof: (EmptyTuple HasDisplacementType EmptyTuple)()

    given inductiveProof[DV, SV, DTail <: Tuple, STail <: Tuple](using
      DomainAffineValueLike[DV] { type Displacement = SV },
      DTail HasDisplacementType STail
    ): (Domain1D[DV] *: DTail HasDisplacementType SV *: STail)()

  /**
    * Uses recursive structural refinement to evaluate path-dependent types in an affine domain type D to witness that S
    * is a tuple of its scalar types. Like `=:=`, typically used infix as a witness, e.g., `using D HasScalarType S`.
    */
  @implicitNotFound("The scalar types in ${S} do not align with scalar types in the affine domain ${D}")
  sealed infix trait HasScalarType[D <: Tuple, S <: Tuple]

  object HasScalarType:
    given emptyProof: (EmptyTuple HasScalarType EmptyTuple)()

    given inductiveProof[DV, SV, DTail <: Tuple, STail <: Tuple](using
      DomainAffineValueLike[DV] { type Scalar = SV },
      DTail HasScalarType STail
    ): (Domain1D[DV] *: DTail HasScalarType SV *: STail)()

  type In1D[R1] = Domain1D[R1] *: EmptyTuple
  type In2D[R1, R2] = (Domain1D[R1], Domain1D[R2])
  type In3D[R1, R2, R3] = (Domain1D[R1], Domain1D[R2], Domain1D[R3])
  type In4D[R1, R2, R3, R4] = (Domain1D[R1], Domain1D[R2], Domain1D[R3], Domain1D[R4])

  def in1D[R1: DomainValueLike](
    r1: Domain1D[R1]
  ): In1D[R1] = r1 *: EmptyTuple
  def in2D[R1: DomainValueLike, R2: DomainValueLike](
    r1: Domain1D[R1],
    r2: Domain1D[R2]
  ): In2D[R1, R2] = (r1, r2)
  def in3D[R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    r1: Domain1D[R1],
    r2: Domain1D[R2],
    r3: Domain1D[R3]
  ): In3D[R1, R2, R3] = (r1, r2, r3)
  def in4D[R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    r1: Domain1D[R1],
    r2: Domain1D[R2],
    r3: Domain1D[R3],
    r4: Domain1D[R4]
  ): In4D[R1, R2, R3, R4] = (r1, r2, r3, r4)
