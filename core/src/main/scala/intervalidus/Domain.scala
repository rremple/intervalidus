package intervalidus

import scala.language.implicitConversions

object Domain:
  // NonEmptyTail[D] is only valid when the dimension of D is greater than one
  type NonEmptyTail[D <: NonEmptyTuple] = Tuple.Tail[D] & NonEmptyTuple

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
