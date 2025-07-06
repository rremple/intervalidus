package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information.
  * @note
  *   intervalidus does not have event-sourced data structures, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam D
  *   the type of domain used in the interval assigned to each value. the type of domain value used in the depth
  *   interval assigned to each value.
  */
enum DiffAction[+V, +D <: NonEmptyTuple](using DomainLike[D]):
  case Create[VV, DD <: NonEmptyTuple: DomainLike](validData: ValidData[VV, DD]) extends DiffAction[VV, DD]
  case Update[VV, DD <: NonEmptyTuple: DomainLike](validData: ValidData[VV, DD]) extends DiffAction[VV, DD]
  case Delete[DD <: NonEmptyTuple: DomainLike](key: DD) extends DiffAction[Nothing, DD]

  def toCodeLikeString: String = this match
    case Create(validData) => s"DiffAction.Create(${validData.toCodeLikeString})"
    case Update(validData) => s"DiffAction.Update(${validData.toCodeLikeString})"
    case Delete(key)       => s"DiffAction.Delete(${key.toCodeLikeString})"

object DiffAction:
  type In1D[V, R1] = DiffAction[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DiffAction[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DiffAction[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DiffAction[V, Domain.In4D[R1, R2, R3, R4]]
