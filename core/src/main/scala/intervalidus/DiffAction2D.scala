package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information.
  * @note
  *   intervalidus does not have event-sourced data structures, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R1
  *   the type of discrete value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete value used in the vertical interval assigned to each value.
  */
enum DiffAction2D[V, R1, R2]:
  case Create(validData: ValidData2D[V, R1, R2])
  case Update(validData: ValidData2D[V, R1, R2])
  case Delete(key: Domain2D[R1, R2])

object DiffAction2D:
  given [V, R1: DiscreteValue, R2: DiscreteValue]: DiffActionLike[DiffAction2D[V, R1, R2]] with
    extension (action: DiffAction2D[V, R1, R2])
      override def toCodeLikeString: String = action match
        case Create(validData) => s"DiffAction2D.Create(${validData.toCodeLikeString})"
        case Update(validData) => s"DiffAction2D.Update(${validData.toCodeLikeString})"
        case Delete(key)       => s"DiffAction2D.Delete(${key.toCodeLikeString})"
