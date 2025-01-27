package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information.
  * @note
  *   intervalidus does not have event-sourced data structures, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R
  *   the type of domain value used in the interval assigned to each value.
  */
enum DiffAction1D[V, R]:
  case Create(validData: ValidData1D[V, R])
  case Update(validData: ValidData1D[V, R])
  case Delete(key: Domain1D[R])

object DiffAction1D:
  given [V, R: DomainValueLike]: DiffActionLike[DiffAction1D[V, R]] with
    extension (action: DiffAction1D[V, R])
      override def toCodeLikeString: String = action match
        case Create(validData) => s"DiffAction1D.Create(${validData.toCodeLikeString})"
        case Update(validData) => s"DiffAction1D.Update(${validData.toCodeLikeString})"
        case Delete(key)       => s"DiffAction1D.Delete(${key.toCodeLikeString})"
