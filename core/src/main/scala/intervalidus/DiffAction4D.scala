package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information.
  * @note
  *   intervalidus does not have event-sourced data structures, and a history of mutations is not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value.
  * @tparam R4
  *   the type of domain value used in the fourth interval assigned to each value.
  */
enum DiffAction4D[V, R1, R2, R3, R4]:
  case Create(validData: ValidData4D[V, R1, R2, R3, R4])
  case Update(validData: ValidData4D[V, R1, R2, R3, R4])
  case Delete(key: Domain4D[R1, R2, R3, R4])

object DiffAction4D:
  given [V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike]
    : DiffActionLike[DiffAction4D[V, R1, R2, R3, R4]] with
    import Domain4D.given
    extension (action: DiffAction4D[V, R1, R2, R3, R4])
      override def toCodeLikeString: String =
        action match
          case Create(validData) => s"DiffAction4D.Create(${validData.toCodeLikeString})"
          case Update(validData) => s"DiffAction4D.Update(${validData.toCodeLikeString})"
          case Delete(key)       => s"DiffAction4D.Delete(${key.toCodeLikeString})"
