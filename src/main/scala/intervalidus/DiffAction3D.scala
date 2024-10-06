package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information. Note that this is not an event-sourced data structure, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R1
  *   the type of discrete value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of discrete value used in the depth interval assigned to each value.
  */
enum DiffAction3D[V, R1, R2, R3]:
  case Create(validData: ValidData3D[V, R1, R2, R3])
  case Update(validData: ValidData3D[V, R1, R2, R3])
  case Delete(key: DiscreteDomain3D[R1, R2, R3])
