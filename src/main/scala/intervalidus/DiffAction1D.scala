package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information. Note that this is not an event-sourced data structure, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value.
  */
enum DiffAction1D[V, R]
  extends DimensionalBase.DiffActionLike[
    V,
    DiscreteDomain1D[R],
    DiscreteInterval1D[R],
    ValidData1D[V, R],
    DiffAction1D[V, R]
  ]:
  case Create(validData: ValidData1D[V, R])
  case Update(validData: ValidData1D[V, R])
  case Delete(key: DiscreteDomain1D[R])
