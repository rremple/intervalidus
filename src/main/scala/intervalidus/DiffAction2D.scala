package intervalidus

/**
  * @inheritdoc
  *
  * @tparam V
  *   the type of the value managed as data (not used in Delete).
  * @tparam R1
  *   the type of discrete value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete value used in the vertical interval assigned to each value.
  */
enum DiffAction2D[V, R1, R2]
  extends DimensionalBase.DiffActionLike[
    V,
    DiscreteDomain2D[R1, R2],
    DiscreteInterval2D[R1, R2],
    ValidData2D[V, R1, R2],
    DiffAction2D[V, R1, R2]
  ]:
  case Create(validData: ValidData2D[V, R1, R2])
  case Update(validData: ValidData2D[V, R1, R2])
  case Delete(key: DiscreteDomain2D[R1, R2])
