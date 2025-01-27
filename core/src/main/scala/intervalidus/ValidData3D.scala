package intervalidus

/**
  * A value that is valid in a three-dimensional interval. Conceptually, this defines a partial function where all
  * domain elements that are part of the 3D interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R1
  *   the type of the horizontal interval.
  * @tparam R2
  *   the type of the vertical interval.
  * @tparam R3
  *   the type of the depth interval.
  * @param value
  *   value that is valid in this 2D interval.
  * @param interval
  *   the interval in which the value is valid.
  */
case class ValidData3D[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
  value: V,
  interval: Interval3D[R1, R2, R3]
) extends ValidDataLike[V, Domain3D[R1, R2, R3], Interval3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]]
