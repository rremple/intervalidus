package intervalidus

/**
  * A value that is valid in a two-dimensional interval. Conceptually, this defines a partial function where all domain
  * elements that are part of the 2D interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R1
  *   the type of the horizontal interval.
  * @tparam R2
  *   the type of the vertical interval.
  * @param value
  *   value that is valid in this 2D interval.
  * @param interval
  *   the interval in which the value is valid.
  */
case class ValidData2D[V, R1: DomainValueLike, R2: DomainValueLike](
  value: V,
  interval: Interval2D[R1, R2]
) extends ValidDataLike[V, Domain2D[R1, R2], Interval2D[R1, R2], ValidData2D[V, R1, R2]]
