package intervalidus

/**
  * A value that is valid in a four-dimensional interval. Conceptually, this defines a partial function where all domain
  * elements that are part of the 4D interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R1
  *   the type of the horizontal interval.
  * @tparam R2
  *   the type of the vertical interval.
  * @tparam R3
  *   the type of the depth interval.
  * @tparam R4
  *   the type of the forth dimension interval.
  * @param value
  *   value that is valid in this 2D interval.
  * @param interval
  *   the interval in which the value is valid.
  */
case class ValidData4D[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
  value: V,
  interval: Interval4D[R1, R2, R3, R4]
) extends ValidDataLike[V, Domain4D[R1, R2, R3, R4], Interval4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]]
