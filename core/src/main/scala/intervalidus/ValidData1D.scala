package intervalidus

/**
  * A value that is valid in a one-dimensional interval. Conceptually, this defines a partial function where all domain
  * elements that are part of the interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R
  *   the type of domain value used in the interval assigned to each value (the domain).
  * @param value
  *   value that is valid in this interval.
  * @param interval
  *   the interval in which the value is valid.
  */
case class ValidData1D[V, R: DomainValueLike](
  value: V,
  interval: Interval1D[R]
) extends ValidDataLike[V, Domain1D[R], Interval1D[R], ValidData1D[V, R]]:
  // no crossing, so no parens required
  override protected def qualifiedInterval: String = s"${interval.toCodeLikeString}"
