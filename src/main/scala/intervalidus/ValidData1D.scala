package intervalidus

/**
  * A value that is valid in a one-dimensional discrete interval. Conceptually, this defines a partial function where
  * all domain elements that are part of the interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value (the domain).
  * @param value
  *   value that is valid in this interval.
  * @param interval
  *   the discrete interval in which the value is valid.
  */
case class ValidData1D[V, R: DiscreteValue](
  value: V,
  interval: DiscreteInterval1D[R]
) extends DimensionalBase.DataLike[V, DiscreteDomain1D[R], DiscreteInterval1D[R]]:
  // no crossing, so no parens required
  override protected def qualifiedInterval: String = s"${interval.toCodeLikeString}"
