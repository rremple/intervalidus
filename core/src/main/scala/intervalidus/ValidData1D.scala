package intervalidus

import intervalidus.collection.BoxedPayload1D

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
) extends ValidDataLike[V, DiscreteDomain1D[R], DiscreteInterval1D[R], ValidData1D[V, R]]:
  def asBoxedPayload: BoxedPayload1D[ValidData1D[V, R]] =
    BoxedPayload1D(interval.asBox, this)

  // no crossing, so no parens required
  override protected def qualifiedInterval: String = s"${interval.toCodeLikeString}"

/**
  * Companion for valid data in one dimension.
  */
object ValidData1D:
  /**
    * Valid data are ordered using interval ordering
    */
  given [V, R: DiscreteValue](using
    intervalOrder: Ordering[DiscreteInterval1D[R]]
  ): Ordering[ValidData1D[V, R]] with
    override def compare(x: ValidData1D[V, R], y: ValidData1D[V, R]): Int =
      intervalOrder.compare(x.interval, y.interval)
