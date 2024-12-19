package intervalidus

import intervalidus.collection.BoxedPayload

/**
  * A value that is valid in a two-dimensional discrete interval. Conceptually, this defines a partial function where
  * all domain elements that are part of the 2D interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R1
  *   the type of the horizontal discrete interval.
  * @tparam R2
  *   the type of the vertical discrete interval.
  * @param value
  *   value that is valid in this 2D interval.
  * @param interval
  *   the discrete interval in which the value is valid.
  */
case class ValidData2D[V, R1: DiscreteValue, R2: DiscreteValue](
  value: V,
  interval: DiscreteInterval2D[R1, R2]
) extends ValidDataLike[V, DiscreteDomain2D[R1, R2], DiscreteInterval2D[R1, R2], ValidData2D[V, R1, R2]]
