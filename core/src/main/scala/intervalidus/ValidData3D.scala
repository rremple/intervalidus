package intervalidus

import intervalidus.collection.BoxedPayload

/**
  * A value that is valid in a three-dimensional discrete interval. Conceptually, this defines a partial function where
  * all domain elements that are part of the 3D interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam R1
  *   the type of the horizontal discrete interval.
  * @tparam R2
  *   the type of the vertical discrete interval.
  * @tparam R3
  *   the type of the depth discrete interval.
  * @param value
  *   value that is valid in this 2D interval.
  * @param interval
  *   the discrete interval in which the value is valid.
  */
case class ValidData3D[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
  value: V,
  interval: DiscreteInterval3D[R1, R2, R3]
) extends ValidDataLike[
    V,
    DiscreteDomain3D[R1, R2, R3],
    DiscreteInterval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3]
  ]:

  override def asBoxedPayload: BoxedPayload[ValidData3D[V, R1, R2, R3]] = BoxedPayload(interval.asBox, this)

/**
  * Companion for valid data in three dimensions.
  */
object ValidData3D:
  /**
    * Valid data are ordered using interval ordering
    */
  given [V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    intervalOrder: Ordering[DiscreteInterval3D[R1, R2, R3]]
  ): Ordering[ValidData3D[V, R1, R2, R3]] with
    override def compare(x: ValidData3D[V, R1, R2, R3], y: ValidData3D[V, R1, R2, R3]): Int =
      intervalOrder.compare(x.interval, y.interval)
