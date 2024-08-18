package intervalidus

/**
  * A two-dimensional discrete domain, like cartesian coordinates. Used in conjunction with [[DiscreteInterval2D]].
  *
  * @param horizontalIndex
  *   domain element on the x-axis.
  * @param verticalIndex
  *   domain element on the y-axis.
  * @tparam T1
  *   the horizontal domain type
  * @tparam T2
  *   the vertical domain type
  */
case class DiscreteDomain2D[T1, T2](
  horizontalIndex: DiscreteDomain1D[T1],
  verticalIndex: DiscreteDomain1D[T2]
) extends DimensionalBase.DomainLike:
  /**
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo(interval: DiscreteInterval2D[T1, T2]): Boolean = interval contains this

  // equivalent symbolic method names

  /**
    * Same as [[belongsTo]]
    *
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  def ∈(interval: DiscreteInterval2D[T1, T2]): Boolean = this belongsTo interval

object DiscreteDomain2D:
  /**
    * Provides a default ordering for any 2D domain based on the orderings of its constituent horizontal and vertical
    * interval components.
    *
    * @param horizontalOrdering
    *   uses ordering of horizontal interval component.
    * @param verticalOrdering
    *   uses ordering of vertical interval component.
    * @tparam T1
    *   the horizontal domain type
    * @tparam T2
    *   the vertical domain type
    * @return
    *   an ordering for the 2D domain
    */
  given [T1, T2](using
    horizontalOrdering: Ordering[DiscreteDomain1D[T1]],
    verticalOrdering: Ordering[DiscreteDomain1D[T2]]
  ): Ordering[DiscreteDomain2D[T1, T2]] with
    override def compare(x: DiscreteDomain2D[T1, T2], y: DiscreteDomain2D[T1, T2]): Int =
      val horizontalCompare = horizontalOrdering.compare(x.horizontalIndex, y.horizontalIndex)
      if horizontalCompare != 0 then horizontalCompare
      else verticalOrdering.compare(x.verticalIndex, y.verticalIndex)
