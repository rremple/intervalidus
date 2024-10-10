package intervalidus

import intervalidus.collection.Coordinate2D

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
case class DiscreteDomain2D[T1: DiscreteValue, T2: DiscreteValue](
  horizontalIndex: DiscreteDomain1D[T1],
  verticalIndex: DiscreteDomain1D[T2]
) extends DimensionalBase.DomainLike[DiscreteDomain2D[T1, T2]]:
  def asCoordinate: Coordinate2D =
    Coordinate2D(horizontalIndex.orderedHash, verticalIndex.orderedHash)

  override def toString: String = s"{$horizontalIndex, $verticalIndex}"

  override def toCodeLikeString: String = s"${horizontalIndex.toCodeLikeString} x ${verticalIndex.toCodeLikeString}"

  /**
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo(interval: DiscreteInterval2D[T1, T2]): Boolean = interval contains this

  /**
    * Cross this domain element with that domain element to arrive at a new three-dimensional domain element.
    *
    * @param that
    *   a one-dimensional domain element to be used in the depth dimension.
    * @tparam T3
    *   discrete value type for that domain.
    * @return
    *   a new three-dimensional domain element with this as the horizontal and vertical components and that as the depth
    *   component.
    */
  infix def x[T3: DiscreteValue](that: DiscreteDomain1D[T3]): DiscreteDomain3D[T1, T2, T3] =
    DiscreteDomain3D(horizontalIndex, verticalIndex, that)

  /**
    * Flips this domain by swapping the vertical and horizontal components with one another.
    */
  def flip: DiscreteDomain2D[T2, T1] = DiscreteDomain2D(verticalIndex, horizontalIndex)

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

/**
  * Companion for the two-dimensional domain used in defining and operating on a discrete intervals.
  */
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
