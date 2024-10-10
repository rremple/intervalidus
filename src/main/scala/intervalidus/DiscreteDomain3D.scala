package intervalidus

import intervalidus.collection.Coordinate3D

import scala.language.implicitConversions

/**
  * A three-dimensional discrete domain, like cartesian coordinates. Used in conjunction with [[DiscreteInterval3D]].
  *
  * @param horizontalIndex
  *   domain element on the x-axis.
  * @param verticalIndex
  *   domain element on the y-axis.
  * @param depthIndex
  *   domain element on the z-axis.
  * @tparam T1
  *   the horizontal domain type
  * @tparam T2
  *   the vertical domain type
  * @tparam T3
  *   the depth domain type
  */
case class DiscreteDomain3D[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
  horizontalIndex: DiscreteDomain1D[T1],
  verticalIndex: DiscreteDomain1D[T2],
  depthIndex: DiscreteDomain1D[T3]
) extends DimensionalBase.DomainLike[DiscreteDomain3D[T1, T2, T3]]:
  def asCoordinate: Coordinate3D =
    Coordinate3D(horizontalIndex.orderedHash, verticalIndex.orderedHash, depthIndex.orderedHash)

  override def toString: String = s"{$horizontalIndex, $verticalIndex, $depthIndex}"

  override def toCodeLikeString: String =
    s"${horizontalIndex.toCodeLikeString} x ${verticalIndex.toCodeLikeString} x ${depthIndex.toCodeLikeString}"

  /**
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo(interval: DiscreteInterval3D[T1, T2, T3]): Boolean = interval contains this

  /**
    * Flips this domain by swapping the vertical and horizontal components with one another and keeping the same depth
    * component.
    */
  def flipAboutDepth: DiscreteDomain3D[T2, T1, T3] = DiscreteDomain3D(verticalIndex, horizontalIndex, depthIndex)

  /**
    * Flips this domain by swapping the depth and horizontal components with one another and keeping the same vertical
    * component.
    */
  def flipAboutVertical: DiscreteDomain3D[T3, T2, T1] = DiscreteDomain3D(depthIndex, verticalIndex, horizontalIndex)

  /**
    * Flips this domain by swapping the vertical and depth components with one another and keeping the same horizontal
    * component.
    */
  def flipAboutHorizontal: DiscreteDomain3D[T1, T3, T2] = DiscreteDomain3D(horizontalIndex, depthIndex, verticalIndex)

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
  def ∈(interval: DiscreteInterval3D[T1, T2, T3]): Boolean = this belongsTo interval

/**
  * Companion for the three-dimensional domain used in defining and operating on a discrete intervals.
  */
object DiscreteDomain3D:
  /**
    * Provides a default ordering for any 3D domain based on the orderings of its constituent horizontal, vertical, and
    * depth interval components.
    *
    * @param horizontalOrdering
    *   uses ordering of horizontal interval component.
    * @param verticalOrdering
    *   uses ordering of vertical interval component.
    * @param depthOrdering
    *   uses ordering of depth interval component.
    * @tparam T1
    *   the horizontal domain type
    * @tparam T2
    *   the vertical domain type
    * @tparam T3
    *   the depth domain type
    * @return
    *   an ordering for the 3D domain
    */
  given [T1, T2, T3](using
    horizontalOrdering: Ordering[DiscreteDomain1D[T1]],
    verticalOrdering: Ordering[DiscreteDomain1D[T2]],
    depthOrdering: Ordering[DiscreteDomain1D[T3]]
  ): Ordering[DiscreteDomain3D[T1, T2, T3]] with
    override def compare(x: DiscreteDomain3D[T1, T2, T3], y: DiscreteDomain3D[T1, T2, T3]): Int =
      val horizontalCompare = horizontalOrdering.compare(x.horizontalIndex, y.horizontalIndex)
      if horizontalCompare != 0 then horizontalCompare
      else
        val verticalCompare = verticalOrdering.compare(x.verticalIndex, y.verticalIndex)
        if verticalCompare != 0 then verticalCompare
        else depthOrdering.compare(x.depthIndex, y.depthIndex)

  /**
    * This allows a client to use a tuple of discrete values in methods requiring a three-dimensional discrete domain
    * element by implicitly converting. For example, a client can write `dataIn2D.getAt(DiscreteDomain3D(1, 2, 3))` (or
    * even `dataIn2D.getAt(Point(1) x Point(2) x Point(3))`) or, simpler, `dataIn1D.getAt((1, 2, 3))` (or, with
    * auto-tupling, even the extra parens can be dropped: `dataIn1D.getAt(1, 2, 3)`)
    */
  given [T1, T2, T3](using
    DiscreteValue[T1],
    DiscreteValue[T2],
    DiscreteValue[T3]
  ): Conversion[(T1, T2, T3), DiscreteDomain3D[T1, T2, T3]] = (t: (T1, T2, T3)) => DiscreteDomain3D(t._1, t._2, t._3)
