package intervalidus

import intervalidus.collection.Coordinate

import scala.language.implicitConversions

/**
  * A three-dimensional domain, like cartesian coordinates. Used in conjunction with [[Interval3D]].
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
case class Domain3D[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike](
  horizontalIndex: Domain1D[T1],
  verticalIndex: Domain1D[T2],
  depthIndex: Domain1D[T3]
):

  override def toString: String = s"{$horizontalIndex, $verticalIndex, $depthIndex}"

  /**
    * Cross this domain element with that domain element to arrive at a new four-dimensional domain element.
    *
    * @param that
    *   a one-dimensional domain element to be used as the fourth dimension.
    * @tparam T4
    *   domain value type for that domain.
    * @return
    *   a new four-dimensional domain element with this as the horizontal and vertical components and that as the depth
    *   component.
    */
  infix def x[T4: DomainValueLike](that: Domain1D[T4]): Domain4D[T1, T2, T3, T4] =
    Domain4D(horizontalIndex, verticalIndex, depthIndex, that)

  /**
    * Flips this domain by swapping the vertical and horizontal components with one another and keeping the same depth
    * component.
    */
  def flipAboutDepth: Domain3D[T2, T1, T3] = Domain3D(verticalIndex, horizontalIndex, depthIndex)

  /**
    * Flips this domain by swapping the depth and horizontal components with one another and keeping the same vertical
    * component.
    */
  def flipAboutVertical: Domain3D[T3, T2, T1] = Domain3D(depthIndex, verticalIndex, horizontalIndex)

  /**
    * Flips this domain by swapping the vertical and depth components with one another and keeping the same horizontal
    * component.
    */
  def flipAboutHorizontal: Domain3D[T1, T3, T2] = Domain3D(horizontalIndex, depthIndex, verticalIndex)

/**
  * Companion for the three-dimensional domain used in defining and operating on intervals.
  */
object Domain3D:

  /**
    * Type class instance for three-dimensional domains.
    */
  given [T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike]: DomainLike[Domain3D[T1, T2, T3]] with
    extension (domain: Domain3D[T1, T2, T3])
      override def isUnbounded: Boolean =
        domain.horizontalIndex.isUnbounded && domain.verticalIndex.isUnbounded & domain.depthIndex.isUnbounded

      override def toCodeLikeString: String =
        s"${domain.horizontalIndex.toCodeLikeString} x ${domain.verticalIndex.toCodeLikeString} x " +
          s"${domain.depthIndex.toCodeLikeString}"

      override def asCoordinate: Coordinate =
        Coordinate(
          domain.horizontalIndex.orderedHash,
          domain.verticalIndex.orderedHash,
          domain.depthIndex.orderedHash
        )

      override def rightAdjacent: Domain3D[T1, T2, T3] =
        domain.horizontalIndex.rightAdjacent x
          domain.verticalIndex.rightAdjacent x
          domain.depthIndex.rightAdjacent

      override def leftAdjacent: Domain3D[T1, T2, T3] =
        domain.horizontalIndex.leftAdjacent x
          domain.verticalIndex.leftAdjacent x
          domain.depthIndex.leftAdjacent

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
    horizontalOrdering: Ordering[Domain1D[T1]],
    verticalOrdering: Ordering[Domain1D[T2]],
    depthOrdering: Ordering[Domain1D[T3]]
  ): Ordering[Domain3D[T1, T2, T3]] with
    override def compare(x: Domain3D[T1, T2, T3], y: Domain3D[T1, T2, T3]): Int =
      val horizontalCompare = horizontalOrdering.compare(x.horizontalIndex, y.horizontalIndex)
      if horizontalCompare != 0 then horizontalCompare
      else
        val verticalCompare = verticalOrdering.compare(x.verticalIndex, y.verticalIndex)
        if verticalCompare != 0 then verticalCompare
        else depthOrdering.compare(x.depthIndex, y.depthIndex)

  /**
    * This allows a client to use a tuple of domain values in methods requiring a three-dimensional domain element by
    * implicitly converting. For example, a client can write `dataIn3D.getAt(Domain3D(1, 2, 3))` (or even
    * `dataIn3D.getAt(Point(1) x Point(2) x Point(3))`) or, simpler, `dataIn3D.getAt((1, 2, 3))` (or, with auto-tupling,
    * even the extra parens can be dropped: `dataIn3D.getAt(1, 2, 3)`)
    */
  given [T1, T2, T3](using
    DomainValueLike[T1],
    DomainValueLike[T2],
    DomainValueLike[T3]
  ): Conversion[(T1, T2, T3), Domain3D[T1, T2, T3]] = (t: (T1, T2, T3)) => Domain3D(t._1, t._2, t._3)
