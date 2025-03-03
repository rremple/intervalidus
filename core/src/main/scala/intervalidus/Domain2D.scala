package intervalidus

import intervalidus.collection.Coordinate

import scala.language.implicitConversions

/**
  * A two-dimensional domain, like cartesian coordinates. Used in conjunction with [[Interval2D]].
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
case class Domain2D[T1: DomainValueLike, T2: DomainValueLike](
  horizontalIndex: Domain1D[T1],
  verticalIndex: Domain1D[T2]
):

  override def toString: String = s"{$horizontalIndex, $verticalIndex}"

  /**
    * Cross this domain element with that domain element to arrive at a new three-dimensional domain element.
    *
    * @param that
    *   a one-dimensional domain element to be used as the depth dimension.
    * @tparam T3
    *   domain value type for that domain.
    * @return
    *   a new three-dimensional domain element with this as the horizontal and vertical components and that as the depth
    *   component.
    */
  infix def x[T3: DomainValueLike](that: Domain1D[T3]): Domain3D[T1, T2, T3] =
    Domain3D(horizontalIndex, verticalIndex, that)

  /**
    * Flips this domain by swapping the vertical and horizontal components with one another.
    */
  def flip: Domain2D[T2, T1] = Domain2D(verticalIndex, horizontalIndex)

/**
  * Companion for the two-dimensional domain used in defining and operating on intervals.
  */
object Domain2D:

  /**
    * Type class instance for two-dimensional domains.
    */
  given [T1: DomainValueLike, T2: DomainValueLike]: DomainLike[Domain2D[T1, T2]] with
    extension (domain: Domain2D[T1, T2])
      override def isUnbounded: Boolean =
        domain.horizontalIndex.isUnbounded && domain.verticalIndex.isUnbounded

      override def toCodeLikeString: String =
        s"${domain.horizontalIndex.toCodeLikeString} x ${domain.verticalIndex.toCodeLikeString}"

      override def asCoordinate: Coordinate =
        Coordinate(domain.horizontalIndex.orderedHash, domain.verticalIndex.orderedHash)

      override def rightAdjacent: Domain2D[T1, T2] =
        domain.horizontalIndex.rightAdjacent x domain.verticalIndex.rightAdjacent

      override def leftAdjacent: Domain2D[T1, T2] =
        domain.horizontalIndex.leftAdjacent x domain.verticalIndex.leftAdjacent

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
    horizontalOrdering: Ordering[Domain1D[T1]],
    verticalOrdering: Ordering[Domain1D[T2]]
  ): Ordering[Domain2D[T1, T2]] with
    override def compare(x: Domain2D[T1, T2], y: Domain2D[T1, T2]): Int =
      val horizontalCompare = horizontalOrdering.compare(x.horizontalIndex, y.horizontalIndex)
      if horizontalCompare != 0 then horizontalCompare
      else verticalOrdering.compare(x.verticalIndex, y.verticalIndex)

  /**
    * This allows a client to use a tuple of domain values in methods requiring a two-dimensional domain element by
    * implicitly converting. For example, a client can write `dataIn2D.getAt(Domain2D(1, 2))` (or even
    * `dataIn2D.getAt(Point(1) x Point(2))`) or, simpler, `dataIn1D.getAt((1, 2))` (or, with auto-tupling, even the
    * extra parens can be dropped: `dataIn1D.getAt(1, 2)`)
    */
  given [T1, T2](using DomainValueLike[T1], DomainValueLike[T2]): Conversion[(T1, T2), Domain2D[T1, T2]] =
    (t: (T1, T2)) => Domain2D(t._1, t._2)
