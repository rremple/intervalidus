package intervalidus

import intervalidus.collection.Coordinate

import scala.language.implicitConversions

/**
  * A four-dimensional domain, like cartesian coordinates. Used in conjunction with [[Interval4D]].
  *
  * @param horizontalIndex
  *   domain element on the x-axis.
  * @param verticalIndex
  *   domain element on the y-axis.
  * @param depthIndex
  *   domain element on the z-axis.
  * @param fourthIndex
  *   domain element on the w-axis.
  * @tparam T1
  *   the horizontal domain type
  * @tparam T2
  *   the vertical domain type
  * @tparam T3
  *   the depth domain type
  * @tparam T4
  *   the fourth domain type
  */
case class Domain4D[T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike](
  horizontalIndex: Domain1D[T1],
  verticalIndex: Domain1D[T2],
  depthIndex: Domain1D[T3],
  fourthIndex: Domain1D[T4]
):

  override def toString: String = s"{$horizontalIndex, $verticalIndex, $depthIndex, $fourthIndex}"

/**
  * Companion for the four-dimensional domain used in defining and operating on intervals.
  */
object Domain4D:

  /**
    * Type class instance for four-dimensional domains.
    */
  given [T1: DomainValueLike, T2: DomainValueLike, T3: DomainValueLike, T4: DomainValueLike]
    : DomainLike[Domain4D[T1, T2, T3, T4]] with
    extension (domain: Domain4D[T1, T2, T3, T4])
      override def isUnbounded: Boolean =
        domain.horizontalIndex.isUnbounded && domain.verticalIndex.isUnbounded & domain.depthIndex.isUnbounded & domain.fourthIndex.isUnbounded

      override def toCodeLikeString: String =
        s"${domain.horizontalIndex.toCodeLikeString} x ${domain.verticalIndex.toCodeLikeString} x " +
          s"${domain.depthIndex.toCodeLikeString} x ${domain.fourthIndex.toCodeLikeString}"

      override def asCoordinate: Coordinate =
        Coordinate(
          domain.horizontalIndex.orderedHash,
          domain.verticalIndex.orderedHash,
          domain.depthIndex.orderedHash,
          domain.fourthIndex.orderedHash
        )

      override def rightAdjacent: Domain4D[T1, T2, T3, T4] =
        domain.horizontalIndex.rightAdjacent x
          domain.verticalIndex.rightAdjacent x
          domain.depthIndex.rightAdjacent x
          domain.fourthIndex.rightAdjacent

      override def leftAdjacent: Domain4D[T1, T2, T3, T4] =
        domain.horizontalIndex.leftAdjacent x
          domain.verticalIndex.leftAdjacent x
          domain.depthIndex.leftAdjacent x
          domain.fourthIndex.leftAdjacent

  /**
    * Provides a default ordering for any 4D domain based on the orderings of its constituent horizontal, vertical,
    * depth, and fourth interval components.
    *
    * @param horizontalOrdering
    *   uses ordering of horizontal interval component.
    * @param verticalOrdering
    *   uses ordering of vertical interval component.
    * @param depthOrdering
    *   uses ordering of depth interval component.
    * @param fourthOrdering
    *   uses ordering of fourth interval component.
    * @tparam T1
    *   the horizontal domain type
    * @tparam T2
    *   the vertical domain type
    * @tparam T3
    *   the depth domain type
    * @tparam T4
    *   the fourth domain type
    * @return
    *   an ordering for the 4D domain
    */
  given [T1, T2, T3, T4](using
    horizontalOrdering: Ordering[Domain1D[T1]],
    verticalOrdering: Ordering[Domain1D[T2]],
    depthOrdering: Ordering[Domain1D[T3]],
    fourthOrdering: Ordering[Domain1D[T4]]
  ): Ordering[Domain4D[T1, T2, T3, T4]] with
    override def compare(x: Domain4D[T1, T2, T3, T4], y: Domain4D[T1, T2, T3, T4]): Int =
      val horizontalCompare = horizontalOrdering.compare(x.horizontalIndex, y.horizontalIndex)
      if horizontalCompare != 0 then horizontalCompare
      else
        val verticalCompare = verticalOrdering.compare(x.verticalIndex, y.verticalIndex)
        if verticalCompare != 0 then verticalCompare
        else
          val depthCompare = depthOrdering.compare(x.depthIndex, y.depthIndex)
          if depthCompare != 0 then depthCompare
          else fourthOrdering.compare(x.fourthIndex, y.fourthIndex)

  /**
    * This allows a client to use a tuple of domain values in methods requiring a four-dimensional domain element by
    * implicitly converting. For example, a client can write `dataIn4D.getAt(Domain4D(1, 2, 3, 4))` (or even
    * `dataIn4D.getAt(Point(1) x Point(2) x Point(3) x Point(4))`) or, simpler, `dataIn4D.getAt((1, 2, 3, 4))` (or, with
    * auto-tupling, even the extra parens can be dropped: `dataIn4D.getAt(1, 2, 3, 4)`)
    */
  given [T1, T2, T3, T4](using
    DomainValueLike[T1],
    DomainValueLike[T2],
    DomainValueLike[T3],
    DomainValueLike[T4]
  ): Conversion[(T1, T2, T3, T4), Domain4D[T1, T2, T3, T4]] = (t: (T1, T2, T3, T4)) => Domain4D(t._1, t._2, t._3, t._4)
