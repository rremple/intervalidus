package intervalidus.collection

import scala.annotation.tailrec

/**
  * Represents a point or unbound limit in multidimensional double space. Unbounded limits are represented using None.
  */
opaque type Coordinate = Vector[Option[Double]]

/**
  * Common definitions for points/unbound limits in multidimensional double space.
  */
object Coordinate:
  private type MinMaxCoordinates = (Coordinate, Coordinate)

  def apply(coordinates: Option[Double]*): Coordinate = coordinates.toVector

  def apply(coordinates: Vector[Option[Double]]): Coordinate = coordinates

  /**
    * Construct a coordinate that is unbound in all dimensions.
    *
    * @param arity
    *   number of dimensions
    * @return
    *   a new coordinate
    */
  def unbound(arity: Int): Coordinate = Vector.fill(arity)(None)

  /**
    * Evaluate each corresponding dimension of the two arguments and only return true if the function applied to each
    * pair returns true.
    *
    * @param minMax1
    *   the first min and max coordinate pair to evaluate
    * @param minMax2
    *   the second min and max coordinate pair to evaluate
    * @param f
    *   function applied to each corresponding dimension of the two min/max coordinate pairs
    * @return
    *   true if f applied to each corresponding dimension of the coordinate pairs is true, false otherwise
    */
  def minMaxForall(minMax1: MinMaxCoordinates, minMax2: MinMaxCoordinates)(
    f: (Option[Double], Option[Double], Option[Double], Option[Double]) => Boolean
  ): Boolean = (minMax1, minMax2) match
    case ((min1, max1), (min2, max2)) =>
      min1.indices.forall: i =>
        f(min1(i), max1(i), min2(i), max2(i))

  extension (coordinates: Coordinate)
    private def combineWith(other: Coordinate)(
      f: (Option[Double], Option[Double]) => Option[Double]
    ): Coordinate = coordinates.indices.toVector.map: i =>
      f(coordinates(i), other(i))

    /** Number of dimensions */
    def arity: Int = coordinates.size

    private def fixWith(boundary: CoordinateFixed, f: (Double, Double) => Double): CoordinateFixed = CoordinateFixed(
      coordinates.indices.toVector.map: i =>
        f(coordinates(i).getOrElse(boundary(i)), boundary(i))
    )

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded are fixed to
      * the boundary. Useful when rescaling a boundary capacity based on a box not strictly contained within it.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound elements fixed to the boundary
      */
    def fixUnbounded(boundary: CoordinateFixed): CoordinateFixed = fixWith(boundary, (keepLeft, _) => keepLeft)

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded or greater
      * than the boundary are fixed to the boundary.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound or larger elements fixed to the boundary
      */
    def fixMin(boundary: CoordinateFixed): CoordinateFixed = fixWith(boundary, _ min _)

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded or less than
      * the boundary are fixed to the boundary.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound or smaller elements fixed to the boundary
      */
    def fixMax(boundary: CoordinateFixed): CoordinateFixed = fixWith(boundary, _ max _)

    /**
      * Treating the other coordinate as the opposite corner of a box, return a vector of coordinate pairs that
      * represent the corners of boxes which make a binary split in all dimensions.
      *
      * @param midPoint
      *   coordinate of the midpoint to the opposite corner, as calculated from the boundary capacity
      * @param other
      *   coordinate of the opposite corner
      * @return
      *   pairs of coordinates for constructing boxes that split the space
      */
    def binarySplit(midPoint: Coordinate, other: Coordinate): Vector[MinMaxCoordinates] =
      @tailrec
      def helper(
        minMidMax: Vector[(Option[Double], Option[Double], Option[Double])],
        protoBoxes: Vector[MinMaxCoordinates] = Vector.empty
      ): Vector[MinMaxCoordinates] = minMidMax.headOption match
        case Some((min, mid, max)) =>
          def growProtoBox(b: MinMaxCoordinates, appendMin: Option[Double], appendMax: Option[Double]) = b match
            case (protoMin, protoMax) => (protoMin.appended(appendMin), protoMax.appended(appendMax))

          val newProtoBoxes =
            if protoBoxes.isEmpty then Vector((Vector(min), Vector(mid)), (Vector(mid), Vector(max)))
            else protoBoxes.map(growProtoBox(_, min, mid)) ++ protoBoxes.map(growProtoBox(_, mid, max))
          helper(minMidMax.tail, newProtoBoxes)

        case None => protoBoxes

      val minMidMax = coordinates.indices.toVector.map: i =>
        (coordinates(i), midPoint(i), other(i))

      helper(minMidMax)

    /**
      * Returns the point where each dimension is the min of this and the other point. Treats unbounded coordinates as
      * smaller than bounded coordinates, suitable for a box minPoint. It is less than or equal to the other point in
      * every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with minimum coordinates.
      */
    def projectBeforeStart(other: Coordinate): Coordinate = combineWith(other):
      case (Some(t), Some(o)) => Some(t min o)
      case _                  => None

    /**
      * Returns the point where each dimension is the min of this and the other point. Treats unbounded coordinates as
      * larger than bounded coordinates, suitable for a box maxPoint. It is less than or equal to the other point in
      * every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with minimum coordinates.
      */
    def projectBeforeEnd(other: Coordinate): Coordinate = combineWith(other):
      case (None, None)       => None
      case (Some(t), Some(o)) => Some(t min o)
      case (someT, None)      => someT
      case (None, someO)      => someO

    /**
      * Returns the point where each dimension is the max of this and the other point. Treats unbounded coordinates as
      * larger than bounded coordinates, suitable for a box maxPoint. It is greater than or equal to the other point in
      * every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with maximum coordinates.
      */
    def projectAfterEnd(other: Coordinate): Coordinate = combineWith(other):
      case (Some(t), Some(o)) => Some(t max o)
      case _                  => None

    /**
      * Returns the point where each dimension is the max of this and the other point. Treats unbounded coordinates as
      * smaller than bounded coordinates, suitable for a box minPoint. It is greater than or equal to the other point in
      * every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with maximum coordinates.
      */
    def projectAfterStart(other: Coordinate): Coordinate = combineWith(other):
      case (None, None)       => None
      case (Some(t), Some(o)) => Some(t max o)
      case (someT, None)      => someT
      case (None, someO)      => someO

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name. Only used by
      * Box.toString
      */
    def asString: String =
      val strings = coordinates.map(_.map(_.toString).getOrElse("<unbounded>"))
      if arity == 1 then strings(0) else strings.mkString("(", ",", ")")
