package intervalidus.collection

import scala.annotation.tailrec

/**
  * Represents a point in multidimensional double space.
  */
opaque type Coordinate = Vector[Double]

object Coordinate:
  private type MinMaxCoordinates = (Coordinate, Coordinate)

  def apply(coordinates: Double*): Coordinate = coordinates.toVector

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
    f: (((Double, Double), (Double, Double))) => Boolean
  ): Boolean = (minMax1, minMax2) match
    case ((min1, max1), (min2, max2)) => min1.zip(max1).zip(min2.zip(max2)).forall(f)

  extension (coordinates: Coordinate)
    private def combineWith(other: Coordinate)(f: ((Double, Double)) => Double) =
      coordinates.zip(other).map(f)

    /**
      * Treating the other coordinate as the opposite corner of a box, return a vector of coordinate pairs that
      * represent the corners of boxes which make a binary split in all dimensions.
      *
      * @param other
      *   coordinate of the opposite corner of a box
      * @return
      *   pairs of vectors for constructing boxes that split the space
      */
    def binarySplit(other: Coordinate): Vector[MinMaxCoordinates] =
      @tailrec
      def helper(
        minMidMax: Vector[(Double, Double, Double)],
        protoBoxes: Vector[MinMaxCoordinates] = Vector.empty
      ): Vector[MinMaxCoordinates] = minMidMax.headOption match
        case Some((min, mid, max)) =>
          def growProtoBox(b: MinMaxCoordinates, appendMin: Double, appendMax: Double) = b match
            case (protoMin, protoMax) => (protoMin.appended(appendMin), protoMax.appended(appendMax))

          val newProtoBoxes =
            if protoBoxes.isEmpty then Vector((Vector(min), Vector(mid)), (Vector(mid), Vector(max)))
            else protoBoxes.map(growProtoBox(_, min, mid)) ++ protoBoxes.map(growProtoBox(_, mid, max))
          helper(minMidMax.tail, newProtoBoxes)

        case None => protoBoxes

      val minMidMax = coordinates
        .zip(other)
        .zip(mid(other))
        .map:
          case ((min, max), mid) => (min, mid, max)

      helper(minMidMax)

    /**
      * Returns the point where each dimension is the min of this and the other point. It is less than or equal to the
      * other point in every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with minimum coordinates.
      */
    def projectBefore(other: Coordinate): Coordinate = combineWith(other)(_ min _)

    /**
      * Returns midpoint between this and other.
      *
      * @param other
      *   coordinate
      * @return
      *   midpoint
      */
    infix def mid(other: Coordinate): Coordinate = combineWith(other)(_ / 2.0 + _ / 2.0)

    /**
      * Returns the point where each dimension is the max of this and the other point. It is greater than or equal to
      * the other point in every dimension.
      *
      * @param other
      *   coordinate
      * @return
      *   a new coordinate point with minimum coordinates.
      */
    def projectAfter(other: Coordinate): Coordinate = combineWith(other)(_ max _)

    // can't override toString in an extension method -- only used by Box.toString
    def asString: String =
      if coordinates.size == 1 then coordinates(0).toString else coordinates.mkString("(", ",", ")")
