package intervalidus.collection

import scala.annotation.tailrec

/**
  * Represents a point in multidimensional double space.
  */
opaque type Coordinate = Vector[Double]

object Coordinate:
  private type MinMaxCoordinates = (Coordinate, Coordinate)

  def apply(coordinates: Double*): Coordinate = coordinates.toVector

  def apply(coordinates: Vector[Double]): Coordinate = coordinates

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
    f: (Double, Double, Double, Double) => Boolean
  ): Boolean = (minMax1, minMax2) match
    case ((min1, max1), (min2, max2)) =>
      min1.indices.forall: i =>
        f(min1(i), max1(i), min2(i), max2(i))

  extension (coordinates: Coordinate)
    private def combineWith(other: Coordinate)(
      f: (Double, Double) => Double
    ): Coordinate = coordinates.indices.toVector.map: i =>
      f(coordinates(i), other(i))

    private def combineWithBounded(other: Coordinate, fromUnbounded: Boolean*)(
      f: (Double, Double) => Double
    ): Coordinate = coordinates.indices.toVector.map: i =>
      if fromUnbounded(i) then coordinates(i) else f(coordinates(i), other(i))

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

      val otherMid = mid(other)
      val minMidMax = coordinates.indices.toVector.map: i =>
        (coordinates(i), otherMid(i), other(i))

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
      * Used by Visualize3D for clipping the boxed coordinate space plotted. Returns the point where each dimension is
      * the min of this and the other point excluding values derived from unbounded domain value (Top or Bottom). It is
      * less than or equal to the other point in every dimension, apart from these exclusions.
      *
      * @param other
      *   coordinate
      * @param fromUnbounded
      *   a corresponding sequence of booleans indicating if the coordinate component was derived from an unbounded
      *   domain value.
      * @return
      *   a new coordinate point with minimum coordinates, apart from these exclusions.
      */
    def projectBeforeBounded(other: Coordinate, fromUnbounded: Boolean*): Coordinate =
      combineWithBounded(other, fromUnbounded*)(_ min _)

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
      *   a new coordinate point with maximum coordinates.
      */
    def projectAfter(other: Coordinate): Coordinate = combineWith(other)(_ max _)

    /**
      * Used by Visualize3D for clipping the boxed coordinate space plotted. Returns the point where each dimension is
      * the max of this and the other point excluding values derived from unbounded domain value (Top or Bottom). It is
      * greater than or equal to the other point in every dimension, apart from these exclusions.
      *
      * @param other
      *   coordinate
      * @param fromUnbounded
      *   a corresponding sequence of booleans indicating if the coordinate component was derived from an unbounded
      *   domain value.
      * @return
      *   a new coordinate point with maximum coordinates, apart from these exclusions.
      */
    def projectAfterBounded(other: Coordinate, fromUnbounded: Boolean*): Coordinate =
      combineWithBounded(other, fromUnbounded*)(_ max _)

    // can't override toString in an extension method -- only used by Box.toString
    def asString: String =
      if coordinates.size == 1 then coordinates(0).toString else coordinates.mkString("(", ",", ")")

    /**
      * Used by Visualize3D for clipping the boxed coordinate space plotted. Returns a string representation of this
      * coordinate using JavaScript array syntax clipped within a more limited coordinate space.
      * @param clipWithin
      *   clipped coordinate space to consider
      * @return
      *   a string representation suitable to use as a visualization URL parameter.
      */
    def toUrlFragment(clipWithin: Box): String = coordinates
      .projectAfter(clipWithin.minPoint)
      .projectBefore(clipWithin.maxPoint)
      .mkString("[", ",", "]")

    /**
      * Translate this point (vector addition).
      * @param other
      *   a coordinate to add.
      * @return
      *   The sum of the two coordinates as vectors.
      */
    infix def +(other: Coordinate): Coordinate = combineWith(other)(_ + _)

    /**
      * Translate this point (vector subtraction).
      * @param other
      *   a coordinate to subtract.
      * @return
      *   The difference of the two coordinates as vectors.
      */
    infix def -(other: Coordinate): Coordinate = combineWith(other)(_ - _)
