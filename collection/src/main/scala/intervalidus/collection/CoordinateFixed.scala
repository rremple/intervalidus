package intervalidus.collection

/**
  * Represents a point in multidimensional double space. All elements are fixed -- no unbounded.
  */
opaque type CoordinateFixed = Vector[Double]

object CoordinateFixed:
  private type MinMaxCoordinates = (CoordinateFixed, CoordinateFixed)

  def apply(coordinates: Double*): CoordinateFixed = coordinates.toVector

  def apply(coordinates: Vector[Double]): CoordinateFixed = coordinates

  /**
    * Construct a coordinate that is bound to a specific value in all dimensions.
    *
    * @param arity
    *   number of dimensions
    * @param value
    *   value of coordinate in all dimensions
    * @return
    *   a new fixed coordinate
    */
  def bound(arity: Int, value: Double): CoordinateFixed = Vector.fill(arity)(value)

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

  extension (coordinates: CoordinateFixed)
    private def combineWith(other: CoordinateFixed)(
      f: (Double, Double) => Double
    ): CoordinateFixed = coordinates.indices.toVector.map: i =>
      f(coordinates(i), other(i))

    /** Number of dimensions */
    def arity: Int = coordinates.size

    /**
      * Returns the coordinate value in the specified dimension. This breaks encapsulation, but it is necessary for
      * [[Coordinate]] when fixing to a boundary.
      *
      * @param dimension
      *   dimension of the coordinate value to return
      */
    def apply(dimension: Int): Double = coordinates(dimension)

    /**
      * Return this fixed coordinate as an unfixed coordinate.
      */
    def unfix: Coordinate = Coordinate(coordinates.map(Some(_)))

    /**
      * Returns midpoint between this and other.
      *
      * @param other
      *   coordinate
      * @return
      *   midpoint
      */
    infix def mid(other: CoordinateFixed): CoordinateFixed = combineWith(other)(_ / 2.0 + _ / 2.0)

    /**
      * Using this as a boundary point (such as a new minimum or new maximum), returns the coordinate that is scaled the
      * distance from the provided midpoint in all dimensions by 2<sup>scaleFactor</sup>. Used by box trees to scale out
      * boundary boxes.
      * @note
      *   does not check for overflow
      *
      * @param midPoint
      *   the midpoint coordinate from which to rescale
      * @param magnitude
      *   the scale factor (as a power of two) used for rescaling (positive will scale out, negative will scale in)
      */
    def scaledFrom(midPoint: CoordinateFixed, magnitude: Double): CoordinateFixed = combineWith(midPoint): (x, xMid) =>
      (x - xMid) * math.pow(2.0, magnitude) + xMid

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name. Only used by
      * Capacity.toString
      */
    def asString: String =
      if arity == 1 then coordinates(0).toString else coordinates.mkString("(", ",", ")")
