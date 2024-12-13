package intervalidus.collection

/**
  * Represents a point in multidimensional double space.
  */
opaque type Coordinate = Vector[Double]

object Coordinate:
  def apply(coordinates: Double*): Coordinate = coordinates.toVector

  def apply(coordinates: Iterable[Double]): Coordinate = coordinates.toVector

  extension (coordinates: Coordinate)
    def zip[T](other: Iterable[T]): Vector[(Double, T)] =
      coordinates.zip(other)
    def zip(other: Coordinate): Vector[(Double, Double)] =
      coordinates.zip(other)

    private def combineWith(other: Coordinate)(f: ((Double, Double)) => Double) =
      coordinates.zip(other).map(f)

    /**
      * Returns the point where each dimension is the min of this and the other point. It will be less than or equal to
      * the other point in every dimension.
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
      * Returns the point where each dimension is the max of this and the other point. It will be greater than or equal
      * to the other point in every dimension.
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
