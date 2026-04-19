package intervalidus.collection

import java.util

/**
  * Represents a point in multidimensional double space. All elements are fixed -- no unbounded (NaN). Although the
  * underlying type is mutable, CoordinateFixed itself is treated as immutable.
  */
opaque type CoordinateFixed = Array[Double]

/**
  * Common definitions for points in multidimensional double space.
  */
object CoordinateFixed:
  private type MinMaxCoordinates = (CoordinateFixed, CoordinateFixed)

  // Slower, only used in tests
  def apply(coordinates: Double*): CoordinateFixed =
    require(coordinates.forall(!_.isNaN), s"$coordinates must not include NaN")
    coordinates.toArray

  // Faster, no checks
  def apply(coordinates: Array[Double]): CoordinateFixed = coordinates

  /**
    * Because the underlying type is Array, the default `equals` implementations deals with object equality rather than
    * logical equality. Since we use fixed coordinates as case class attributes, we have to be careful to redefine
    * `equals` there to use this method that checks for logical array equality.
    */
  def equals(lhs: CoordinateFixed, rhs: CoordinateFixed): Boolean =
    util.Arrays.equals(lhs, rhs)

  def hashCode(coordinate: CoordinateFixed): Int =
    util.Arrays.hashCode(coordinate)

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
  def bound(arity: Int, value: Double): CoordinateFixed = Array.fill(arity)(value)

  /**
    * Does a capacity (described by min/max fixed coordinates) fully contain another capacity (also described by min/max
    * fixed coordinates)?
    *
    * @param thisMinPoint
    *   capacity min coordinates
    * @param thisMaxPoint
    *   capacity max coordinates
    * @param otherMinPoint
    *   other capacity min coordinates
    * @param otherMaxPoint
    *   other capacity max coordinates
    * @return
    *   true or false
    */
  def contains(
    thisMinPoint: CoordinateFixed,
    thisMaxPoint: CoordinateFixed,
    otherMinPoint: CoordinateFixed,
    otherMaxPoint: CoordinateFixed
  ): Boolean = thisMinPoint.indices.forall: i =>
    otherMinPoint(i) >= thisMinPoint(i) && otherMaxPoint(i) <= thisMaxPoint(i)

  extension (coordinates: CoordinateFixed)
    /** Number of dimensions */
    def arity: Int = coordinates.length

    /**
      * @note
      *   This is the double-dispatch method that should not be called directly. Start with the method of the same name
      *   in Coordinate.
      *
      * Any coordinates that are unbounded are fixed to these coordinates. Useful when rescaling a boundary capacity
      * based on a box not strictly contained within it.
      *
      * @param unfixed
      *   an array of unfixed coordinates
      * @return
      *   coordinates with unbound elements fixed to these coordinates
      */
    def fixUnbounded(unfixed: Array[Double]): CoordinateFixed =
      val result = unfixed.clone() // defaults preserved when unfixed are bound (not isNaN)
      result.indices.foreach: i =>
        if result(i).isNaN then result(i) = coordinates(i)
      CoordinateFixed(result)

    /**
      * @note
      *   This is the double-dispatch method that should not be called directly. Start with the method of the same name
      *   in Coordinate.
      *
      * Any coordinates that are unbounded or greater than these coordinates are fixed to these coordinates.
      *
      * @param unfixed
      *   an array of unfixed coordinates
      * @return
      *   coordinates with unbound or larger elements fixed to these coordinates
      */
    def fixMin(unfixed: Array[Double]): CoordinateFixed =
      val result = unfixed.clone() // defaults preserved when unfixed are bound (not isNaN) and <= these coordinates
      result.indices.foreach: i =>
        if unfixed(i).isNaN || unfixed(i) > coordinates(i)
        then result(i) = coordinates(i)
      CoordinateFixed(result)

    /**
      * @note
      *   This is the double-dispatch method that should not be called directly. Start with the method of the same name
      *   in Coordinate.
      *
      * Any coordinates that are unbounded or less than these coordinates are fixed to these coordinates.
      *
      * @param unfixed
      *   an array of unfixed coordinates
      * @return
      *   coordinates with unbound or smaller elements fixed to these coordinates
      */
    def fixMax(unfixed: Array[Double]): CoordinateFixed =
      val result = unfixed.clone() // defaults preserved when unfixed are bound (not isNaN) and >= these coordinates
      result.indices.foreach: i =>
        if unfixed(i).isNaN || unfixed(i) < coordinates(i)
        then result(i) = coordinates(i)
      CoordinateFixed(result)

    /**
      * Return this fixed coordinate as an unfixed coordinate.
      */
    def unfix: Coordinate = Coordinate(coordinates)

    /**
      * Returns midpoint between this and other.
      *
      * @param other
      *   coordinate
      * @return
      *   midpoint
      */
    infix def mid(other: CoordinateFixed): CoordinateFixed =
      val result = new Array[Double](arity)
      coordinates.indices.foreach: i =>
        result(i) = coordinates(i) / 2.0 + other(i) / 2.0
      result

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
    def scaledFrom(midPoint: CoordinateFixed, magnitude: Double): CoordinateFixed =
      val result = new Array[Double](arity)
      coordinates.indices.foreach: i =>
        result(i) = (coordinates(i) - midPoint(i)) * math.pow(2.0, magnitude) + midPoint(i)
      result

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name. Only used by
      * Capacity.toString / require.
      */
    def asString: String =
      if arity == 1 then coordinates(0).toString else coordinates.mkString("(", ",", ")")
