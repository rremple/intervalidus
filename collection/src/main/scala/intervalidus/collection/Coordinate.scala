package intervalidus.collection

import java.util
import scala.annotation.tailrec

/**
  * Represents a point or unbound limit in multidimensional double space. Unbounded limits are represented using NaN.
  * Although the underlying type is mutable, Coordinate itself is treated as immutable.
  */
opaque type Coordinate = Array[Double]

/**
  * Common definitions for points/unbound limits in multidimensional double space.
  */
object Coordinate:
  private type MinMaxCoordinates = (Coordinate, Coordinate)

  // Slower, only used in tests
  def apply(coordinates: Option[Double]*): Coordinate =
    coordinates.map(_.getOrElse(Double.NaN)).toArray

  // Faster, no maps
  def apply(coordinates: Array[Double]): Coordinate = coordinates

  /**
    * Because the underlying type is Array, the default `equals` implementations deals with object equality rather than
    * logical equality. Since we use coordinates as case class attributes, we have to be careful to redefine `equals`
    * there to use this method that checks for logical array equality.
    */
  def equals(lhs: Coordinate, rhs: Coordinate): Boolean =
    util.Arrays.equals(lhs, rhs)

  def hashCode(coordinate: Coordinate): Int =
    util.Arrays.hashCode(coordinate)

  /**
    * Construct a coordinate that is unbound in all dimensions.
    *
    * @param arity
    *   number of dimensions
    * @return
    *   a new coordinate
    */
  def unbound(arity: Int): Coordinate = Array.fill(arity)(Double.NaN)

  /**
    * Does a box (described by min/max coordinates) fully contain another box (also described by min/max coordinates)?
    *
    * @param thisMinPoint
    *   box min coordinates
    * @param thisMaxPoint
    *   box max coordinates
    * @param otherMinPoint
    *   other box min coordinates
    * @param otherMaxPoint
    *   other box max coordinates
    * @return
    *   true or false
    */
  def contains(
    thisMinPoint: Coordinate,
    thisMaxPoint: Coordinate,
    otherMinPoint: Coordinate,
    otherMaxPoint: Coordinate
  ): Boolean =
    var i = 0
    var result = true
    while result && i < thisMinPoint.length do // equivalent to "thisMinPoint.indices.forall: i =>"
      val thisMin = thisMinPoint(i)
      def thisMax = thisMaxPoint(i)
      def otherMin = otherMinPoint(i)
      def otherMax = otherMaxPoint(i)
      val otherMinGeqThisMin = thisMin.isNaN || otherMin >= thisMin
      def otherMaxLeqThisMax = thisMax.isNaN || otherMax <= thisMax
      // otherMin >= thisMin && otherMax <= thisMax
      result = otherMinGeqThisMin && otherMaxLeqThisMax
      i = i + 1
    result

  /**
    * Does a box (described by min/max coordinates) intersect another box (also described by min/max coordinates)?
    * (Touching or overlapping.)
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching.
    *
    * @param thisMinPoint
    *   box min coordinates
    * @param thisMaxPoint
    *   box max coordinates
    * @param otherMinPoint
    *   other box min coordinates
    * @param otherMaxPoint
    *   other box max coordinates
    * @return
    *   true or false
    */
  def intersects(
    thisMinPoint: Coordinate,
    thisMaxPoint: Coordinate,
    otherMinPoint: Coordinate,
    otherMaxPoint: Coordinate
  ): Boolean =
    var i = 0
    var result = true
    while result && i < thisMinPoint.length do // equivalent to "thisMinPoint.indices.forall: i =>"
      def thisMin = thisMinPoint(i)
      def thisMax = thisMaxPoint(i)
      val otherMin = otherMinPoint(i)
      def otherMax = otherMaxPoint(i)
      val otherMinLeqThisMax = otherMin.isNaN || thisMax.isNaN || otherMin <= thisMax
      def otherMaxGeqThisMin = otherMax.isNaN || thisMin.isNaN || otherMax >= thisMin
      // otherMin <= thisMax && otherMax >= thisMin
      result = otherMinLeqThisMax && otherMaxGeqThisMin
      i = i + 1
    result

  extension (coordinates: Coordinate)
    /** Number of dimensions */
    def arity: Int = coordinates.length

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded are fixed to
      * the boundary. Useful when rescaling a boundary capacity based on a box not strictly contained within it.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound elements fixed to the boundary
      */
    def fixUnbounded(boundary: CoordinateFixed): CoordinateFixed = boundary.fixUnbounded(coordinates)

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded or greater
      * than the boundary are fixed to the boundary.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound or larger elements fixed to the boundary
      */
    def fixMin(boundary: CoordinateFixed): CoordinateFixed = boundary.fixMin(coordinates)

    /**
      * Fix these coordinates based on some fixed boundary coordinates. Any coordinates that are unbounded or less than
      * the boundary are fixed to the boundary.
      * @param boundary
      *   fixed coordinates representing a boundary capacity minimum or maximum.
      * @return
      *   these coordinates with unbound or smaller elements fixed to the boundary
      */
    def fixMax(boundary: CoordinateFixed): CoordinateFixed = boundary.fixMax(coordinates)

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
        minMidMax: Vector[(Double, Double, Double)],
        protoBoxes: Vector[MinMaxCoordinates] = Vector.empty
      ): Vector[MinMaxCoordinates] = minMidMax.headOption match
        case Some((min, mid, max)) =>
          def growProtoBox(b: MinMaxCoordinates, appendMin: Double, appendMax: Double) = b match
            case (protoMin, protoMax) => (protoMin.appended(appendMin), protoMax.appended(appendMax))

          val newProtoBoxes =
            if protoBoxes.isEmpty then Vector((Array(min), Array(mid)), (Array(mid), Array(max)))
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
    def projectBeforeStart(other: Coordinate): Coordinate =
      val result = coordinates.clone() // defaults preserved when these coordinates are unbound (isNaN)
      result.indices.foreach: i =>
        if !result(i).isNaN then result(i) = result(i) min other(i)
      result

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
    def projectBeforeEnd(other: Coordinate): Coordinate =
      val result = coordinates.clone() // defaults preserved when other coordinates are unbound (isNaN)
      result.indices.foreach: i =>
        if !other(i).isNaN then
          result(i) =
            if result(i).isNaN
            then other(i)
            else result(i) min other(i)
      result

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
    def projectAfterEnd(other: Coordinate): Coordinate =
      val result = coordinates.clone() // defaults preserved when these coordinates are unbound (isNaN)
      result.indices.foreach: i =>
        if !result(i).isNaN then result(i) = result(i) max other(i)
      result

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
    def projectAfterStart(other: Coordinate): Coordinate =
      val result = coordinates.clone() // defaults preserved when other coordinates are unbound (isNaN)
      result.indices.foreach: i =>
        if !other(i).isNaN then
          result(i) =
            if result(i).isNaN
            then other(i)
            else result(i) max other(i)
      result

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name. Only used by
      * Box.toString / require.
      */
    def asString: String =
      val strings = coordinates.map(c => if c.isNaN then "<unbounded>" else c.toString)
      if arity == 1 then strings(0) else strings.mkString("(", ",", ")")
