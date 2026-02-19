package intervalidus.collection

import scala.util.Properties

/**
  * Like a box, but with fixed coordinates. Used for calculating the midPoint for splitting and scaling boundaries.
  *
  * @param minPoint
  *   Fixed coordinates of the minimum corner (left/lower/back/etc., depending on dimension)
  * @param maxPoint
  *   Fixed coordinates of the maximum corner (right/upper/front/etc., depending on dimension)
  */
case class Capacity(minPoint: CoordinateFixed, maxPoint: CoordinateFixed):
  require(minPoint.arity == maxPoint.arity, s"minPoint $minPoint and maxPoint $maxPoint must have the same arity")

  /** Number of dimensions */
  def arity: Int = minPoint.arity

  /** Min and max points */
  def corners: (CoordinateFixed, CoordinateFixed) = (minPoint, maxPoint)

  /**
    * The center of the capacity -- the point halfway between the min point and max point.
    */
  val midPoint: CoordinateFixed = minPoint mid maxPoint

  /**
    * Does this capacity fully contain the other capacity?
    *
    * @param other
    *   capacity to test
    * @return
    *   true or false
    */
  def contains(other: Capacity): Boolean = CoordinateFixed.minMaxForall(this.corners, other.corners):
    case (thisMin, thisMax, otherMin, otherMax) =>
      otherMin >= thisMin && otherMax <= thisMax

  /**
    * Does this capacity fully contain the other box (when its unbound elements are fixed to this capacity)?
    *
    * @param box
    *   box to test
    * @return
    *   true or false
    */
  infix def contains(box: Box): Boolean =
    contains(box.fixUnbounded(this))

  /**
    * Create a new capacity that is scaled up by a factor of 2<sup>magnitude</sup> in every dimension.
    *
    * @param magnitude
    *   the scale factor (as a power of two) used for rescaling (positive will scale out, negative will scale in)
    */
  def rescale(magnitude: Double): Capacity =
    Capacity(
      minPoint.scaledFrom(midPoint, magnitude),
      maxPoint.scaledFrom(midPoint, magnitude)
    )

  /**
    * When some other box does not fit in this capacity, returns a new larger capacity containing the other box.
    */
  def growAround(other: Box): Capacity =
    Capacity(other.minPoint.fixMin(minPoint), other.maxPoint.fixMax(maxPoint))

  override def toString: String = s"[${minPoint.asString}..${maxPoint.asString}]"

/**
  * Common definitions for capacities, which are like boxes, but with fixed coordinates.
  */
object Capacity:
  /**
    * Default initial tree boundary capacity size in each dimension. Default is 1.0. It can be overridden by setting the
    * environment variable `INTERVALIDUS_TREE_BOUNDARY_CAPACITY_SIZE`.
    */
  val defaultBoundaryCapacitySize: Double = Properties
    .envOrElse("INTERVALIDUS_TREE_BOUNDARY_CAPACITY_SIZE", "1.0")
    .toDouble
  //   println(s"using initial tree boundary capacity size = defaultBoundaryCapacitySize")

  /**
    * Returns a tight capacity around the origin based on [[defaultBoundaryCapacitySize]].
    * @param arity
    *   number of dimensions
    */
  def aroundOrigin(arity: Int): Capacity =
    Capacity(
      CoordinateFixed.bound(arity, -defaultBoundaryCapacitySize / 2),
      CoordinateFixed.bound(arity, defaultBoundaryCapacitySize / 2)
    )
