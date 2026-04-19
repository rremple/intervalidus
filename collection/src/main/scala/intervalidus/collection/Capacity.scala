package intervalidus.collection

/**
  * Like a box, but with fixed coordinates. Used for calculating the midPoint for splitting and scaling boundaries.
  *
  * @param minPoint
  *   Fixed coordinates of the minimum corner (left/lower/back/etc., depending on dimension)
  * @param maxPoint
  *   Fixed coordinates of the maximum corner (right/upper/front/etc., depending on dimension)
  */
case class Capacity(minPoint: CoordinateFixed, maxPoint: CoordinateFixed):
  require(
    minPoint.arity == maxPoint.arity,
    s"minPoint ${minPoint.asString} and maxPoint ${maxPoint.asString} must have the same arity"
  )

  /**
    * Because the underlying type of CoordinateFixed is Array (semi-opaque), the default `equals` implementations deals
    * with object equality rather than logical equality, so we have to redefine `equals` here to checks for logical
    * equality.
    */
  override def equals(obj: Any): Boolean = obj match
    case that: Capacity =>
      CoordinateFixed.equals(minPoint, that.minPoint) && CoordinateFixed.equals(maxPoint, that.maxPoint)
    case _ => false

  override def hashCode(): Int =
    CoordinateFixed.hashCode(minPoint) * 31 + CoordinateFixed.hashCode(maxPoint)

  /** Number of dimensions */
  def arity: Int = minPoint.arity

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
  def contains(other: Capacity): Boolean =
    CoordinateFixed.contains(this.minPoint, this.maxPoint, other.minPoint, other.maxPoint)

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
    * Default initial tree boundary capacity size in each dimension.
    */
  val defaultBoundaryCapacitySize: Double = 1.0

  /**
    * Returns a tight capacity around the origin.
    * @param arity
    *   number of dimensions
    */
  def aroundOrigin(arity: Int): Capacity =
    Capacity(
      CoordinateFixed.bound(arity, -defaultBoundaryCapacitySize / 2),
      CoordinateFixed.bound(arity, defaultBoundaryCapacitySize / 2)
    )
