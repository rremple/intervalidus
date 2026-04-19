package intervalidus.collection

/**
  * Represents a box in multidimensional double space. It could be used as a bounding box that defines the boundary of a
  * tree node, or it could be used as a valid region associated with some payload.
  *
  * @param minPoint
  *   Coordinate of the minimum corner (left/lower/back/etc., depending on dimension)
  * @param maxPoint
  *   Coordinate of the maximum corner (right/upper/front/etc., depending on dimension)
  */
case class Box(minPoint: Coordinate, maxPoint: Coordinate):
  require(
    minPoint.arity == maxPoint.arity,
    s"minPoint ${minPoint.asString} and maxPoint ${maxPoint.asString} must have the same arity"
  )

  /*
   * Because the underlying type of Coordinate is Array (opaque), the default `equals` and `hashCode` implementations
   * deal with object equality rather than logical value equality, so we have to redefine them here to checks equality
   * correctly (including NaN == NaN).
   */

  // cached to make equals faster when this != that
  private val cachedHashCode: Int = Coordinate.hashCode(minPoint) * 31 + Coordinate.hashCode(maxPoint)

  override def hashCode(): Int = cachedHashCode

  override def equals(obj: Any): Boolean = obj match
    case that: Box =>
      hashCode == that.hashCode &&
      Coordinate.equals(minPoint, that.minPoint) && Coordinate.equals(maxPoint, that.maxPoint)
    case _ => false

  /** Number of dimensions */
  def arity: Int = minPoint.arity

  /** Fix unbound coordinates to the coordinates of the given fixed capacity */
  def fixUnbounded(capacity: Capacity): Capacity =
    Capacity(minPoint.fixUnbounded(capacity.minPoint), maxPoint.fixUnbounded(capacity.maxPoint))

  /**
    * Does this box fully contain the other box?
    *
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  infix def contains(other: Box): Boolean = Coordinate.contains(minPoint, maxPoint, other.minPoint, other.maxPoint)

  /**
    * Does this box intersect the other box? (Touching or overlapping.)
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching.
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  infix def intersects(other: Box): Boolean = Coordinate.intersects(minPoint, maxPoint, other.minPoint, other.maxPoint)

  /**
    * The intersection of this and the other box.
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching. So the resulting
    *   intersection may have `min == max` in some or all dimensions.
    * @param other
    *   box to test
    * @return
    *   the intersection if this intersects the other, otherwise None.
    */
  def intersection(other: Box): Option[Box] =
    if intersects(other)
    then Some(Box(minPoint.projectAfterStart(other.minPoint), maxPoint.projectBeforeEnd(other.maxPoint)))
    else None

  /**
    * When some other box does not fit in this box, returns a new larger box containing the other box.
    */
  def growAround(other: Box): Box =
    Box(minPoint.projectBeforeStart(other.minPoint), maxPoint.projectAfterEnd(other.maxPoint))

  /**
    * Construct a new boxed payload from this box
    *
    * @param payload
    *   the payload
    * @tparam A
    *   the payload type
    * @return
    *   a new boxed payload
    */
  infix def ->[A](payload: A): BoxedPayload[A] = BoxedPayload(this, payload)

  override def toString: String = s"[${minPoint.asString}..${maxPoint.asString}]"

/**
  * Common definitions for boxes in multidimensional double space.
  */
object Box:
  /**
    * Construct a box that is unbound in all dimensions.
    *
    * @param arity
    *   number of dimensions
    * @return
    *   a new box
    */
  def unbound(arity: Int): Box =
    Box(Coordinate.unbound(arity), Coordinate.unbound(arity))

  /**
    * Construct a box at a single coordinate point.
    *
    * @param onePoint
    *   single coordinate
    * @return
    *   a new box
    */
  def at(onePoint: Coordinate): Box = Box(onePoint, onePoint)
