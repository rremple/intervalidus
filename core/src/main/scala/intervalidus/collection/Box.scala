package intervalidus.collection

/**
  * Represents a box in multidimensional double space. Could be a bounding box that defines the region of a tree node,
  * or could be a valid region associated with data.
  *
  * @param minPoint
  *   Coordinate of the minimum corner of the box(left / lower / back / etc., depending on dimension)
  * @param maxPoint
  *   Coordinate of the maximum corner of the box (right/upper/front/etc., depending on dimension)
  */
case class Box(minPoint: Coordinate, maxPoint: Coordinate):
  private def corners: (Coordinate, Coordinate) = (minPoint, maxPoint)

  /**
    * The center of the box -- the point halfway between the mid and max point.
    */
  val midPoint: Coordinate = minPoint mid maxPoint

  /**
    * Does this box fully contain the other box?
    *
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  def contains(other: Box): Boolean = Coordinate.minMaxForall(this.corners, other.corners):
    case ((thisMin, thisMax), (otherMin, otherMax)) =>
      otherMin >= thisMin && otherMax <= thisMax

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
  def intersects(other: Box): Boolean = Coordinate.minMaxForall(this.corners, other.corners):
    case ((thisMin, thisMax), (otherMin, otherMax)) =>
      otherMin <= thisMax && otherMax >= thisMin

  /**
    * The intersection of this and the other box.
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching. So the resulting
    *   intersection may have min == max in some or all dimensions.
    * @param other
    *   box to test
    * @return
    *   the intersection if this intersects the other, otherwise None.
    */
  def intersection(other: Box): Option[Box] =
    if intersects(other)
    then Some(Box(minPoint.projectAfter(other.minPoint), maxPoint.projectBefore(other.maxPoint)))
    else None

  /**
    * Constructs a boxes based on binary splits in each dimension. For n dimensions, there are 2<sup>n</sup> boxes
    * created, which generalizes subtree branches in B-trees (2 boxes in one dimension), quadtrees (4 boxes in two
    * dimensions), octrees (8 boxes in three dimensions), etc.
    *
    * @return
    *   a vector of boxes that can be used for subtree boundaries
    */
  def binarySplit: Vector[Box] = minPoint.binarySplit(maxPoint).map(Box(_, _))

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

object Box:
  /**
    * Construct a box at a single coordinate point.
    *
    * @param onePoint
    *   single coordinate
    * @return
    *   a new box
    */
  def at(onePoint: Coordinate): Box = Box(onePoint, onePoint)
