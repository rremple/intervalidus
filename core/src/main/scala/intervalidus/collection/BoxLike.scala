package intervalidus.collection

/**
  * Represents a box in multidimensional double space. Could be a bounding box that defines the region of a tree node,
  * or could be a valid region associated with data.
  */
trait BoxLike[C <: CoordinateLike[C], Self <: BoxLike[C, Self]]:
  /**
    * Coordinate of the minimum corner of the box (left/lower/back/etc., depending on dimension)
    */
  def minPoint: C

  /**
    * Coordinate of the maximum corner of the box (right/upper/front/etc., depending on dimension)
    */
  def maxPoint: C

  /**
    * The center of the box -- the point halfway between the mid and max point.
    */
  val midPoint: C = minPoint mid maxPoint

  /**
    * Does this box fully contain the other box?
    *
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  def contains(other: Self): Boolean

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
  def intersects(other: Self): Boolean

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
  def intersection(other: Self): Option[Self]

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
  infix def ->[A](payload: A): BoxedPayloadLike[A, C, Self, _]

  override def toString: String = s"[$minPoint..$maxPoint]"

/** @inheritdoc */
case class Box1D(minPoint: Coordinate1D, maxPoint: Coordinate1D) extends BoxLike[Coordinate1D, Box1D]:
  override def contains(other: Box1D): Boolean =
    other.minPoint.x >= minPoint.x && other.maxPoint.x <= maxPoint.x

  override def intersects(other: Box1D): Boolean =
    !(other.minPoint.x > maxPoint.x || other.maxPoint.x < minPoint.x)

  override def intersection(other: Box1D): Option[Box1D] =
    if intersects(other)
    then Some(Box1D(minPoint.projectAfter(other.minPoint), maxPoint.projectBefore(other.maxPoint)))
    else None

  override infix def ->[A](payload: A): BoxedPayload1D[A] = BoxedPayload1D(this, payload)

  /**
    * One of the two bisections of this box
    */
  def left: Box1D = Box1D(minPoint, midPoint)

  /**
    * One of the two bisections of this box
    */
  def right: Box1D = Box1D(midPoint, maxPoint)

/** @inheritdoc */
case class Box2D(minPoint: Coordinate2D, maxPoint: Coordinate2D) extends BoxLike[Coordinate2D, Box2D]:
  override def contains(other: Box2D): Boolean =
    other.minPoint.x >= minPoint.x && other.maxPoint.x <= maxPoint.x &&
      other.minPoint.y >= minPoint.y && other.maxPoint.y <= maxPoint.y

  override def intersects(other: Box2D): Boolean =
    !(other.minPoint.x > maxPoint.x || other.maxPoint.x < minPoint.x ||
      other.minPoint.y > maxPoint.y || other.maxPoint.y < minPoint.y)

  override def intersection(other: Box2D): Option[Box2D] =
    if intersects(other)
    then Some(Box2D(minPoint.projectAfter(other.minPoint), maxPoint.projectBefore(other.maxPoint)))
    else None

  override infix def ->[A](payload: A): BoxedPayload2D[A] = BoxedPayload2D(this, payload)

  /**
    * One of the four quadrants of this box
    */
  def leftLower: Box2D = Box2D(minPoint, midPoint)

  /**
    * One of the four quadrants of this box
    */
  def leftUpper: Box2D = Box2D(minPoint.copy(y = midPoint.y), maxPoint.copy(x = midPoint.x))

  /**
    * One of the four quadrants of this box
    */
  def rightLower: Box2D = Box2D(minPoint.copy(x = midPoint.x), maxPoint.copy(y = midPoint.y))

  /**
    * One of the four quadrants of this box
    */
  def rightUpper: Box2D = Box2D(midPoint, maxPoint)

/** @inheritdoc */
case class Box3D(minPoint: Coordinate3D, maxPoint: Coordinate3D) extends BoxLike[Coordinate3D, Box3D]:
  override def contains(other: Box3D): Boolean =
    other.minPoint.x >= minPoint.x && other.maxPoint.x <= maxPoint.x &&
      other.minPoint.y >= minPoint.y && other.maxPoint.y <= maxPoint.y &&
      other.minPoint.z >= minPoint.z && other.maxPoint.z <= maxPoint.z

  override def intersects(other: Box3D): Boolean =
    !(other.minPoint.x > maxPoint.x || other.maxPoint.x < minPoint.x ||
      other.minPoint.y > maxPoint.y || other.maxPoint.y < minPoint.y ||
      other.minPoint.z > maxPoint.z || other.maxPoint.z < minPoint.z)

  override def intersection(other: Box3D): Option[Box3D] =
    if intersects(other)
    then Some(Box3D(minPoint.projectAfter(other.minPoint), maxPoint.projectBefore(other.maxPoint)))
    else None

  override infix def ->[A](value: A): BoxedPayload3D[A] = BoxedPayload3D(this, value)

  /**
    * One of the eight octants of this box
    */
  def leftLowerBack: Box3D = Box3D(minPoint, midPoint)

  /**
    * One of the eight octants of this box
    */
  def leftLowerFront: Box3D = Box3D(minPoint.copy(z = midPoint.z), midPoint.copy(z = maxPoint.z))

  /**
    * One of the eight octants of this box
    */
  def leftUpperBack: Box3D = Box3D(minPoint.copy(y = midPoint.y), midPoint.copy(y = maxPoint.y))

  /**
    * One of the eight octants of this box
    */
  def leftUpperFront: Box3D = Box3D(midPoint.copy(x = minPoint.x), maxPoint.copy(x = midPoint.x))

  /**
    * One of the eight octants of this box
    */
  def rightLowerBack: Box3D = Box3D(minPoint.copy(x = midPoint.x), midPoint.copy(x = maxPoint.x))

  /**
    * One of the eight octants of this box
    */
  def rightLowerFront: Box3D = Box3D(midPoint.copy(y = minPoint.y), maxPoint.copy(y = midPoint.y))

  /**
    * One of the eight octants of this box
    */
  def rightUpperBack: Box3D = Box3D(midPoint.copy(z = minPoint.z), maxPoint.copy(z = midPoint.z))

  /**
    * One of the eight octants of this box
    */
  def rightUpperFront: Box3D = Box3D(midPoint, maxPoint)
