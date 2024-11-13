package intervalidus.collection

/**
  * A simple coordinate class to represent a point in multidimensional double space.
  *
  * @tparam Self
  *   F-bounded self type
  */
trait CoordinateLike[Self <: CoordinateLike[Self]]:
  this: Self =>

  /**
    * Returns the point where each dimension is the min of this and the other point. It will be less than or equal to
    * the other point in every dimension.
    *
    * @param other
    *   coordinate
    * @return
    *   a new coordinate point with minimum coordinates.
    */
  def projectBefore(other: Self): Self

  /**
    * Returns midpoint between this and other.
    *
    * @param other
    *   coordinate
    * @return
    *   midpoint
    */
  infix def mid(other: Self): Self

  /**
    * Returns the point where each dimension is the max of this and the other point. It will be greater than or equal to
    * the other point in every dimension.
    *
    * @param other
    *   coordinate
    * @return
    *   a new coordinate point with minimum coordinates.
    */
  def projectAfter(other: Self): Self

/**
  * A simple coordinate class to represent a point in one-dimensional double space (the number line).
  *
  * @param x
  *   the coordinate
  */
case class Coordinate1D(x: Double) extends CoordinateLike[Coordinate1D]:
  override def projectBefore(other: Coordinate1D): Coordinate1D =
    Coordinate1D(x.min(other.x))
  override infix def mid(other: Coordinate1D): Coordinate1D =
    Coordinate1D(x / 2.0 + other.x / 2.0)
  override def projectAfter(other: Coordinate1D): Coordinate1D =
    Coordinate1D(x.max(other.x))
  override def toString: String = x.toString

/**
  * A simple coordinate class to represent a point in two-dimensional double space (a Cartesian plane).
  *
  * @param x
  *   the x coordinate
  * @param y
  *   the y coordinate
  */
case class Coordinate2D(x: Double, y: Double) extends CoordinateLike[Coordinate2D]:
  override def projectBefore(other: Coordinate2D): Coordinate2D =
    Coordinate2D(x.min(other.x), y.min(other.y))
  override infix def mid(other: Coordinate2D): Coordinate2D =
    Coordinate2D(x / 2.0 + other.x / 2.0, y / 2.0 + other.y / 2.0)
  override def projectAfter(other: Coordinate2D): Coordinate2D =
    Coordinate2D(x.max(other.x), y.max(other.y))
  override def toString: String = s"($x,$y)"

/**
  * A simple coordinate class to represent a point in three-dimensional double space (a Cartesian solid).
  *
  * @param x
  *   the x coordinate
  * @param y
  *   the y coordinate
  * @param z
  *   the z coordinate
  */
case class Coordinate3D(x: Double, y: Double, z: Double) extends CoordinateLike[Coordinate3D]:
  override def projectBefore(other: Coordinate3D): Coordinate3D =
    Coordinate3D(x.min(other.x), y.min(other.y), z.min(other.z))
  override infix def mid(other: Coordinate3D): Coordinate3D =
    Coordinate3D(x / 2.0 + other.x / 2.0, y / 2.0 + other.y / 2.0, z / 2.0 + other.z / 2.0)
  override def projectAfter(other: Coordinate3D): Coordinate3D =
    Coordinate3D(x.max(other.x), y.max(other.y), z.max(other.z))
  override def toString: String = s"($x,$y,$z)"
