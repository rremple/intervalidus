package intervalidus.collection

/**
  * Represents a boundary in multidimensional double space. Used in defining the boundary of a tree branch and its
  * subtrees. The edges of `capacity` approximate the edges of `box`.
  *
  * A subtree boundary within a branch always has the properties that `branch.capacity contains subtree.capacity` and
  * `branch.box contains subtree.box`.
  *
  * @param box
  *   defines the actual boundary, (which may be unbounded in some/all min/max coordinates).
  * @param capacity
  *   defines the constrained area of the boundary in which all the data fits.
  */
case class Boundary private (box: Box, capacity: Capacity):
  require(box.arity == capacity.arity, s"box $box and capacity $capacity must have the same arity")

  // only true if box starts out as completely unbounded
  // require(box.fixUnbounded(capacity).contains(capacity), s"box $box must contain capacity $capacity")

  /**
    * Generally the box is managed internally, but this method can be used to manipulate it directly. Useful for
    * testing.
    * @param newBox
    *   the box to be used in a new boundary with the same capacity
    * @return
    *   a new boundary with the new box and the same capacity
    */
  def withBox(newBox: Box): Boundary = copy(box = newBox)

  /**
    * Constructs new boundaries based on binary splits in each dimension using the boundary capacity midpoint. For n
    * dimensions, there are 2<sup>n</sup> boxes created (hyperoctants), which generalizes subtree branches in B-trees (2
    * boxes in one dimension), quadtrees (4 boxes in two dimensions), octrees (8 boxes in three dimensions), 4D
    * hyperoctrees (16 boxes in four dimensions), etc.
    *
    * @return
    *   a vector of boxes that can be used for subtree boundaries
    */
  def binarySplit: Vector[Boundary] =
    val corners = box.minPoint.binarySplit(capacity.midPoint.unfix, box.maxPoint)
    corners.map: (boxMinPoint, boxMaxPoint) =>
      Boundary(
        Box(boxMinPoint, boxMaxPoint),
        Capacity(boxMinPoint.fixMax(capacity.minPoint), boxMaxPoint.fixMin(capacity.maxPoint))
      )

  /**
    * Does both the box and capacity of this boundary fully contain the other box?
    *
    * @param otherBox
    *   box to test
    * @return
    *   true or false
    */
  infix def contains(otherBox: Box): Boolean =
    (capacity contains otherBox) && (box contains otherBox)

  /**
    * When some other box does not fit in this boundary (either its box, capacity, or both), returns a new larger
    * boundary containing the other box. Uses a scaling magnitude of 1.0 on the capacity, which doubles its size in
    * every dimension by 2x after growing it to contain the other box.
    */
  def growAround(other: Box): Boundary =
    if capacity contains other then copy(box = box.growAround(other)) // no change in capacity
    else Boundary(box.growAround(other), capacity.growAround(other).rescale(1.0))

  override def toString: String = s"Boundary: box $box, capacity $capacity"

/**
  * Common definitions for boundaries in multidimensional double space.
  */
object Boundary:

  /**
    * Returns a boundary based on some boundary capacity, where the box it approximates is completely unbounded.
    *
    * @param capacity
    *   initial capacity of the boundary
    */
  def apply(capacity: Capacity): Boundary =
    Boundary(Box.unbound(capacity.arity), capacity)
