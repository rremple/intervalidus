package intervalidus.collection.immutable
import intervalidus.collection.*

/**
  * Constructors for companion immutable box tree.
  */
object BoxQuadtree extends ImmutableBoxTreeObjectLike[Coordinate2D, Box2D]:
  type BoxedPayloadType[A] = BoxedPayload2D[A]
  type SelfType[A] = BoxQuadtree[A]
  def apply[A](
    boundary: Box2D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxQuadtree[A] = BoxQuadtreeBranch[A](boundary, 0, capacity, depthLimit)

/**
  * Immutable box tree in two dimensions, see [[https://en.wikipedia.org/wiki/Quadtree]].
  *
  * @tparam A
  *   payload type
  */
sealed trait BoxQuadtree[A] extends ImmutableBoxTreeLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]:
  override def addAll(ds: IterableOnce[BoxedPayload2D[A]]): BoxQuadtree[A] =
    ds.iterator.foldLeft(this: BoxQuadtree[A])(_.addOne(_))

/**
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxQuadtreeLeaf[A] private (
  val boundary: Box2D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val data: List[BoxedPayload2D[A]]
) extends BoxQuadtree[A]
  with ImmutableBoxTreeLeafLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]:

  // manage state as new instances
  def this(boundary: Box2D, depth: Int, capacity: Int, depthLimit: Int) =
    this(boundary, depth, capacity, depthLimit, List.empty)

  override protected def newLeaf(data: List[BoxedPayload2D[A]]): BoxQuadtreeLeaf[A] =
    BoxQuadtreeLeaf(boundary, depth, capacity, depthLimit, data)

  override protected def newBranch: BoxQuadtreeBranch[A] =
    BoxQuadtreeBranch(boundary, depth, capacity, depthLimit)

/**
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxQuadtreeBranch[A] private (
  val boundary: Box2D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val subtrees: Vector[BoxQuadtree[A]]
) extends BoxQuadtree[A]
  with ImmutableBoxTreeBranchLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]:

  override protected val subtreeBoundaries: Vector[Box2D] = subtrees.map(_.boundary)

  // manage state as new instances
  def this(boundary: Box2D, depth: Int, capacity: Int, depthLimit: Int) = this(
    boundary,
    depth,
    capacity,
    depthLimit,
    Vector(
      BoxQuadtreeLeaf[A](boundary.leftLower, depth + 1, capacity, depthLimit),
      BoxQuadtreeLeaf[A](boundary.leftUpper, depth + 1, capacity, depthLimit),
      BoxQuadtreeLeaf[A](boundary.rightLower, depth + 1, capacity, depthLimit),
      BoxQuadtreeLeaf[A](boundary.rightUpper, depth + 1, capacity, depthLimit)
    )
  )

  override protected def newBranch(subtrees: Vector[BoxQuadtree[A]]): BoxQuadtreeBranch[A] =
    BoxQuadtreeBranch(boundary, depth, capacity, depthLimit, subtrees)
