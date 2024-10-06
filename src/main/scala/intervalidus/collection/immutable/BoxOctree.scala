package intervalidus.collection.immutable
import intervalidus.collection.*

object BoxOctree extends ImmutableBoxTreeObjectLike[Coordinate3D, Box3D]:
  type BoxedPayloadType[A] = BoxedPayload3D[A]
  type SelfType[A] = BoxOctree[A]
  def apply[A](
    boundary: Box3D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxOctree[A] = BoxOctreeBranch[A](boundary, 0, capacity, depthLimit)

sealed trait BoxOctree[A] extends ImmutableBoxTreeLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]:

  override def addAll(ds: IterableOnce[BoxedPayload3D[A]]): BoxOctree[A] =
    ds.iterator.foldLeft(this: BoxOctree[A])(_.addOne(_))

class BoxOctreeLeaf[A] private (
  val boundary: Box3D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val data: List[BoxedPayload3D[A]]
) extends BoxOctree[A]
  with ImmutableBoxTreeLeafLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]:

  // manage state as new instances
  def this(boundary: Box3D, depth: Int, capacity: Int, depthLimit: Int) =
    this(boundary, depth, capacity, depthLimit, List.empty)

  override protected def newLeaf(data: List[BoxedPayload3D[A]]): BoxOctreeLeaf[A] =
    BoxOctreeLeaf(boundary, depth, capacity, depthLimit, data)

  override protected def newBranch: BoxOctreeBranch[A] =
    BoxOctreeBranch(boundary, depth, capacity, depthLimit)

class BoxOctreeBranch[A] private (
  val boundary: Box3D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val subtrees: Vector[BoxOctree[A]]
) extends BoxOctree[A]
  with ImmutableBoxTreeBranchLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]:

  override protected val subtreeBoundaries: Vector[Box3D] = subtrees.map(_.boundary)

  // manage state as new instances
  def this(boundary: Box3D, depth: Int, capacity: Int, depthLimit: Int) = this(
    boundary,
    depth,
    capacity,
    depthLimit,
    Vector(
      BoxOctreeLeaf[A](boundary.leftLowerBack, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.leftLowerFront, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.leftUpperBack, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.leftUpperFront, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.rightLowerBack, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.rightLowerFront, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.rightUpperBack, depth, capacity, depthLimit),
      BoxOctreeLeaf[A](boundary.rightUpperFront, depth, capacity, depthLimit)
    )
  )

  override protected def newBranch(subtrees: Vector[BoxOctree[A]]): BoxOctreeBranch[A] =
    BoxOctreeBranch(boundary, depth, capacity, depthLimit, subtrees)
