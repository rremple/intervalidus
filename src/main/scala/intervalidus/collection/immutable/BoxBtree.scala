package intervalidus.collection.immutable
import intervalidus.collection.*

/** @inheritdoc */
object BoxBtree extends ImmutableBoxTreeObjectLike[Coordinate1D, Box1D]:
  type BoxedPayloadType[A] = BoxedPayload1D[A]
  type SelfType[A] = BoxBtree[A]
  override def apply[A](
    boundary: Box1D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxBtree[A] = BoxBtreeBranch[A](boundary, 0, capacity, depthLimit)

/**
  * Immutable box tree in one dimension, see [[https://en.wikipedia.org/wiki/B-tree]].
  *
  * @tparam A
  *   payload type
  */
sealed trait BoxBtree[A] extends ImmutableBoxTreeLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]:
  override def addAll(ds: IterableOnce[BoxedPayload1D[A]]): BoxBtree[A] =
    ds.iterator.foldLeft(this: BoxBtree[A])(_.addOne(_))

/**
  * @inheritdoc
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxBtreeLeaf[A] private (
  val boundary: Box1D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val data: List[BoxedPayload1D[A]]
) extends BoxBtree[A]
  with ImmutableBoxTreeLeafLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]:

  // manage state as new instances
  def this(boundary: Box1D, depth: Int, capacity: Int, depthLimit: Int) =
    this(boundary, depth, capacity, depthLimit, List.empty)

  override protected def newLeaf(data: List[BoxedPayload1D[A]]): BoxBtreeLeaf[A] =
    BoxBtreeLeaf(boundary, depth, capacity, depthLimit, data)

  override protected def newBranch: BoxBtreeBranch[A] =
    BoxBtreeBranch(boundary, depth, capacity, depthLimit)

/**
  * @inheritdoc
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxBtreeBranch[A] private (
  val boundary: Box1D,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  val subtrees: Vector[BoxBtree[A]]
) extends BoxBtree[A]
  with ImmutableBoxTreeBranchLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]:

  override protected val subtreeBoundaries: Vector[Box1D] = subtrees.map(_.boundary)

  // manage state as new instances
  def this(boundary: Box1D, depth: Int, capacity: Int, depthLimit: Int) = this(
    boundary,
    depth,
    capacity,
    depthLimit,
    Vector(
      BoxBtreeLeaf[A](boundary.left, depth + 1, capacity, depthLimit),
      BoxBtreeLeaf[A](boundary.right, depth + 1, capacity, depthLimit)
    )
  )

  override protected def newBranch(subtrees: Vector[BoxBtree[A]]): BoxBtreeBranch[A] =
    BoxBtreeBranch(boundary, depth, capacity, depthLimit, subtrees)
