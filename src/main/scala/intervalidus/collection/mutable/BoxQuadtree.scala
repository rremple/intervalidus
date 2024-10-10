package intervalidus.collection.mutable

import intervalidus.collection.*

/** @inheritdoc */
object BoxQuadtree extends MutableBoxTreeObjectLike[Coordinate2D, Box2D]:
  type BoxedPayloadType[A] = BoxedPayload2D[A]
  type SelfType[A] = BoxQuadtree[A]
  def apply[A](
    boundary: Box2D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxQuadtree[A] = BoxQuadtreeBranch[A](boundary, 0, capacity, depthLimit)

/**
  * Mutable box tree in two dimensions, see [[https://en.wikipedia.org/wiki/Quadtree]].
  *
  * @tparam A
  *   payload type
  */
sealed trait BoxQuadtree[A] extends MutableBoxTreeLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]

/**
  * @inheritdoc
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxQuadtreeLeaf[A](val boundary: Box2D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxQuadtree[A]
  with MutableBoxTreeLeafLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]:

  override def copy: BoxQuadtree[A] =
    val newLeaf = BoxQuadtreeLeaf[A](boundary, depth, capacity, depthLimit)
    newLeaf.data = this.data // safe because data is a mutable reference to an immutable list
    newLeaf

/**
  * @inheritdoc
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxQuadtreeBranch[A](val boundary: Box2D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxQuadtree[A]
  with MutableBoxTreeBranchLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]:

  type SuperSelf = MutableBoxTreeLike[A, Coordinate2D, Box2D, BoxedPayload2D[A], BoxQuadtree[A]]

  override def copy: BoxQuadtree[A] =
    val newBranch = BoxQuadtreeBranch[A](boundary, depth, capacity, depthLimit)
    newBranch.updateSubtrees(subtrees.map(_.copy))
    newBranch

  // manage subtree state
  override protected val subtreeBoundaries: Vector[Box2D] = Vector(
    boundary.leftLower,
    boundary.leftUpper,
    boundary.rightLower,
    boundary.rightUpper
  )
  private var subtreesState: Vector[SuperSelf] =
    subtreeBoundaries.map(BoxQuadtreeLeaf[A](_, depth + 1, capacity, depthLimit))
  override def subtrees: Vector[SuperSelf] = subtreesState
  override def updateSubtrees(subtrees: Vector[SuperSelf]): Unit = subtreesState = subtrees

  // new instances
  override protected def newBranch(boundary: Box2D, depth: Int): BoxQuadtreeBranch[A] =
    BoxQuadtreeBranch(boundary, depth, capacity, depthLimit)
