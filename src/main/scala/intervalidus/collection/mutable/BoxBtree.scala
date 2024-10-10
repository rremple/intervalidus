package intervalidus.collection.mutable

import intervalidus.collection.*

/** @inheritdoc */
object BoxBtree extends MutableBoxTreeObjectLike[Coordinate1D, Box1D]:
  type BoxedPayloadType[A] = BoxedPayload1D[A]
  type SelfType[A] = BoxBtree[A]
  def apply[A](
    boundary: Box1D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxBtree[A] = BoxBtreeBranch[A](boundary, 0, capacity, depthLimit)

/**
  * Mutable box tree in one dimension, see [[https://en.wikipedia.org/wiki/B-tree]].
  *
  * @tparam A
  *   payload type
  */
sealed trait BoxBtree[A] extends MutableBoxTreeLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]

/**
  * @inheritdoc
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxBtreeLeaf[A](val boundary: Box1D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxBtree[A]
  with MutableBoxTreeLeafLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]:

  override def copy: BoxBtree[A] =
    val newLeaf = BoxBtreeLeaf[A](boundary, depth, capacity, depthLimit)
    newLeaf.data = this.data // safe because data is a mutable reference to an immutable list
    newLeaf

/**
  * @inheritdoc
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxBtreeBranch[A](val boundary: Box1D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxBtree[A]
  with MutableBoxTreeBranchLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]:

  type SuperSelf = MutableBoxTreeLike[A, Coordinate1D, Box1D, BoxedPayload1D[A], BoxBtree[A]]

  override def copy: BoxBtree[A] =
    val newBranch = BoxBtreeBranch[A](boundary, depth, capacity, depthLimit)
    newBranch.updateSubtrees(subtrees.map(_.copy))
    newBranch

  // manage subtree state
  override protected val subtreeBoundaries: Vector[Box1D] = Vector(
    boundary.left,
    boundary.right
  )
  private var subtreesState: Vector[SuperSelf] =
    subtreeBoundaries.map(BoxBtreeLeaf[A](_, depth + 1, capacity, depthLimit))
  override def subtrees: Vector[SuperSelf] = subtreesState
  override def updateSubtrees(subtrees: Vector[SuperSelf]): Unit = subtreesState = subtrees

  // new instances
  override protected def newBranch(boundary: Box1D, depth: Int): BoxBtreeBranch[A] =
    BoxBtreeBranch(boundary, depth, capacity, depthLimit)
