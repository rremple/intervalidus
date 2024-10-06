package intervalidus.collection.mutable

import intervalidus.collection.*

object BoxOctree extends MutableBoxTreeObjectLike[Coordinate3D, Box3D]:
  type BoxedPayloadType[A] = BoxedPayload3D[A]
  type SelfType[A] = BoxOctree[A]
  def apply[A](
    boundary: Box3D,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxOctree[A] = BoxOctreeBranch[A](boundary, 0, capacity, depthLimit)

sealed trait BoxOctree[A] extends MutableBoxTreeLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]

class BoxOctreeLeaf[A](val boundary: Box3D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxOctree[A]
  with MutableBoxTreeLeafLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]:

  override def copy: BoxOctree[A] =
    val newLeaf = BoxOctreeLeaf[A](boundary, depth, capacity, depthLimit)
    newLeaf.data = this.data // safe because data is a mutable reference to an immutable list
    newLeaf

class BoxOctreeBranch[A](val boundary: Box3D, val depth: Int, val capacity: Int, val depthLimit: Int)
  extends BoxOctree[A]
  with MutableBoxTreeBranchLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]:

  type SuperSelf = MutableBoxTreeLike[A, Coordinate3D, Box3D, BoxedPayload3D[A], BoxOctree[A]]

  override def copy: BoxOctree[A] =
    val newBranch = BoxOctreeBranch[A](boundary, depth, capacity, depthLimit)
    newBranch.updateSubtrees(subtrees.map(_.copy))
    newBranch

  // manage subtree state
  override protected val subtreeBoundaries: Vector[Box3D] = Vector(
    boundary.leftLowerBack,
    boundary.leftLowerFront,
    boundary.leftUpperBack,
    boundary.leftUpperFront,
    boundary.rightLowerBack,
    boundary.rightLowerFront,
    boundary.rightUpperBack,
    boundary.rightUpperFront
  )
  private var subtreesState: Vector[SuperSelf] =
    subtreeBoundaries.map(BoxOctreeLeaf[A](_, depth + 1, capacity, depthLimit))
  override def subtrees: Vector[SuperSelf] = subtreesState
  override def updateSubtrees(subtrees: Vector[SuperSelf]): Unit = subtreesState = subtrees

  // new instances
  override protected def newBranch(boundary: Box3D, depth: Int): BoxOctreeBranch[A] =
    BoxOctreeBranch(boundary, depth, capacity, depthLimit)
