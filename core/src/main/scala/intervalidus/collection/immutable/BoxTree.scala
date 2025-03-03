package intervalidus.collection.immutable

import intervalidus.collection.*

/**
  * Constructors and types for immutable box search trees of arbitrary dimensions.
  */
object BoxTree extends BoxTreeObjectLike:
  def apply[A](
    boundary: Box,
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxTree[A] = BoxTreeBranch[A](boundary, 0, capacity, depthLimit)

  def from[A](
    boundary: Box,
    ds: IterableOnce[BoxedPayload[A]],
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxTree[A] = apply(boundary, capacity, depthLimit).addAll(ds)

/**
  * Immutable box search tree in arbitrary dimensions. Mutation operations return a new tree with updates applied.
  *
  * See [[https://en.wikipedia.org/wiki/Tree_(abstract_data_type)]], [[https://en.wikipedia.org/wiki/B-tree]],
  * [[https://en.wikipedia.org/wiki/Quadtree]], and [[https://en.wikipedia.org/wiki/Octree]].
  *
  * @tparam A
  *   payload type
  */
sealed trait BoxTree[A] extends BoxTreeLike[A, BoxTree[A]]:
  /**
    * Inserts/updates boxed data into the tree.
    *
    * @param d
    *   boxed data to insert/update.
    * @return
    *   updated tree.
    */
  def addOne(d: BoxedPayload[A]): BoxTree[A]

  /**
    * Deletes boxed data from the tree.
    *
    * @param d
    *   boxed data to delete.
    * @return
    *   updated tree.
    */
  def remove(d: BoxedPayload[A]): BoxTree[A]

  /**
    * Clears data.
    *
    * @return
    *   updated tree.
    */
  def clear: BoxTree[A]

  /**
    * Adds all the data.
    *
    * @param ds
    *   boxed data to insert/update.
    * @return
    *   updated tree.
    */
  def addAll(ds: IterableOnce[BoxedPayload[A]]): BoxTree[A] =
    ds.iterator.foldLeft(this: BoxTree[A])(_.addOne(_))

/**
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxTreeLeaf[A] private (
  val boundary: Box,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  private val data: List[BoxedPayload[A]] // state
) extends BoxTree[A]:

  // manage state as new instances
  def this(boundary: Box, depth: Int, capacity: Int, depthLimit: Int) =
    this(boundary, depth, capacity, depthLimit, List.empty)

  // create new instances with the same boundary, depth, capacity, and depthLimit
  private def newLeaf(data: List[BoxedPayload[A]]): BoxTreeLeaf[A] =
    BoxTreeLeaf(boundary, depth, capacity, depthLimit, data)

  private def newBranch: BoxTreeBranch[A] =
    BoxTreeBranch(boundary, depth, capacity, depthLimit)

  override def copy: BoxTree[A] = newLeaf(data)

  private def hasCapacity: Boolean = data.length < capacity || depth == depthLimit

  override def addOne(d: BoxedPayload[A]): BoxTree[A] =
    // If there is room, we add the boxed payload here. Otherwise, we create a branch (which creates new leaves)
    // and push the new and existing into it.
    if hasCapacity then newLeaf(d :: data) else data.foldLeft(newBranch.addOne(d))(_.addOne(_))

  // ignores parentBox
  override def remove(d: BoxedPayload[A]): BoxTree[A] =
    newLeaf(data.filterNot(x => x.box == d.box && x.payload == d.payload))

  override def get(range: Box): Iterable[BoxedPayload[A]] = data.filter(d => range.intersects(d.box))

  override def toIterable: Iterable[BoxedPayload[A]] = data

  override def clear: BoxTree[A] = newLeaf(List.empty)

/**
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxTreeBranch[A] private (
  val boundary: Box,
  val depth: Int,
  val capacity: Int,
  val depthLimit: Int,
  private val subtrees: Vector[BoxTree[A]] // state
) extends BoxTree[A]:

  // manage state as new instances
  def this(boundary: Box, depth: Int, capacity: Int, depthLimit: Int) = this(
    boundary,
    depth,
    capacity,
    depthLimit,
    boundary.binarySplit.map(BoxTreeLeaf[A](_, depth + 1, capacity, depthLimit))
  )

  // manage subtree state
  private val subtreeBoundaries: Vector[Box] = subtrees.map(_.boundary)

  private def newBranch(subtrees: Vector[BoxTree[A]]): BoxTreeBranch[A] =
    BoxTreeBranch(boundary, depth, capacity, depthLimit, subtrees)

  override def copy: BoxTree[A] = newBranch(subtrees)

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: BoxedPayload[A]): BoxTree[A] =
    val boxSplits = subtreeBoundaries.count(d.box.intersects) > 1
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary) match
        case None => subtree
        case Some(newBox) =>
          val dataToAdd =
            if !boxSplits then d
            else d.withBox(newBox).withParentBox(d.parentBox.orElse(Some(d.box)))
          subtree.addOne(dataToAdd)
    newBranch(updatedSubtrees)

  override def remove(d: BoxedPayload[A]): BoxTree[A] =
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary) match
        case None         => subtree
        case Some(newBox) => subtree.remove(d.withBox(newBox))
    newBranch(updatedSubtrees)

  override def get(range: Box): Iterable[BoxedPayload[A]] =
    if boundary.intersects(range)
    then subtrees.flatMap(subtree => range.intersection(subtree.boundary).flatMap(subtree.get))
    else Vector.empty

  override def toIterable: Iterable[BoxedPayload[A]] = subtrees.flatMap(_.toIterable)

  override def clear: BoxTree[A] = // recursively clear, leaving structure in place
    newBranch(subtrees.map(_.clear))
