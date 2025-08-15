package intervalidus.collection.mutable

import intervalidus.collection.*

/**
  * Constructors and types for mutable box search trees of arbitrary dimensions.
  */
object BoxTree extends BoxTreeObjectLike:
  def apply[A](
    boundary: Boundary,
    nodeCapacity: Int = defaultNodeCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxTree[A] = BoxTreeBranch[A](boundary, 0, nodeCapacity, depthLimit)

  def from[A](
    boundary: Boundary,
    ds: IterableOnce[BoxedPayload[A]],
    nodeCapacity: Int = defaultNodeCapacity,
    depthLimit: Int = defaultDepthLimit
  ): BoxTree[A] =
    val newTree: BoxTree[A] = apply(boundary, nodeCapacity, depthLimit)
    newTree.addAll(ds)
    newTree

/**
  * Mutable box search tree in arbitrary dimensions. Mutation operations are applied in place.
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
    * @param d
    *   boxed data to insert/update.
    */
  def addOne(d: BoxedPayload[A]): Unit

  /**
    * Deletes boxed data from the tree.
    * @param d
    *   boxed data to delete.
    */
  def remove(d: BoxedPayload[A]): Unit

  /**
    * Clears data.
    */
  def clear(): Unit

  /**
    * Indicates if a node in the tree has capacity for more elements. Always true for branches. (Only useful in mutable
    * context, where branches interrogate the capacity of leaves.)
    *
    * @return
    *   true if there's capacity remaining, otherwise false.
    */
  def hasCapacity: Boolean

  /**
    * Adds all the data.
    *
    * @param ds
    *   boxed data to insert/update.
    */
  def addAll(ds: IterableOnce[BoxedPayload[A]]): Unit =
    ds.iterator.foreach(addOne)

/**
  * A leaf holds a list of data (up to the node capacity) for a particular subtree.
  */
class BoxTreeLeaf[A](val boundary: Boundary, val depth: Int, val nodeCapacity: Int, val depthLimit: Int)
  extends BoxTree[A]:

  // state
  private var data: List[BoxedPayload[A]] = List.empty

  override def copy: BoxTree[A] =
    val newLeaf = BoxTreeLeaf[A](boundary, depth, nodeCapacity, depthLimit)
    newLeaf.data = this.data // safe because data is a mutable reference to an immutable list
    newLeaf

  override def hasCapacity: Boolean = data.length < nodeCapacity || depth == depthLimit

  override def addOne(d: BoxedPayload[A]): Unit =
    data = d :: data

  // ignores parentBox
  override def remove(d: BoxedPayload[A]): Unit =
    data = data.filterNot(x => x.box == d.box && x.payload == d.payload)

  override def get(range: Box): Iterable[BoxedPayload[A]] = data.filter(d => range.intersects(d.box))

  override def toIterable: Iterable[BoxedPayload[A]] = data

  override def clear(): Unit =
    data = List.empty

/**
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxTreeBranch[A](initialBoundary: Boundary, val depth: Int, val nodeCapacity: Int, val depthLimit: Int)
  extends BoxTree[A]:

  // manage subtree state
  private var dynamicBoundary: Boundary = initialBoundary // can grow
  private var subtreeBoundaries: Vector[Boundary] = dynamicBoundary.binarySplit
  private var subtrees: Vector[BoxTree[A]] =
    subtreeBoundaries.map(BoxTreeLeaf[A](_, depth + 1, nodeCapacity, depthLimit))

  // A subtree boundary within a branch always has the properties that
  //  `branch.box contains subtree.box` and `branch.capacity contains subtree.capacity`
  // require(
  //  subtreeBoundaries.forall(subtree =>
  //    (boundary.box contains subtree.box) && (boundary.capacity contains subtree.capacity)
  //  ),
  //  s"not all subtrees are nested properly in the boundary"
  // )

  override def boundary: Boundary = dynamicBoundary

  override def copy: BoxTree[A] =
    val newTree = BoxTreeBranch[A](boundary, depth, nodeCapacity, depthLimit)
    newTree.subtrees = subtrees.map(_.copy)
    newTree

  override def hasCapacity: Boolean = true

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: BoxedPayload[A]): Unit =
    // If this is the root branch, we may have to grow the boundary and rehash if the boxed payload is out of bounds
    if depth == 0 && !(boundary contains d.box)
    then // out of bounds, so first we grow the boundary and repartition everything
      val newBoundary = boundary.growAround(d.box)
      // println(s"resizing based on ${d.box} not fitting in capacity ${boundary.capacity}")
      val tempBranch = BoxTreeBranch[A](newBoundary, depth, nodeCapacity, depthLimit)
      tempBranch.addAll(BoxedPayload.deduplicate(toIterable))
      dynamicBoundary = newBoundary
      subtreeBoundaries = tempBranch.subtreeBoundaries
      subtrees = tempBranch.subtrees

    val boxSplits = subtreeBoundaries.count(_.box.intersects(d.box)) > 1
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary.box) match
        case None => subtree
        case Some(newBox) =>
          val dataToAdd =
            if !boxSplits then d
            else d.withBox(newBox).withParentBox(d.parentBox.orElse(Some(d.box)))
          // If there is room, we add the boxed payload to the subtree (either another branch or a leaf with capacity).
          // Otherwise, we create a branch (which creates new leaves) and push the new and existing into it.
          if subtree.hasCapacity then
            subtree.addOne(dataToAdd)
            subtree
          else
            val newSubtree = BoxTreeBranch[A](subtree.boundary, depth + 1, nodeCapacity, depthLimit)
            newSubtree.addAll(subtree.toIterable)
            newSubtree.addOne(dataToAdd)
            newSubtree
    subtrees = updatedSubtrees

  override def remove(d: BoxedPayload[A]): Unit =
    subtrees.foreach: subtree =>
      d.box
        .intersection(subtree.boundary.box)
        .foreach: newBox =>
          subtree.remove(d.withBox(newBox))

  override def get(range: Box): Iterable[BoxedPayload[A]] =
    if boundary.box.intersects(range)
    then subtrees.flatMap(subtree => range.intersection(subtree.boundary.box).flatMap(subtree.get))
    else Vector.empty

  override def toIterable: Iterable[BoxedPayload[A]] = subtrees.flatMap(_.toIterable)

  override def clear(): Unit = // recursively clear, leaving the structure in place
    subtrees.foreach(_.clear())
