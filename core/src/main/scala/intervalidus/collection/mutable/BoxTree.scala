package intervalidus.collection.mutable

import intervalidus.collection.*

/**
  * Constructors and types for mutable box trees of arbitrary dimensions.
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
  ): BoxTree[A] =
    val newTree: BoxTree[A] = apply(boundary, capacity, depthLimit)
    newTree.addAll(ds)
    newTree

/**
  * Mutable box tree in arbitrary dimensions, see [[https://en.wikipedia.org/wiki/Tree]]. Mutation operations are
  * applied in place.
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
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
class BoxTreeLeaf[A](val boundary: Box, val depth: Int, val capacity: Int, val depthLimit: Int) extends BoxTree[A]:

  // state
  private var data: List[BoxedPayload[A]] = List.empty

  override def copy: BoxTree[A] =
    val newLeaf = BoxTreeLeaf[A](boundary, depth, capacity, depthLimit)
    newLeaf.data = this.data // safe because data is a mutable reference to an immutable list
    newLeaf

  override def hasCapacity: Boolean = data.length < capacity || depth == depthLimit

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
class BoxTreeBranch[A](val boundary: Box, val depth: Int, val capacity: Int, val depthLimit: Int) extends BoxTree[A]:

  // manage subtree state
  private val subtreeBoundaries: Vector[Box] = boundary.binarySplit
  private var subtrees: Vector[BoxTree[A]] =
    subtreeBoundaries.map(BoxTreeLeaf[A](_, depth + 1, capacity, depthLimit))

  override def copy: BoxTree[A] =
    val newTree = BoxTreeBranch[A](boundary, depth, capacity, depthLimit)
    newTree.subtrees = subtrees.map(_.copy)
    newTree

  override def hasCapacity: Boolean = true

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: BoxedPayload[A]): Unit =
    val boxSplits = subtreeBoundaries.count(d.box.intersects) > 1
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary) match
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
            val newSubtree = BoxTreeBranch[A](subtree.boundary, depth + 1, capacity, depthLimit)
            newSubtree.addAll(subtree.toIterable)
            newSubtree.addOne(dataToAdd)
            newSubtree
    subtrees = updatedSubtrees

  override def remove(d: BoxedPayload[A]): Unit =
    subtrees.foreach: subtree =>
      d.box
        .intersection(subtree.boundary)
        .foreach: newBox =>
          subtree.remove(d.withBox(newBox))

  override def get(range: Box): Iterable[BoxedPayload[A]] =
    if boundary.intersects(range)
    then subtrees.flatMap(subtree => range.intersection(subtree.boundary).flatMap(subtree.get))
    else Vector.empty

  override def toIterable: Iterable[BoxedPayload[A]] = subtrees.flatMap(_.toIterable)

  override def clear(): Unit = // recursively clear, leaving structure in place
    subtrees.foreach(_.clear())
