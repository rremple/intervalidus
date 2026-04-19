package intervalidus.collection.mutable

import intervalidus.collection.*

import scala.collection.mutable

/**
  * Constructors and types for mutable box search trees of arbitrary dimensions.
  */
object BoxTree:
  def apply[A](boundary: Boundary)(using config: CollectionConfig): BoxTree[A] =
    BoxTreeBranch[A](boundary, 0)

  def from[A](boundary: Boundary, ds: IterableOnce[BoxedPayload[A]])(using config: CollectionConfig): BoxTree[A] =
    val newTree: BoxTree[A] = apply(boundary)
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
class BoxTreeLeaf[A](
  val boundary: Boundary,
  val depth: Int,
  dataSharedReference: mutable.ArrayBuffer[BoxedPayload[A]] = null
)(using val config: CollectionConfig)
  extends BoxTree[A]:

  private var copyOnWrite: Boolean = dataSharedReference != null // do we have a shared reference?

  // state
  private var data: mutable.ArrayBuffer[BoxedPayload[A]] =
    if copyOnWrite then dataSharedReference // referenced data will be copied on write
    else new mutable.ArrayBuffer(config.nodeCapacity / 2)

  private inline def ensureWritable(): Unit =
    if copyOnWrite then
      data = mutable.ArrayBuffer.from(this.data) // have to copy the referenced data
      copyOnWrite = false

  override def copy: BoxTree[A] =
    copyOnWrite = true // mark the local leaf as requiring copy on any subsequent write...
    BoxTreeLeaf[A](
      boundary,
      depth,
      dataSharedReference = this.data // ...then it is safe to share a reference to this leaf's data
    )

  override def hasCapacity: Boolean = data.length < config.nodeCapacity || depth == config.depthLimit

  override def addOne(d: BoxedPayload[A]): Unit =
    ensureWritable()
    data.addOne(d)

  // ignores parentBox
  override def remove(d: BoxedPayload[A]): Unit =
    ensureWritable()
    // like data.indexWhere, but avoiding iterator/closure
    var i = 0
    var index = -1
    while index == -1 && i < data.length do
      val x = data(i)
      if x.box == d.box && x.payload == d.payload then index = i
      i = i + 1
    if index >= 0 then
      // swap-and-remove: like data.remove(index), but avoiding partial array copy
      val lastIndex = data.length - 1
      data(index) = data(lastIndex)
      data.remove(lastIndex)

  override def get(range: Box): Iterable[BoxedPayload[A]] = data.filter(d => range.intersects(d.box))

  override def toIterableOnce: IterableOnce[BoxedPayload[A]] = data

  override def toIterable: Iterable[BoxedPayload[A]] = data

  override def clear(): Unit =
    ensureWritable()
    data.clear()

/**
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
class BoxTreeBranch[A](
  initialBoundary: Boundary,
  val depth: Int,
  subtreesSharedReference: Vector[BoxTree[A]] = null
)(using val config: CollectionConfig)
  extends BoxTree[A]:

  private var copyOnWrite: Boolean = subtreesSharedReference != null // do we have a shared reference?

  // manage subtree state
  private var dynamicBoundary: Boundary = initialBoundary // can grow
  private var subtreeBoundaries: Vector[Boundary] = dynamicBoundary.binarySplit
  private var subtrees: Vector[BoxTree[A]] =
    if copyOnWrite then subtreesSharedReference // referenced subtrees will be copied on write
    else subtreeBoundaries.map(BoxTreeLeaf[A](_, depth + 1))

  // A subtree boundary within a branch always has the properties that
  //  `branch.box contains subtree.box` and `branch.capacity contains subtree.capacity`
  // require(
  //  subtreeBoundaries.forall(subtree =>
  //    (boundary.box contains subtree.box) && (boundary.capacity contains subtree.capacity)
  //  ),
  //  s"not all subtrees are nested properly in the boundary"
  // )

  override def boundary: Boundary = dynamicBoundary

  private inline def ensureWritable(): Unit =
    if copyOnWrite then
      subtrees = subtrees.map(_.copy) // have to copy the referenced subtrees (just one level)
      copyOnWrite = false

  override def copy: BoxTree[A] =
    copyOnWrite = true // mark the local branch as requiring copy on any subsequent write...
    BoxTreeBranch[A](
      boundary,
      depth,
      subtreesSharedReference = subtrees // ...then it is safe to share a reference to this branch's subtrees
    )

  override def hasCapacity: Boolean = true

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: BoxedPayload[A]): Unit =
    ensureWritable()
    // If this is the root branch, we may have to grow the boundary and rehash if the boxed payload is out of bounds
    if depth == 0 && !(boundary contains d.box)
    then // out of bounds, so first we grow the boundary and repartition everything
      val newBoundary = boundary.growAround(d.box)
      // config.log(s"resizing based on ${d.box} not fitting in capacity ${boundary.capacity}")
      val tempBranch = BoxTreeBranch[A](newBoundary, depth)
      tempBranch.addAll(BoxedPayload.deduplicateIterableOnce(toIterableOnce))
      dynamicBoundary = newBoundary
      subtreeBoundaries = tempBranch.subtreeBoundaries
      subtrees = tempBranch.subtrees

    val boxSplits = subtreeBoundaries.count(_.box.intersects(d.box)) > 1
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary.box) match
        case None         => subtree
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
            val newSubtree = BoxTreeBranch[A](subtree.boundary, depth + 1)
            newSubtree.addAll(subtree.toIterableOnce)
            newSubtree.addOne(dataToAdd)
            newSubtree
    subtrees = updatedSubtrees

  override def remove(d: BoxedPayload[A]): Unit =
    ensureWritable()
    subtrees.foreach: subtree =>
      d.box
        .intersection(subtree.boundary.box)
        .foreach: newBox =>
          subtree.remove(d.withBox(newBox))

  override def get(range: Box): Iterable[BoxedPayload[A]] =
    subtrees.flatMap(subtree => range.intersection(subtree.boundary.box).flatMap(subtree.get))

  override def toIterable: Iterable[BoxedPayload[A]] = subtrees.flatMap(_.toIterable)

  override def toIterableOnce: IterableOnce[BoxedPayload[A]] = subtrees.iterator.flatMap(_.toIterableOnce)

  override def clear(): Unit = // recursively clear, leaving the structure in place
    ensureWritable()
    subtrees.foreach(_.clear())
