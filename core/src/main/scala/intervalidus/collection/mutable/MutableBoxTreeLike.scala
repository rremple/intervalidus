package intervalidus.collection.mutable

import intervalidus.collection.{BoxLike, BoxTreeLike, BoxTreeObjectLike, BoxedPayloadLike, CoordinateLike}

/**
  * Mutable box tree base type. Mutation operations are applied in place.
  *
  * @tparam A
  *   payload type
  * @tparam C
  *   F-bounded coordinate type
  * @tparam B
  *   F-bounded box type (depends on the coordinate type)
  * @tparam P
  *   F-bounded boxed payload type (depends on the payload, coordinate, and box types)
  * @tparam Self
  *   F-bounded self type
  */
trait MutableBoxTreeLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  Self <: MutableBoxTreeLike[A, C, B, P, Self]
] extends BoxTreeLike[A, C, B, P, Self]:
  this: Self =>

  /**
    * Inserts/updates boxed data into the tree.
    * @param d
    *   boxed data to insert/update.
    */
  def addOne(d: P): Unit

  /**
    * Deletes boxed data from the tree.
    * @param d
    *   boxed data to delete.
    */
  def remove(d: P): Unit

  /**
    * Clears data.
    */
  def clear(): Unit

  /**
    * Indicates if a node in the tree has capacity for more elements. Because this is always true for branches, only
    * leaves override this method. (Only useful in mutable context, where branches interrogate the capacity of leaves.)
    *
    * @return
    *   true if there's capacity remaining, otherwise false.
    */
  def hasCapacity: Boolean = true

  /**
    * Adds all the data.
    * @param ds
    *   boxed data to insert/update.
    */
  def addAll(ds: IterableOnce[P]): Unit =
    ds.iterator.foreach(addOne)

/**
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
trait MutableBoxTreeLeafLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  SuperSelf <: MutableBoxTreeLike[A, C, B, P, SuperSelf]
] extends MutableBoxTreeLike[A, C, B, P, SuperSelf]:
  this: SuperSelf =>

  // state
  var data: List[P] = List.empty

  override def hasCapacity: Boolean =
    data.length < capacity || depth == depthLimit

  override def addOne(d: P): Unit =
    data = d :: data

  // ignores parentBox
  override def remove(d: P): Unit =
    data = data.filterNot(x => x.box == d.box && x.payload == d.payload)

  override def get(range: B): Iterable[P] =
    data.filter(d => range.intersects(d.box))

  override def toIterable: Iterable[P] =
    data

  override def clear(): Unit =
    data = List.empty

/**
  * A branch divides the management of data into many subtrees -- no data are stored on the branch itself
  */
trait MutableBoxTreeBranchLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  SuperSelf <: MutableBoxTreeLike[A, C, B, P, SuperSelf]
] extends MutableBoxTreeLike[A, C, B, P, SuperSelf]:
  this: SuperSelf =>

  // manage subtree state
  protected def subtreeBoundaries: Vector[B]
  def subtrees: Vector[MutableBoxTreeLike[A, C, B, P, SuperSelf]]
  def updateSubtrees(updatedSubtrees: Vector[MutableBoxTreeLike[A, C, B, P, SuperSelf]]): Unit

  // create new instances
  protected def newBranch(
    boundary: B,
    depth: Int
  ): MutableBoxTreeBranchLike[A, C, B, P, SuperSelf]

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: P): Unit =
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
            val newSubtree = newBranch(subtree.boundary, depth + 1)
            newSubtree.addAll(subtree.toIterable)
            newSubtree.addOne(dataToAdd)
            newSubtree
    updateSubtrees(updatedSubtrees)

  override def remove(d: P): Unit =
    subtrees.foreach: subtree =>
      d.box
        .intersection(subtree.boundary)
        .foreach: newBox =>
          subtree.remove(d.withBox(newBox))

  override def get(range: B): Iterable[P] =
    if boundary.intersects(range)
    then subtrees.flatMap(q => range.intersection(q.boundary).flatMap(q.get))
    else Vector.empty

  override def toIterable: Iterable[P] = subtrees.flatMap(_.toIterable)

  override def clear(): Unit = subtrees.foreach(_.clear()) // recursively clear, leaving structure in place
  // updateSubtrees(subtreeBoundaries.map(newLeaf(_, depth + 1))) // truncate structure

/**
  * Constructors and types for mutable box trees.
  *
  * @tparam C
  *   F-bounded coordinate type
  * @tparam B
  *   F-bounded box type (depends on the coordinate type)
  */
trait MutableBoxTreeObjectLike[C <: CoordinateLike[C], B <: BoxLike[C, B]] extends BoxTreeObjectLike:

  type BoxedPayloadType[A] <: BoxedPayloadLike[A, C, B, BoxedPayloadType[A]]
  type SelfType[A] <: MutableBoxTreeLike[A, C, B, BoxedPayloadType[A], SelfType[A]]

  def apply[A](boundary: B, capacity: Int, depthLimit: Int): SelfType[A]

  def from[A](
    boundary: B,
    ds: IterableOnce[BoxedPayloadType[A]],
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): SelfType[A] =
    val newTree: SelfType[A] = apply(boundary, capacity, depthLimit)
    newTree.addAll(ds)
    newTree
