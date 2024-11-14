package intervalidus.collection.immutable

import intervalidus.collection.{BoxLike, BoxTreeLike, BoxTreeObjectLike, BoxedPayloadLike, CoordinateLike}

/**
  * Immutable box tree base type. Mutation operations return a new tree with updates applied.
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
trait ImmutableBoxTreeLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  Self <: ImmutableBoxTreeLike[A, C, B, P, Self]
] extends BoxTreeLike[A, C, B, P, Self]:
  this: Self =>

  /**
    * Inserts/updates boxed data into the tree.
    * @param d
    *   boxed data to insert/update.
    * @return
    *   updated tree.
    */
  def addOne(d: P): Self

  /**
    * Deletes boxed data from the tree.
    * @param d
    *   boxed data to delete.
    * @return
    *   updated tree.
    */
  def remove(d: P): Self

  /**
    * Clears data.
    * @return
    *   updated tree.
    */
  def clear: Self

  /**
    * Adds all the data.
    * @param ds
    *   boxed data to insert/update.
    * @return
    *   updated tree.
    */
  def addAll(ds: IterableOnce[P]): Self //  = ds.iterator.foldLeft(this)(_.addOne(_))

/**
  * A leaf holds a list of data (up to capacity) for a particular subtree.
  */
trait ImmutableBoxTreeLeafLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  SuperSelf <: ImmutableBoxTreeLike[A, C, B, P, SuperSelf]
] extends ImmutableBoxTreeLike[A, C, B, P, SuperSelf]:
  this: SuperSelf =>

  // create new instances with the same boundary, depth, capacity, and depthLimit
  protected def newLeaf(data: List[P]): SuperSelf
  protected def newBranch: SuperSelf

  override def copy: SuperSelf = newLeaf(data)

  // state
  def data: List[P]

  private def hasCapacity: Boolean =
    data.length < capacity || depth == depthLimit

  override def addOne(d: P): SuperSelf =
    // If there is room, we add the boxed payload here. Otherwise, we create a branch (which creates new leaves)
    // and push the new and existing into it.
    if hasCapacity then newLeaf(d :: data) else data.foldLeft(newBranch.addOne(d))(_.addOne(_))

  // ignores parentBox
  override def remove(d: P): SuperSelf =
    newLeaf(data.filterNot(x => x.box == d.box && x.payload == d.payload))

  override def get(range: B): Iterable[P] = data.filter(d => range.intersects(d.box))

  override def toIterable: Iterable[P] = data

  override def clear: SuperSelf = newLeaf(List.empty)

/**
  * A branch divides the management of data into multiple subtrees -- no data are stored on the branch itself.
  */
trait ImmutableBoxTreeBranchLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  SuperSelf <: ImmutableBoxTreeLike[A, C, B, P, SuperSelf]
] extends ImmutableBoxTreeLike[A, C, B, P, SuperSelf]:
  this: SuperSelf =>

  // manage subtree state
  protected def subtreeBoundaries: Vector[B]
  def subtrees: Vector[SuperSelf]

  protected def newBranch(
    subtrees: Vector[SuperSelf]
  ): SuperSelf

  override def copy: SuperSelf = newBranch(subtrees)

  // Here we may have to split boxes that overlap our subtree boundaries
  override def addOne(d: P): SuperSelf =
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

  override def remove(d: P): SuperSelf =
    val updatedSubtrees = subtrees.map: subtree =>
      d.box.intersection(subtree.boundary) match
        case None         => subtree
        case Some(newBox) => subtree.remove(d.withBox(newBox))
    newBranch(updatedSubtrees)

  override def get(range: B): Iterable[P] =
    if boundary.intersects(range)
    then subtrees.flatMap(q => range.intersection(q.boundary).flatMap(q.get))
    else Vector.empty

  override def toIterable: Iterable[P] = subtrees.flatMap(_.toIterable)

  override def clear: SuperSelf = // recursively clear, leaving structure in place
    newBranch(subtrees.map(_.clear))

/**
  * Constructors and types for immutable box trees.
  *
  * @tparam C
  *   F-bounded coordinate type
  * @tparam B
  *   F-bounded box type (depends on the coordinate type)
  */
trait ImmutableBoxTreeObjectLike[C <: CoordinateLike[C], B <: BoxLike[C, B]] extends BoxTreeObjectLike:

  type BoxedPayloadType[A] <: BoxedPayloadLike[A, C, B, BoxedPayloadType[A]]
  type SelfType[A] <: ImmutableBoxTreeLike[A, C, B, BoxedPayloadType[A], SelfType[A]]

  def apply[A](boundary: B, capacity: Int, depthLimit: Int): SelfType[A]

  def from[A](
    boundary: B,
    ds: IterableOnce[BoxedPayloadType[A]],
    capacity: Int = defaultCapacity,
    depthLimit: Int = defaultDepthLimit
  ): SelfType[A] = apply(boundary, capacity, depthLimit).addAll(ds)
