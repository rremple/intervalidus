package intervalidus.collection

/**
  * Collection that manages boxed data structures in multidimensional double space. This tree hold boxes rather than
  * individual points. Boxes are split to fit into the subtrees of the data structures (B-trees, quadtrees, or octrees,
  * depending on the dimension).
  *
  * These collections leverage the ordered hash functions on discrete domain components (which only support
  * successor/predecessor functions and not functions requiring distance calculations, e.g., midpoint) to improve
  * performance of data structures in one-, two-, and three-dimensional discrete domain space.
  *
  * In particular, it makes querying faster for box intersections. The cost is that duplicates can be returned (since
  * boxes can be split). Benchmarks have shown a 7x-11x improvement in throughput of random `DataIn2D.set` operations
  * (depending on number and size of the intervals used), so clearly the speed benefit outweighs the cost of
  * deduplicating results (at least in two dimensions). The mutable version was also faster than the immutable version,
  * but only slightly (less than 5%).
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
trait BoxTreeLike[
  A,
  C <: CoordinateLike[C],
  B <: BoxLike[C, B],
  P <: BoxedPayloadLike[A, C, B, P],
  Self <: BoxTreeLike[A, C, B, P, Self]
]:
  /**
    * Make a copy of this tree.
    * @return
    *   A new tree with the same elements.
    */
  def copy: Self

  /**
    * The box in which data can be managed.
    */
  def boundary: B

  /**
    * The target number of elements a leaf can manage until it will be split into a branch. The number of elements can
    * exceed this target if the tree reaches the depth limit.
    */
  def capacity: Int

  /**
    * The depth of this leaf/branch.
    */
  def depth: Int

  /**
    * Leaves are split into branches when they reach capacity **unless** that would add a leaf to a depth that would
    * exceed this depth limit.
    *
    * @note
    *   Having a depth limit is not just an optimization, it is important functionally because of the potential of
    *   ordered hash collisions used as the box coordinates. Say that there are many source discrete values that hash to
    *   the same double value. If the number of these colliding items reaches the capacity of a leaf, without a depth
    *   limit there would be an infinite regression branch creations until OOM.
    */
  def depthLimit: Int

  /**
    * Queries the tree for boxes intersecting a certain range. Because boxes can be split across multiple quadrants, the
    * data returned can have duplicates, i.e., same data value with the same parent box. (Use
    * [[BoxedPayload.deduplicate]] to remove these duplicates.)
    * @param range
    *   box to query.
    * @return
    *   boxed data intersecting the range.
    */
  def get(range: B): Iterable[P]

  /**
    * A utility method to show data (for testing purposes). Because boxes can be split, the data returned can have
    * duplicates
    * @return
    *   all the data.
    */
  def toIterable: Iterable[P]
