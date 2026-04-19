package intervalidus.collection

/**
  * Collection that manages boxed data structures in multidimensional double space. This tree holds boxes rather than
  * individual points. The tree is a hyperoctree (i.e., a B-tree, quadtree, octree, etc., depending on the dimension).
  * Boxes are split to fit into the subtrees (i.e., the hyperoctant/orthants) of the data structures.
  *
  * See [[https://en.wikipedia.org/wiki/Tree_(abstract_data_type)]], [[https://en.wikipedia.org/wiki/B-tree]],
  * [[https://en.wikipedia.org/wiki/Quadtree]], [[https://en.wikipedia.org/wiki/Octree]],
  * [[https://en.wikipedia.org/wiki/Orthant]], and [[https://mathworld.wolfram.com/Hyperoctant.html]].
  *
  * These collections leverage the ordered hash functions on domain values to improve the performance of data structures
  * in multidimensional domain space.
  *
  * In particular, it makes querying faster for box intersections. However, duplicates can be returned (since boxes can
  * be split) as well as false positives (since ordered hashes of discrete values can have collisions). Benchmarks have
  * shown 10x-70x improvement in throughput of random `Data.set` and 10x-140x improvement in `Data.intersects`
  * operations (depending on the number and size of the intervals used), so clearly the speed benefit outweighs the cost
  * of deduplicating results (at least in two dimensions). The mutable version was also faster than the immutable
  * version, but only slightly (less than 5%).
  *
  * @tparam A
  *   payload type
  * @tparam Self
  *   F-bounded self-type
  */
trait BoxTreeLike[A, Self <: BoxTreeLike[A, Self]]:

  /**
    * Configuration for this box tree, including its node capacity and depth limit.
    */
  given config: CollectionConfig

  /**
    * Make a copy of this tree.
    * @return
    *   A new tree with the same elements.
    */
  def copy: Self

  /**
    * The box in which data can be managed.
    */
  def boundary: Boundary

  /**
    * The depth of this leaf/branch.
    */
  def depth: Int

  /**
    * Queries the tree for boxes intersecting a certain range. Because boxes can be split across multiple subtrees, the
    * data returned can have duplicates, i.e., same data value with the same parent box. (Use
    * [[BoxedPayload.deduplicate]] to remove these duplicates.)
    * @param range
    *   box to query.
    * @return
    *   boxed data intersecting the range.
    */
  def get(range: Box): Iterable[BoxedPayload[A]]

  /**
    * Queries the tree for boxes intersecting a certain range and deduplicates the results.
    * @param range
    *   box to query.
    * @return
    *   deduplicated boxed data intersecting the range.
    */
  def getAndDeduplicate(range: Box): Iterable[BoxedPayload[A]] =
    BoxedPayload.deduplicate(get(range))

  /**
    * Returns an iterable once (e.g., an Iterator) of all data. Useful for root-level reorgs to reduce memory pressure.
    *
    * @return
    *   all the data.
    */
  def toIterableOnce: IterableOnce[BoxedPayload[A]]

  /**
    * A utility method to show data (for testing purposes). Because boxes can be split, the data returned can have
    * duplicates
    * @return
    *   all the data.
    */
  def toIterable: Iterable[BoxedPayload[A]]
