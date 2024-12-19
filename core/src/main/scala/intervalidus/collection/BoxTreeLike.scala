package intervalidus.collection

import scala.util.Properties

/**
  * Collection that manages boxed data structures in multidimensional double space. This tree hold boxes rather than
  * individual points. Boxes are split to fit into the subtrees of the data structures (B-trees, quadtrees, or octrees,
  * depending on the dimension).
  *
  * See [[https://en.wikipedia.org/wiki/Tree_(abstract_data_type)]], [[https://en.wikipedia.org/wiki/B-tree]],
  * [[https://en.wikipedia.org/wiki/Quadtree]], and [[https://en.wikipedia.org/wiki/Octree]].
  *
  * These collections leverage the ordered hash functions on discrete domain components (which only support
  * successor/predecessor functions and not functions requiring distance calculations, e.g., midpoint) to improve
  * performance of data structures in one-, two-, and three-dimensional discrete domain space.
  *
  * In particular, it makes querying faster for box intersections. However, duplicates can be returned (since boxes can
  * be split) as well as false positives (since ordered hashes of discrete values can have collisions). Benchmarks have
  * shown 10x-70x improvement in throughput of random `DataIn2D.set` and 10x-140x improvement in `DataIn2D.intersects`
  * operations (depending on number and size of the intervals used), so clearly the speed benefit outweighs the cost of
  * deduplicating results (at least in two dimensions). The mutable version was also faster than the immutable version,
  * but only slightly (less than 5%).
  *
  * @tparam A
  *   payload type
  */
trait BoxTreeLike[A, Self <: BoxTreeLike[A, Self]]:
  this: Self =>

  /**
    * Make a copy of this tree.
    * @return
    *   A new tree with the same elements.
    */
  def copy: Self

  /**
    * The box in which data can be managed.
    */
  def boundary: Box

  /**
    * The target number of elements a leaf can manage until it is split into a branch. The number of elements can exceed
    * this target if the tree reaches the depth limit.
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
    * A utility method to show data (for testing purposes). Because boxes can be split, the data returned can have
    * duplicates
    * @return
    *   all the data.
    */
  def toIterable: Iterable[BoxedPayload[A]]

/**
  * Attributes common to all box search tree companions, settable as environment variables.
  */
trait BoxTreeObjectLike:
  // based on benchmarks of 2D "set" on initial 10K random boxes (up to 1K on each side) in [-500K..500K]^2 space
  /**
    * Default capacity of leaf nodes. Default is 256, which was found to be optimal in benchmarks. Can be overridden by
    * setting the environment variable `INTERVALIDUS_TREE_NODE_CAPACITY`.
    */
  val defaultCapacity: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_NODE_CAPACITY", "256")
    .toInt
  // println(s"using search tree leaf node capacity = $defaultCapacity")

  /**
    * Default depth limit of trees. Default is 32, which was found to be optimal in "set" benchmarks (though it was
    * observed that any value > 17 is good). Can be overridden by setting the environment variable
    * `INTERVALIDUS_TREE_DEPTH_LIMIT`.
    */
  val defaultDepthLimit: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_DEPTH_LIMIT", "32")
    .toInt
// println(s"using search tree depth limit = $defaultDepthLimit")
