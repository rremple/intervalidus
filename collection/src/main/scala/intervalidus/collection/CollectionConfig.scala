package intervalidus.collection

import scala.util.Properties

case class CollectionConfig(
  /**
    * The target number of elements a leaf can manage until it is split into a branch. The number of elements can exceed
    * this target if the tree reaches the depth limit.
    *
    * Default capacity of leaf nodes. Default is 1024, which was found to be optimal in benchmarks. It can be overridden
    * by setting the environment variable or system property `INTERVALIDUS_TREE_NODE_CAPACITY`.
    */
  nodeCapacity: Int,

  /**
    * Leaves are split into branches when they reach the node capacity **unless** that would add a leaf to a depth that
    * would exceed this depth limit.
    *
    * Default depth limit of trees. Default is 32, which was found to be optimal in "set" benchmarks (though it was
    * observed that any value > 17 is good). It can be overridden by setting the environment variable or system property
    * `INTERVALIDUS_TREE_DEPTH_LIMIT`.
    *
    * @note
    *   Having a depth limit is not just an optimization, it is important functionally because of the potential of
    *   ordered hash collisions used as the box coordinates. Say that there are many source discrete values that hash to
    *   the same double value. If the number of these colliding items reaches the node capacity of a leaf, without a
    *   depth limit, there would be an infinite regression of branch creations until OOM.
    */
  depthLimit: Int,

  /**
    * Used for logging, which logs to the console by default. (You can swap for 'yourLogger.info')
    */
  log: Any => Unit
):
  def withNodeCapacity(value: Int): CollectionConfig = copy(nodeCapacity = value)
  def withDepthLimit(value: Int): CollectionConfig = copy(depthLimit = value)
  def withLogAction(value: Any => Unit): CollectionConfig = copy(log = value)

object CollectionConfig:
  // Initially based on benchmarks of 2D "set" on initial 10K random boxes (up to 1K on each side) in [-500K..500K]^2
  // space. Revised based on DataMonoid benchmarks of 1D-5D "union" and "get" on varying numbers of random boxes in
  // [-1000..1000]^n space.
  /**
    * Default capacity of leaf nodes. Default is 1024, which was found to be optimal in benchmarks. It can be overridden
    * by setting the environment variable or system property `INTERVALIDUS_TREE_NODE_CAPACITY`.
    */
  val defaultNodeCapacity: Int = 1024

  /**
    * Default depth limit of trees. Default is 32, which was found to be optimal in "set" benchmarks (though it was
    * observed that any value > 17 is good). It can be overridden by setting the environment variable or system property
    * `INTERVALIDUS_TREE_DEPTH_LIMIT`.
    */
  val defaultDepthLimit = 32

  private def configIntFromEnvironment(name: String, property: String, defaultSetting: Int, floor: Int): Int =
    val newSetting = Option(System.getProperty(property))
      .orElse(Properties.envOrNone(property))
      .flatMap(_.toIntOption)
      .getOrElse(defaultSetting)
      .max(floor)
    if newSetting != defaultSetting then println(s"[Intervalidus] $name changed from $defaultSetting to $newSetting")
    newSetting

  def nodeCapacityFromEnvironment: Int = configIntFromEnvironment(
    name = "Search tree leaf node capacity",
    property = "INTERVALIDUS_TREE_NODE_CAPACITY",
    defaultSetting = defaultNodeCapacity,
    floor = 2
  )
  def depthLimitFromEnvironment: Int = configIntFromEnvironment(
    name = "Search tree depth limit",
    property = "INTERVALIDUS_TREE_DEPTH_LIMIT",
    defaultSetting = defaultDepthLimit,
    floor = 2
  )

  /**
    * @note
    *   Default is set once, so whatever the environment has at launch is it. If you want to change settings for
    *   different trees use a non-default context argument to BoxTree.
    */
  given default: CollectionConfig = CollectionConfig(
    log = s => println(s),
    nodeCapacity = nodeCapacityFromEnvironment,
    depthLimit = depthLimitFromEnvironment
  )
