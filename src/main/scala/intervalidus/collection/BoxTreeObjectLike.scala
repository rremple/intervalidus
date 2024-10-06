package intervalidus.collection

import scala.util.Properties

/**
  * Attributes common to all box tree companions, settable as environment variables.
  */
trait BoxTreeObjectLike:
  // based on benchmarks of 2D "set" on initial 10K random boxes (up to 1K on each side) in [-500K..500K]^2 space
  /**
    * Default capacity of leaf nodes. Default is 256, which was found to be optimal in benchmarks.
    */
  val defaultCapacity: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_NODE_CAPACITY", "256")
    .toInt
  // println(s"using search tree leaf node capacity = $defaultCapacity")

  /**
    * Default depth limit of trees. Default is 32, which was found to be optimal in "set" benchmarks (though any value >
    * 17 is good).
    */
  val defaultDepthLimit: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_DEPTH_LIMIT", "32")
    .toInt
  // println(s"using search tree depth limit = $defaultDepthLimit")
