package intervalidus.collection

import scala.util.Properties

trait BoxTreeObjectLike:
  // based on benchmarks of 2D "set" on initial 10K random boxes (up to 1K on each side) in [-500K..500K]^2 space
  val defaultCapacity: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_NODE_CAPACITY", "256") // based on benchmarks
    .toInt
  // println(s"using search tree leaf node capacity = $defaultCapacity")

  val defaultDepthLimit: Int = Properties
    .envOrElse("INTERVALIDUS_TREE_DEPTH_LIMIT", "32") // based on "set" benchmarks, any value > 17 is good
    .toInt
  // println(s"using search tree depth limit = $defaultDepthLimit")
