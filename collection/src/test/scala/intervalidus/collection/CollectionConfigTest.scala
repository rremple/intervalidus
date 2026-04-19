package intervalidus.collection

import intervalidus.collection.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollectionConfigTest extends AnyFunSuite with Matchers:

  test("Basic configuration"):
    val config = CollectionConfig.default
    config shouldBe summon[CollectionConfig]
    config.nodeCapacity shouldBe CollectionConfig.defaultNodeCapacity
    config.log("Ignore this!")

    var logged: Vector[String] = Vector.empty
    def capture(s: Any): Unit =
      logged = logged.appended(s.toString)
    val customConfig = config.withLogAction(capture)
    customConfig.log("Hello")
    customConfig.log("world")
    logged shouldBe Vector("Hello", "world")

  test("BoxTree configuration"):
    val specialNodeCapacity = 1023
    val specialDepthLimit = 31
    System.setProperty("INTERVALIDUS_TREE_NODE_CAPACITY", specialNodeCapacity.toString)
    System.setProperty("INTERVALIDUS_TREE_DEPTH_LIMIT", specialDepthLimit.toString)
    CollectionConfig.nodeCapacityFromEnvironment shouldBe specialNodeCapacity
    CollectionConfig.depthLimitFromEnvironment shouldBe specialDepthLimit

    System.clearProperty("INTERVALIDUS_TREE_NODE_CAPACITY")
    System.clearProperty("INTERVALIDUS_TREE_DEPTH_LIMIT")
    CollectionConfig.nodeCapacityFromEnvironment shouldBe CollectionConfig.defaultNodeCapacity
    CollectionConfig.depthLimitFromEnvironment shouldBe CollectionConfig.defaultDepthLimit
