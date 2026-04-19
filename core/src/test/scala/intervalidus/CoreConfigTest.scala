package intervalidus

import intervalidus.collection.CollectionConfig
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class CoreConfigTest extends AnyFunSuite with Matchers:

  import DiscreteValue.IntDiscreteValue
  type Dim = Domain.In1D[Int]

  test("Is configurable"):
    // All defaults
    val config = CoreConfig.default[Dim]
    config shouldBe summon[CoreConfig[Dim]]
    config.capacityHint shouldBe None
    config.experimental.enabled("tada") shouldBe false
    config.collectionConfig.nodeCapacity shouldBe CollectionConfig.defaultNodeCapacity

    { // default with CoreConfig non-default experimental implicit
      given Experimental = Experimental("tada")
      val config = CoreConfig.default[Dim]
      config.capacityHint shouldBe None
      config.experimental.enabled("tada") shouldBe true
      config.collectionConfig.nodeCapacity shouldBe CollectionConfig.defaultNodeCapacity
    }

    { // default with CoreConfig non-default experimental explicit
      val config = CoreConfig.default[Dim].withExperimental(Experimental("tada"))
      config.capacityHint shouldBe None
      config.experimental.enabled("tada") shouldBe true
      config.collectionConfig.nodeCapacity shouldBe CollectionConfig.defaultNodeCapacity
    }

    { // default with CoreConfig non-default collectionConfig implicit
      given CollectionConfig = CollectionConfig.default.withNodeCapacity(5000)
      val config = CoreConfig.default[Dim]
      config.capacityHint shouldBe None
      config.experimental.enabled("tada") shouldBe false
      config.collectionConfig.nodeCapacity shouldBe 5000
    }

    { // default with CoreConfig non-default collectionConfig explicit
      val config = CoreConfig.default[Dim].withCollectionConfig(CollectionConfig.default.withNodeCapacity(5000))
      config.capacityHint shouldBe None
      config.experimental.enabled("tada") shouldBe false
      config.collectionConfig.nodeCapacity shouldBe 5000
    }

    { // non-default with default experimental and collectionConfig
      val capacity: Interval[Dim] = Interval1D.interval(-10, 10)
      val config = CoreConfig.default[Dim].withCapacityHint(capacity)
      config.capacityHint shouldBe Some(capacity)
      config.experimental.enabled("tada") shouldBe false
      config.collectionConfig.nodeCapacity shouldBe CollectionConfig.defaultNodeCapacity
    }

    {
      var logged: Vector[String] = Vector.empty
      def capture(s: Any): Unit =
        logged = logged.appended(s.toString)
      val config = CoreConfig.default[Dim].withLogAction(capture)
      config.log("Hello")
      config.log("world")
      logged shouldBe Vector("Hello", "world")
    }
