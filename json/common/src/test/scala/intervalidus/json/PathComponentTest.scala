package intervalidus.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PathComponentTest extends AnyFunSuite with Matchers:

  import PathComponent.{ArrayPathComponent, ObjectPathComponent}

  test("Path component constructors"):
    PathComponent.toObject("key") shouldBe ObjectPathComponent("key")
    PathComponent.toArrayIndex(1).toString shouldBe "[1]"
    PathComponent.toArrayAll.toString shouldBe "[*]"
    PathComponent.toArraySlice(from = 1).toString shouldBe "[1:]"
    PathComponent.toArraySlice(until = 4).toString shouldBe "[0:4]"
    PathComponent.toArraySlice(1, 4).toString shouldBe "[1:4]"

  test("Path component matching"):
    assert(!(PathComponent.toObject("1") matches PathComponent.toArrayIndex(1)))
    assert(PathComponent.toArraySlice(0, 4) matches PathComponent.toArrayIndex(1))
    assert(PathComponent.toArrayIndex(1) matches PathComponent.toArraySlice(0, 4))
    assert(PathComponent.toArraySlice(1, 4) matches PathComponent.toArraySlice(1))
    assert(PathComponent.toArraySlice(1) matches PathComponent.toArraySlice(1))
    assert(!(PathComponent.toArraySlice(1, 4) matches PathComponent.toArraySlice(4, 8)))
    assert(PathComponent.toArraySlice(1, 4) matches PathComponent.toArrayIndex(1))
    assert(PathComponent.toArraySlice(1, 4) matches PathComponent.toArrayIndex(1))
