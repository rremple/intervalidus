package intervalidus.collection

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxTest extends AnyFunSuite with Matchers:

  test("Box (1D) methods"):
    def box(start: Double, end: Double): Box =
      Box(Coordinate(start), Coordinate(end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate(50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))
    boundary.binarySplit should contain theSameElementsAs Vector(box(0, 50), box(50, 100))
    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[0.0..100.0] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe "[0.0..100.0] -> Hello (from [0.0..200.0])"

  test("Box (2D) methods"):
    def box(start: Double, end: Double): Box =
      Box(Coordinate(start, start), Coordinate(end, end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate(50, 50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))
    boundary.binarySplit should contain theSameElementsAs Vector(
      Box(Coordinate(0, 0), Coordinate(50, 50)),
      Box(Coordinate(0, 50), Coordinate(50, 100)),
      Box(Coordinate(50, 0), Coordinate(100, 50)),
      Box(Coordinate(50, 50), Coordinate(100, 100))
    )
    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0)..(100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(Box(Coordinate(0, 0), Coordinate(200, 200)))).toString shouldBe
      "[(0.0,0.0)..(100.0,100.0)] -> Hello (from [(0.0,0.0)..(200.0,200.0)])"

  test("Box (3D) methods"):
    def box(start: Double, end: Double): Box =
      Box(Coordinate(start, start, start), Coordinate(end, end, end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate(50, 50, 50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))

    boundary.binarySplit should contain theSameElementsAs Vector(
      Box(Coordinate(0, 0, 0), Coordinate(50, 50, 50)),
      Box(Coordinate(0, 0, 50), Coordinate(50, 50, 100)),
      Box(Coordinate(0, 50, 0), Coordinate(50, 100, 50)),
      Box(Coordinate(0, 50, 50), Coordinate(50, 100, 100)),
      Box(Coordinate(50, 0, 0), Coordinate(100, 50, 50)),
      Box(Coordinate(50, 0, 50), Coordinate(100, 50, 100)),
      Box(Coordinate(50, 50, 0), Coordinate(100, 100, 50)),
      Box(Coordinate(50, 50, 50), Coordinate(100, 100, 100))
    )

    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe
      "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello (from [(0.0,0.0,0.0)..(200.0,200.0,200.0)])"
