package intervalidus.collection

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxTest extends AnyFunSuite with Matchers:

  test("Box1D methods"):
    def box(start: Double, end: Double): Box1D =
      Box1D(Coordinate1D(start), Coordinate1D(end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate1D(50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))
    boundary.left shouldBe box(0, 50)
    boundary.right shouldBe box(50, 100)
    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[0.0..100.0] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe "[0.0..100.0] -> Hello (from [0.0..200.0])"

  test("Box2D methods"):
    def box(start: Double, end: Double): Box2D =
      Box2D(Coordinate2D(start, start), Coordinate2D(end, end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate2D(50, 50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))
    boundary.leftLower shouldBe Box2D(Coordinate2D(0, 0), Coordinate2D(50, 50))
    boundary.leftUpper shouldBe Box2D(Coordinate2D(0, 50), Coordinate2D(50, 100))
    boundary.rightLower shouldBe Box2D(Coordinate2D(50, 0), Coordinate2D(100, 50))
    boundary.rightUpper shouldBe Box2D(Coordinate2D(50, 50), Coordinate2D(100, 100))
    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0)..(100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(Box2D(Coordinate2D(0, 0), Coordinate2D(200, 200)))).toString shouldBe
      "[(0.0,0.0)..(100.0,100.0)] -> Hello (from [(0.0,0.0)..(200.0,200.0)])"

  test("Box3D methods"):
    def box(start: Double, end: Double): Box3D =
      Box3D(Coordinate3D(start, start, start), Coordinate3D(end, end, end))

    val boundary = box(0, 100)
    boundary.midPoint shouldBe Coordinate3D(50, 50, 50)
    assert(boundary.contains(box(50, 60)))
    assert(boundary.contains(box(50, 100)))
    assert(!boundary.contains(box(50, 150)))

    boundary.leftLowerBack shouldBe Box3D(Coordinate3D(0, 0, 0), Coordinate3D(50, 50, 50))
    boundary.leftLowerFront shouldBe Box3D(Coordinate3D(0, 0, 50), Coordinate3D(50, 50, 100))
    boundary.leftUpperBack shouldBe Box3D(Coordinate3D(0, 50, 0), Coordinate3D(50, 100, 50))
    boundary.leftUpperFront shouldBe Box3D(Coordinate3D(0, 50, 50), Coordinate3D(50, 100, 100))
    boundary.rightLowerBack shouldBe Box3D(Coordinate3D(50, 0, 0), Coordinate3D(100, 50, 50))
    boundary.rightLowerFront shouldBe Box3D(Coordinate3D(50, 0, 50), Coordinate3D(100, 50, 100))
    boundary.rightUpperBack shouldBe Box3D(Coordinate3D(50, 50, 0), Coordinate3D(100, 100, 50))
    boundary.rightUpperFront shouldBe Box3D(Coordinate3D(50, 50, 50), Coordinate3D(100, 100, 100))

    assert(boundary.intersects(box(50, 150)))
    boundary.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!boundary.intersects(box(150, 160)))
    boundary.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe
      "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello (from [(0.0,0.0,0.0)..(200.0,200.0,200.0)])"
