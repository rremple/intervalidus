package intervalidus.collection

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxTest extends AnyFunSuite with Matchers:

  test("Box and other runtime checks"):
    val coordinate1D = Coordinate(Some(1.0))
    val coordinateFixed1D = CoordinateFixed(1.0)
    val coordinate2D = Coordinate(Some(1.0), Some(2.0))
    val coordinateFixed2D = CoordinateFixed(1.0, 2.0)

    assertResult(1)(coordinate1D.arity)
    assertResult(2)(coordinate2D.arity)
    assertResult(1)(coordinateFixed1D.arity)
    assertResult(2)(coordinateFixed2D.arity)

    assertThrows[IllegalArgumentException]:
      val _ = Box(coordinate1D, coordinate2D)

    assertThrows[IllegalArgumentException]:
      val _ = Capacity(coordinateFixed1D, coordinateFixed2D)

    assertThrows[IllegalArgumentException]:
      val _ = Boundary(Capacity(coordinateFixed1D, coordinateFixed1D)).withBox(Box(coordinate2D, coordinate2D))

  test("Box (1D) methods"):
    def coordinateFixed(c: Double): CoordinateFixed = CoordinateFixed(c)
    def coordinate(c: Double): Coordinate = Coordinate(Some(c))
    def coordinateUnbound: Coordinate = Coordinate(None)
    def capacity(start: Double, end: Double): Capacity = Capacity(coordinateFixed(start), coordinateFixed(end))
    def box(start: Double, end: Double): Box = Box(coordinate(start), coordinate(end))
    def boxTo(end: Double): Box = Box(coordinateUnbound, coordinate(end))
    def boxFrom(start: Double): Box = Box(coordinate(start), coordinateUnbound)
    def boxUnbound: Box = Box(coordinateUnbound, coordinateUnbound)

    val boundary = Boundary(capacity(40, 60)).withBox(box(0, 100))
    assertResult(s"Boundary: box [0.0..100.0], capacity [40.0..60.0]")(boundary.toString)

    boundary.capacity.midPoint shouldBe coordinateFixed(50)
    assertResult(box(50, 50))(Box.at(coordinate(50)))
    assert(boundary.box contains box(50, 60))
    assert(boundary.capacity contains box(50, 60))
    assert(boundary.box contains box(50, 100))
    assert(!(boundary.capacity contains box(50, 100)))
    assert(!(boundary.box contains box(50, 150)))
    assert(!(boundary.capacity contains box(50, 150)))

    assert(boxTo(60) contains boxTo(50))
    assert(boxUnbound contains boxTo(50))
    assert(!(box(40, 60) contains boxTo(50)))
    assert(boxFrom(40) contains boxFrom(50))
    assert(boxUnbound contains boxFrom(50))
    assert(!(box(40, 60) contains boxFrom(50)))
    // assert(!(box(40, 60) contains boxUnbound))

    assert(boxTo(60) intersects boxTo(50))
    assert(boxUnbound intersects boxTo(50))
    assert(!(boxTo(50) intersects boxFrom(60)))
    assert(boxFrom(50) intersects boxFrom(60))
    assert(boxUnbound intersects boxFrom(60))
    assert(!(boxFrom(60) intersects boxTo(50)))

    boundary.binarySplit.map(_.box) should contain theSameElementsAs Vector(box(0, 50), box(50, 100))
    boundary.binarySplit.map(_.capacity) should contain theSameElementsAs Vector(capacity(40, 50), capacity(50, 60))
    assert(boundary.box intersects box(50, 150))
    boundary.box.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!(boundary.box intersects box(150, 160)))
    boundary.box.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary.box -> "Hello"
    boxedPayload.toString shouldBe "[0.0..100.0] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe "[0.0..100.0] -> Hello (from [0.0..200.0])"

  test("Box (2D) methods"):
    def coordinateFixed(c1: Double, c2: Double): CoordinateFixed =
      CoordinateFixed(c1, c2)
    def coordinate(c1: Double, c2: Double): Coordinate =
      Coordinate(Some(c1), Some(c2))
    def capacity(start: Double, end: Double): Capacity =
      Capacity(coordinateFixed(start, start), coordinateFixed(end, end))
    def box(start: Double, end: Double): Box =
      Box(coordinate(start, start), coordinate(end, end))

    val boundary = Boundary(capacity(40, 60)).withBox(box(0, 100))
    assertResult(s"Boundary: box [(0.0,0.0)..(100.0,100.0)], capacity [(40.0,40.0)..(60.0,60.0)]")(boundary.toString)

    boundary.capacity.midPoint shouldBe coordinateFixed(50, 50)
    assert(boundary.box contains box(50, 60))
    assert(boundary.capacity contains box(50, 60))
    assert(boundary.box contains box(50, 100))
    assert(!(boundary.capacity contains box(50, 100)))
    assert(!(boundary.box contains box(50, 150)))
    assert(!(boundary.capacity contains box(50, 150)))
    boundary.binarySplit.map(_.box) should contain theSameElementsAs Vector(
      Box(coordinate(0, 0), coordinate(50, 50)),
      Box(coordinate(0, 50), coordinate(50, 100)),
      Box(coordinate(50, 0), coordinate(100, 50)),
      Box(coordinate(50, 50), coordinate(100, 100))
    )
    boundary.binarySplit.map(_.capacity) should contain theSameElementsAs Vector(
      Capacity(coordinateFixed(40, 40), coordinateFixed(50, 50)),
      Capacity(coordinateFixed(40, 50), coordinateFixed(50, 60)),
      Capacity(coordinateFixed(50, 40), coordinateFixed(60, 50)),
      Capacity(coordinateFixed(50, 50), coordinateFixed(60, 60))
    )
    assert(boundary.box intersects box(50, 150))
    boundary.box.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!(boundary.box intersects box(150, 160)))
    boundary.box.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary.box -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0)..(100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(Box(coordinate(0, 0), coordinate(200, 200)))).toString shouldBe
      "[(0.0,0.0)..(100.0,100.0)] -> Hello (from [(0.0,0.0)..(200.0,200.0)])"

  test("Box (3D) methods"):
    def coordinateFixed(c1: Double, c2: Double, c3: Double): CoordinateFixed =
      CoordinateFixed(c1, c2, c3)
    def coordinate(c1: Double, c2: Double, c3: Double): Coordinate =
      Coordinate(Some(c1), Some(c2), Some(c3))
    def capacity(start: Double, end: Double): Capacity =
      Capacity(coordinateFixed(start, start, start), coordinateFixed(end, end, end))
    def box(start: Double, end: Double): Box =
      Box(coordinate(start, start, start), coordinate(end, end, end))

    assertResult(coordinateFixed(50, 60, 50))(
      Coordinate(Some(40), Some(60), None).fixMax(coordinateFixed(50, 50, 50))
    )
    assertResult(coordinateFixed(40, 50, 50))(
      Coordinate(Some(40), Some(60), None).fixMin(coordinateFixed(50, 50, 50))
    )
    assertResult(coordinateFixed(40, 60, 50))(
      Coordinate(Some(40), Some(60), None).fixUnbounded(coordinateFixed(50, 50, 50))
    )
    assertResult(Coordinate(Some(40), Some(50), None, None, None))(
      Coordinate(Some(40), Some(60), None, Some(40), None).projectBeforeStart(
        Coordinate(Some(50), Some(50), Some(40), None, None)
      )
    )
    assertResult(Coordinate(Some(40), Some(50), Some(40), Some(40), None))(
      Coordinate(Some(40), Some(60), None, Some(40), None).projectBeforeEnd(
        Coordinate(Some(50), Some(50), Some(40), None, None)
      )
    )
    assertResult(Coordinate(Some(50), Some(60), Some(40), Some(40), None))(
      Coordinate(Some(40), Some(60), None, Some(40), None).projectAfterStart(
        Coordinate(Some(50), Some(50), Some(40), None, None)
      )
    )
    assertResult(Coordinate(Some(50), Some(60), None, None, None))(
      Coordinate(Some(40), Some(60), None, Some(40), None).projectAfterEnd(
        Coordinate(Some(50), Some(50), Some(40), None, None)
      )
    )

    val boundary = Boundary(capacity(40, 60)).withBox(box(0, 100))
    boundary.capacity.midPoint shouldBe coordinateFixed(50, 50, 50)
    assert(boundary.box contains box(50, 60))
    assert(boundary.capacity contains box(50, 60))
    assert(boundary.box contains box(50, 100))
    assert(!(boundary.capacity contains box(50, 100)))
    assert(!(boundary.box contains box(50, 150)))
    assert(!(boundary.capacity contains box(50, 150)))

    boundary.binarySplit.map(_.box) should contain theSameElementsAs Vector(
      Box(coordinate(0, 0, 0), coordinate(50, 50, 50)),
      Box(coordinate(0, 0, 50), coordinate(50, 50, 100)),
      Box(coordinate(0, 50, 0), coordinate(50, 100, 50)),
      Box(coordinate(0, 50, 50), coordinate(50, 100, 100)),
      Box(coordinate(50, 0, 0), coordinate(100, 50, 50)),
      Box(coordinate(50, 0, 50), coordinate(100, 50, 100)),
      Box(coordinate(50, 50, 0), coordinate(100, 100, 50)),
      Box(coordinate(50, 50, 50), coordinate(100, 100, 100))
    )
    boundary.binarySplit.map(_.capacity) should contain theSameElementsAs Vector(
      Capacity(coordinateFixed(40, 40, 40), coordinateFixed(50, 50, 50)),
      Capacity(coordinateFixed(40, 40, 50), coordinateFixed(50, 50, 60)),
      Capacity(coordinateFixed(40, 50, 40), coordinateFixed(50, 60, 50)),
      Capacity(coordinateFixed(40, 50, 50), coordinateFixed(50, 60, 60)),
      Capacity(coordinateFixed(50, 40, 40), coordinateFixed(60, 50, 50)),
      Capacity(coordinateFixed(50, 40, 50), coordinateFixed(60, 50, 60)),
      Capacity(coordinateFixed(50, 50, 40), coordinateFixed(60, 60, 50)),
      Capacity(coordinateFixed(50, 50, 50), coordinateFixed(60, 60, 60))
    )

    assert(boundary.box intersects box(50, 150))
    boundary.box.intersection(box(50, 150)) shouldBe Some(box(50, 100))
    assert(!(boundary.box intersects box(150, 160)))
    boundary.box.intersection(box(150, 160)) shouldBe None
    val boxedPayload = boundary.box -> "Hello"
    boxedPayload.toString shouldBe "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello"
    boxedPayload.withParentBox(Some(box(0, 200))).toString shouldBe
      "[(0.0,0.0,0.0)..(100.0,100.0,100.0)] -> Hello (from [(0.0,0.0,0.0)..(200.0,200.0,200.0)])"
