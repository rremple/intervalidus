package intervalidus.collection.immutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxQuadtreeTest extends AnyFunSuite with Matchers:
  def capacityCoordinate(coordinates: Double*): CoordinateFixed =
    CoordinateFixed(coordinates.toVector)
  def coordinate(coordinates: Double*): Coordinate =
    Coordinate(coordinates.toVector.map(Some(_)))

  test("Immutable: BoxTree (2D -- Quadtree) methods"):
    // Create a quadtree with a small capacity per node and insert some boxes
    val boundary = Boundary(
      Capacity(capacityCoordinate(-8, -8), capacityCoordinate(8, 8))
    )
    val tree = BoxTree[String](boundary)
      .addOne(Box(coordinate(3, 3), coordinate(5, 5)) -> "one") // right upper
      .addOne(Box(coordinate(-1, 1), coordinate(3, 3)) -> "two") // left and right upper (split)
      .addOne(Box(coordinate(1, -1), coordinate(3, 1)) -> "three") // right lower and upper (split)
      .addOne(Box(coordinate(-1, -1), coordinate(1, 1)) -> "four") // all quadrants (split)
    val result = tree.toIterable.groupMap(_.parentBox)(d => d.copy(parentBox = None))
    result.keys should contain theSameElementsAs
      List(
        None,
        Some(Box(coordinate(-1, 1), coordinate(3, 3))),
        Some(Box(coordinate(1, -1), coordinate(3, 1))),
        Some(Box(coordinate(-1, -1), coordinate(1, 1)))
      )
    result(None) should contain theSameElementsAs Vector(
      Box(coordinate(3, 3), coordinate(5, 5)) -> "one" // right upper
    )
    result(Some(Box(coordinate(-1, 1), coordinate(3, 3)))) should contain theSameElementsAs Vector(
      Box(coordinate(-1, 1), coordinate(0, 3)) -> "two", // left upper
      Box(coordinate(0, 1), coordinate(3, 3)) -> "two" // right upper
    )
    result(Some(Box(coordinate(1, -1), coordinate(3, 1)))) should contain theSameElementsAs Vector(
      Box(coordinate(1, -1), coordinate(3, 0)) -> "three", // right lower
      Box(coordinate(1, 0), coordinate(3, 1)) -> "three" // right upper
    )
    result(Some(Box(coordinate(-1, -1), coordinate(1, 1)))) should contain theSameElementsAs Vector(
      Box(coordinate(-1, -1), coordinate(0, 0)) -> "four", // left lower
      Box(coordinate(-1, 0), coordinate(0, 1)) -> "four", // left upper
      Box(coordinate(0, -1), coordinate(1, 0)) -> "four", // right lower
      Box(coordinate(0, 0), coordinate(1, 1)) -> "four" // right upper
    )

    val expected = List(
      Box(coordinate(3, 3), coordinate(5, 5)) -> "one",
      Box(coordinate(-1, 1), coordinate(3, 3)) -> "two",
      Box(coordinate(1, -1), coordinate(3, 1)) -> "three",
      Box(coordinate(-1, -1), coordinate(1, 1)) -> "four"
    )
    BoxedPayload.deduplicate(tree.toIterable) should contain theSameElementsAs expected
    tree.getAndDeduplicate(Box(coordinate(-1, -1), coordinate(5, 5))) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    // Trees always start as a branch and immutable leaves never get copied, so forcing that to happen here just for
    // coverage -- won't happen in real life!
    val leaf = BoxTreeLeaf[String](boundary, depth = 0, nodeCapacity = 1, depthLimit = 1)
      .addOne(Box(coordinate(3, 3), coordinate(5, 5)) -> "one")
    val leafCopy = leaf.copy
    leaf.toIterable should contain theSameElementsAs List(Box(coordinate(3, 3), coordinate(5, 5)) -> "one")
    leafCopy.toIterable should contain theSameElementsAs leaf.toIterable

    tree.get(Box(coordinate(4, 4), coordinate(5, 5))) should contain theSameElementsAs
      List(Box(coordinate(3, 3), coordinate(5, 5)) -> "one")
    tree.get(Box(coordinate(40, 40), coordinate(50, 50))) shouldBe List.empty

    val treeSplit = BoxTree
      .from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
      .addOne(Box(coordinate(3, 5), coordinate(7, 7)) -> "five") // splits right upper
      .addOne(Box(coordinate(5, 4), coordinate(7, 5)) -> "six")
    treeSplit.get(Box(coordinate(3, 5), coordinate(7, 7))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(
          Box(coordinate(3, 5), coordinate(4, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ), // left upper
        BoxedPayload(
          Box(coordinate(4, 5), coordinate(7, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ), // right upper
        // because they touch y=5 (potential false positive from query)
        BoxedPayload(
          Box(coordinate(3, 4), coordinate(4, 5)),
          "one",
          Some(Box(coordinate(3, 3), coordinate(5, 5)))
        ),
        BoxedPayload(
          Box(coordinate(4, 4), coordinate(5, 5)),
          "one",
          Some(Box(coordinate(3, 3), coordinate(5, 5)))
        ),
        BoxedPayload(
          Box(coordinate(5, 4), coordinate(7, 5)),
          "six",
          Some(Box(coordinate(5, 4), coordinate(7, 5)))
        )
      )

    // because we use y=5.01 below, the ones touching y=5 aren't included
    treeSplit.get(Box(coordinate(3, 5.01), coordinate(7, 7))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(
          Box(coordinate(3, 5), coordinate(4, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ), // left upper
        BoxedPayload(
          Box(coordinate(4, 5), coordinate(7, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ) // right upper
      )

    val shrunkenTree = treeSplit
      .remove(BoxedPayload(Box(coordinate(3, 3), coordinate(5, 5)), "one"))
      .remove(BoxedPayload(Box(coordinate(5, 4), coordinate(7, 5)), "six"))
    shrunkenTree.get(Box(coordinate(3, 5), coordinate(7, 7))) should contain theSameElementsAs
      List(
        BoxedPayload(
          Box(coordinate(3, 5), coordinate(4, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ), // left upper
        BoxedPayload(
          Box(coordinate(4, 5), coordinate(7, 7)),
          "five",
          Some(Box(coordinate(3, 5), coordinate(7, 7)))
        ) // right upper
      )

    val emptyTree = BoxTree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
      .clear
    assert(emptyTree.toIterable.isEmpty)
