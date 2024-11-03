package intervalidus.collection.mutable

import intervalidus.collection.{Box2D, BoxedPayload, BoxedPayload2D, Coordinate2D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxQuadtreeTest extends AnyFunSuite with Matchers:
  test("Mutable: BoxQuadtree methods"):
    // Create a quadtree with a small capacity per node and insert some boxes
    val boundary = Box2D(Coordinate2D(-8, -8), Coordinate2D(8, 8))
    val tree = BoxQuadtree[String](boundary)
    tree.addOne(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)) -> "one") // right upper
    tree.addOne(Box2D(Coordinate2D(-1, 1), Coordinate2D(3, 3)) -> "two") // left and right upper (split)
    tree.addOne(Box2D(Coordinate2D(1, -1), Coordinate2D(3, 1)) -> "three") // right lower and upper (split)
    tree.addOne(Box2D(Coordinate2D(-1, -1), Coordinate2D(1, 1)) -> "four") // all quadrants (split)
    tree.toIterable.groupMap(_.parentBox)(d => d.copy(parentBox = None)) should contain theSameElementsAs
      List(
        None -> Vector(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)) -> "one"), // right upper
        Some(Box2D(Coordinate2D(-1, 1), Coordinate2D(3, 3))) -> Vector(
          Box2D(Coordinate2D(-1, 1), Coordinate2D(0, 3)) -> "two", // left upper
          Box2D(Coordinate2D(0, 1), Coordinate2D(3, 3)) -> "two" // right upper
        ),
        Some(Box2D(Coordinate2D(1, -1), Coordinate2D(3, 1))) -> Vector(
          Box2D(Coordinate2D(1, -1), Coordinate2D(3, 0)) -> "three", // right lower
          Box2D(Coordinate2D(1, 0), Coordinate2D(3, 1)) -> "three" // right upper
        ),
        Some(Box2D(Coordinate2D(-1, -1), Coordinate2D(1, 1))) -> Vector(
          Box2D(Coordinate2D(-1, -1), Coordinate2D(0, 0)) -> "four", // left lower
          Box2D(Coordinate2D(-1, 0), Coordinate2D(0, 1)) -> "four", // left upper
          Box2D(Coordinate2D(0, -1), Coordinate2D(1, 0)) -> "four", // right lower
          Box2D(Coordinate2D(0, 0), Coordinate2D(1, 1)) -> "four" // right upper
        )
      )
    val expected = List(
      Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)) -> "one",
      Box2D(Coordinate2D(-1, 1), Coordinate2D(3, 3)) -> "two",
      Box2D(Coordinate2D(1, -1), Coordinate2D(3, 1)) -> "three",
      Box2D(Coordinate2D(-1, -1), Coordinate2D(1, 1)) -> "four"
    )
    BoxedPayload.deduplicate(tree.toIterable) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    tree.get(Box2D(Coordinate2D(4, 4), Coordinate2D(5, 5))) should contain theSameElementsAs
      List(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)) -> "one")
    tree.get(Box2D(Coordinate2D(40, 40), Coordinate2D(50, 50))) shouldBe List.empty

    val treeSplit = BoxQuadtree.from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
    treeSplit.addOne(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)) -> "five") // splits right upper
    treeSplit.addOne(Box2D(Coordinate2D(5, 4), Coordinate2D(7, 5)) -> "six")
    treeSplit.get(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload2D(
          Box2D(Coordinate2D(3, 5), Coordinate2D(4, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ), // left upper
        BoxedPayload2D(
          Box2D(Coordinate2D(4, 5), Coordinate2D(7, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ), // right upper
        // because they touch y=5 (potential false positive from query)
        BoxedPayload2D(
          Box2D(Coordinate2D(3, 4), Coordinate2D(4, 5)),
          "one",
          Some(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)))
        ),
        BoxedPayload2D(
          Box2D(Coordinate2D(4, 4), Coordinate2D(5, 5)),
          "one",
          Some(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)))
        ),
        BoxedPayload2D(
          Box2D(Coordinate2D(5, 4), Coordinate2D(7, 5)),
          "six",
          Some(Box2D(Coordinate2D(5, 4), Coordinate2D(7, 5)))
        )
      )

    // because we use y=5.01 below, the ones touching y=5 aren't included
    treeSplit.get(Box2D(Coordinate2D(3, 5.01), Coordinate2D(7, 7))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload2D(
          Box2D(Coordinate2D(3, 5), Coordinate2D(4, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ), // left upper
        BoxedPayload2D(
          Box2D(Coordinate2D(4, 5), Coordinate2D(7, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ) // right upper
      )

    val shrunkenTree = treeSplit.copy
    shrunkenTree.remove(BoxedPayload2D(Box2D(Coordinate2D(3, 3), Coordinate2D(5, 5)), "one"))
    shrunkenTree.remove(BoxedPayload2D(Box2D(Coordinate2D(5, 4), Coordinate2D(7, 5)), "six"))
    shrunkenTree.get(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7))) should contain theSameElementsAs
      List(
        BoxedPayload2D(
          Box2D(Coordinate2D(3, 5), Coordinate2D(4, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ), // left upper
        BoxedPayload2D(
          Box2D(Coordinate2D(4, 5), Coordinate2D(7, 7)),
          "five",
          Some(Box2D(Coordinate2D(3, 5), Coordinate2D(7, 7)))
        ) // right upper
      )

    val emptyTree = BoxQuadtree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
    emptyTree.clear()
    assert(emptyTree.toIterable.isEmpty)
