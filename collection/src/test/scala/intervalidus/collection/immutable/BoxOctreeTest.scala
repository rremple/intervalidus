package intervalidus.collection.immutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxOctreeTest extends AnyFunSuite with Matchers:
  test("Immutable: BoxTree (3D -- Octree) methods"):
    // Create a octree with a small capacity per node and insert some boxes
    val boundary = Box(Coordinate(-8, -8, -8), Coordinate(8, 8, 8))
    val tree = BoxTree[String](boundary)
      .addOne(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)) -> "one") // right upper front
      .addOne(Box(Coordinate(-1, 1, 1), Coordinate(3, 3, 2)) -> "two") // left and right upper front (split)
      .addOne(Box(Coordinate(1, -1, 1), Coordinate(3, 1, 2)) -> "three") // right lower and upper front (split)
      .addOne(Box(Coordinate(-1, -1, -1), Coordinate(1, 1, 1)) -> "four") // all octants (split)
    val result = tree.toIterable.groupMap(_.parentBox)(d => d.copy(parentBox = None))
    result.keys should contain theSameElementsAs List(
      None,
      Some(Box(Coordinate(-1, 1, 1), Coordinate(3, 3, 2))),
      Some(Box(Coordinate(1, -1, 1), Coordinate(3, 1, 2))),
      Some(Box(Coordinate(-1, -1, -1), Coordinate(1, 1, 1)))
    )
    result(None) should contain theSameElementsAs Vector(
      BoxedPayload(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)), "one")
    )
    result(Some(Box(Coordinate(-1, 1, 1), Coordinate(3, 3, 2)))) should contain theSameElementsAs Vector(
      BoxedPayload(Box(Coordinate(-1, 1, 1), Coordinate(0, 3, 2)), "two"),
      BoxedPayload(Box(Coordinate(0, 1, 1), Coordinate(3, 3, 2)), "two")
    )
    result(Some(Box(Coordinate(1, -1, 1), Coordinate(3, 1, 2)))) should contain theSameElementsAs Vector(
      BoxedPayload(Box(Coordinate(1, -1, 1), Coordinate(3, 0, 2)), "three"),
      BoxedPayload(Box(Coordinate(1, 0, 1), Coordinate(3, 1, 2)), "three")
    )
    result(Some(Box(Coordinate(-1, -1, -1), Coordinate(1, 1, 1)))) should contain theSameElementsAs Vector(
      BoxedPayload(Box(Coordinate(-1, -1, -1), Coordinate(0, 0, 0)), "four"),
      BoxedPayload(Box(Coordinate(-1, -1, 0), Coordinate(0, 0, 1)), "four"),
      BoxedPayload(Box(Coordinate(-1, 0, -1), Coordinate(0, 1, 0)), "four"),
      BoxedPayload(Box(Coordinate(-1, 0, 0), Coordinate(0, 1, 1)), "four"),
      BoxedPayload(Box(Coordinate(0, -1, -1), Coordinate(1, 0, 0)), "four"),
      BoxedPayload(Box(Coordinate(0, -1, 0), Coordinate(1, 0, 1)), "four"),
      BoxedPayload(Box(Coordinate(0, 0, -1), Coordinate(1, 1, 0)), "four"),
      BoxedPayload(Box(Coordinate(0, 0, 0), Coordinate(1, 1, 1)), "four")
    )

    val expected = List(
      Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)) -> "one",
      Box(Coordinate(-1, 1, 1), Coordinate(3, 3, 2)) -> "two",
      Box(Coordinate(1, -1, 1), Coordinate(3, 1, 2)) -> "three",
      Box(Coordinate(-1, -1, -1), Coordinate(1, 1, 1)) -> "four"
    )
    BoxedPayload.deduplicate(tree.toIterable) should contain theSameElementsAs expected
    tree.getAndDeduplicate(Box(Coordinate(-1, -1, -1), Coordinate(5, 5, 2))) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    // Trees always start as a branch and immutable leaves never get copied, so forcing that to happen here just for
    // coverage -- won't happen in real life!
    val leaf = BoxTreeLeaf[String](boundary, depth = 0, capacity = 1, depthLimit = 1)
      .addOne(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)) -> "one")
    val leafCopy = leaf.copy
    leaf.toIterable should contain theSameElementsAs List(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)) -> "one")
    leafCopy.toIterable should contain theSameElementsAs leaf.toIterable

    tree.get(Box(Coordinate(4, 4, 1), Coordinate(5, 5, 2))) should contain theSameElementsAs
      List(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)) -> "one")
    tree.get(Box(Coordinate(40, 40, 40), Coordinate(50, 50, 50))) shouldBe List.empty

    val treeSplit = BoxTree
      .from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
      .addOne(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)) -> "five") // splits right upper front
      .addOne(Box(Coordinate(5, 4, 1), Coordinate(7, 5, 2)) -> "six")
    treeSplit.get(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(
          Box(Coordinate(3, 5, 1), Coordinate(4, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ), // l u f
        BoxedPayload(
          Box(Coordinate(4, 5, 1), Coordinate(7, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ), // r u f
        // because they touch y=5 (potential false positive from query)
        BoxedPayload(
          Box(Coordinate(3, 4, 1), Coordinate(4, 5, 2)),
          "one",
          Some(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)))
        ),
        BoxedPayload(
          Box(Coordinate(4, 4, 1), Coordinate(5, 5, 2)),
          "one",
          Some(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)))
        ),
        BoxedPayload(
          Box(Coordinate(5, 4, 1), Coordinate(7, 5, 2)),
          "six",
          Some(Box(Coordinate(5, 4, 1), Coordinate(7, 5, 2)))
        )
      )

    // because we use y=5.01 below, the ones touching y=5 aren't included
    treeSplit.get(Box(Coordinate(3, 5.01, 1), Coordinate(7, 7, 2))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(
          Box(Coordinate(3, 5, 1), Coordinate(4, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ), // left upper
        BoxedPayload(
          Box(Coordinate(4, 5, 1), Coordinate(7, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ) // right upper
      )

    val shrunkenTree = treeSplit
      .remove(BoxedPayload(Box(Coordinate(3, 3, 1), Coordinate(5, 5, 2)), "one"))
      .remove(BoxedPayload(Box(Coordinate(5, 4, 1), Coordinate(7, 5, 2)), "six"))
    shrunkenTree.get(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2))) should contain theSameElementsAs
      List(
        BoxedPayload(
          Box(Coordinate(3, 5, 1), Coordinate(4, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ), // l u f
        BoxedPayload(
          Box(Coordinate(4, 5, 1), Coordinate(7, 7, 2)),
          "five",
          Some(Box(Coordinate(3, 5, 1), Coordinate(7, 7, 2)))
        ) // r u f
      )

    val emptyTree = BoxTree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
      .clear
    assert(emptyTree.toIterable.isEmpty)
