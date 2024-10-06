package intervalidus.collection.mutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxOctreeTest extends AnyFunSuite with Matchers:
  test("Mutable: BoxOctree methods"):
    // Create a octree with a small capacity per node and insert some boxes
    val boundary = Box3D(Coordinate3D(-8, -8, -8), Coordinate3D(8, 8, 8))
    val tree = BoxOctree[String](boundary)
    tree.addOne(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)) -> "one") // right upper front
    tree.addOne(Box3D(Coordinate3D(-1, 1, 1), Coordinate3D(3, 3, 2)) -> "two") // left and right upper front (split)
    tree.addOne(Box3D(Coordinate3D(1, -1, 1), Coordinate3D(3, 1, 2)) -> "three") // right lower and upper front (split)
    tree.addOne(Box3D(Coordinate3D(-1, -1, -1), Coordinate3D(1, 1, 1)) -> "four") // all ocrants (split)
    tree.toIterable.groupMap(_.parentBox)(d => d.copy(parentBox = None)) should contain theSameElementsAs List(
      None -> Vector(
        BoxedPayload3D(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)), "one")
      ),
      Some(Box3D(Coordinate3D(-1, 1, 1), Coordinate3D(3, 3, 2))) -> Vector(
        BoxedPayload3D(Box3D(Coordinate3D(-1, 1, 1), Coordinate3D(0, 3, 2)), "two"),
        BoxedPayload3D(Box3D(Coordinate3D(0, 1, 1), Coordinate3D(3, 3, 2)), "two")
      ),
      Some(Box3D(Coordinate3D(1, -1, 1), Coordinate3D(3, 1, 2))) -> Vector(
        BoxedPayload3D(Box3D(Coordinate3D(1, -1, 1), Coordinate3D(3, 0, 2)), "three"),
        BoxedPayload3D(Box3D(Coordinate3D(1, 0, 1), Coordinate3D(3, 1, 2)), "three")
      ),
      Some(Box3D(Coordinate3D(-1, -1, -1), Coordinate3D(1, 1, 1))) -> Vector(
        BoxedPayload3D(Box3D(Coordinate3D(-1, -1, -1), Coordinate3D(0, 0, 0)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(-1, -1, 0), Coordinate3D(0, 0, 1)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(-1, 0, -1), Coordinate3D(0, 1, 0)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(-1, 0, 0), Coordinate3D(0, 1, 1)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(0, -1, -1), Coordinate3D(1, 0, 0)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(0, -1, 0), Coordinate3D(1, 0, 1)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(0, 0, -1), Coordinate3D(1, 1, 0)), "four"),
        BoxedPayload3D(Box3D(Coordinate3D(0, 0, 0), Coordinate3D(1, 1, 1)), "four")
      )
    )

    val expected = List(
      Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)) -> "one",
      Box3D(Coordinate3D(-1, 1, 1), Coordinate3D(3, 3, 2)) -> "two",
      Box3D(Coordinate3D(1, -1, 1), Coordinate3D(3, 1, 2)) -> "three",
      Box3D(Coordinate3D(-1, -1, -1), Coordinate3D(1, 1, 1)) -> "four"
    )
    BoxedPayload.deduplicate(tree.toIterable) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    tree.get(Box3D(Coordinate3D(4, 4, 1), Coordinate3D(5, 5, 2))) should contain theSameElementsAs
      List(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)) -> "one")
    tree.get(Box3D(Coordinate3D(40, 40, 40), Coordinate3D(50, 50, 50))) shouldBe List.empty

    val treeSplit = BoxOctree.from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
    treeSplit.addOne(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)) -> "five") // splits right upper front
    treeSplit.addOne(Box3D(Coordinate3D(5, 4, 1), Coordinate3D(7, 5, 2)) -> "six")
    treeSplit.get(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload3D(
          Box3D(Coordinate3D(3, 5, 1), Coordinate3D(4, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ), // l u f
        BoxedPayload3D(
          Box3D(Coordinate3D(4, 5, 1), Coordinate3D(7, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ), // r u f
        // because they touch y=5 (potential false positive from query)
        BoxedPayload3D(
          Box3D(Coordinate3D(3, 4, 1), Coordinate3D(4, 5, 2)),
          "one",
          Some(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)))
        ),
        BoxedPayload3D(
          Box3D(Coordinate3D(4, 4, 1), Coordinate3D(5, 5, 2)),
          "one",
          Some(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)))
        ),
        BoxedPayload3D(
          Box3D(Coordinate3D(5, 4, 1), Coordinate3D(7, 5, 2)),
          "six",
          Some(Box3D(Coordinate3D(5, 4, 1), Coordinate3D(7, 5, 2)))
        )
      )

    // because we use y=5.01 below, the ones touching y=5 aren't included
    treeSplit.get(Box3D(Coordinate3D(3, 5.01, 1), Coordinate3D(7, 7, 2))) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload3D(
          Box3D(Coordinate3D(3, 5, 1), Coordinate3D(4, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ), // left upper
        BoxedPayload3D(
          Box3D(Coordinate3D(4, 5, 1), Coordinate3D(7, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ) // right upper
      )

    val shrunkenTree = treeSplit.copy
    shrunkenTree.remove(BoxedPayload3D(Box3D(Coordinate3D(3, 3, 1), Coordinate3D(5, 5, 2)), "one"))
    shrunkenTree.remove(BoxedPayload3D(Box3D(Coordinate3D(5, 4, 1), Coordinate3D(7, 5, 2)), "six"))
    shrunkenTree.get(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2))) should contain theSameElementsAs
      List(
        BoxedPayload3D(
          Box3D(Coordinate3D(3, 5, 1), Coordinate3D(4, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ), // l u f
        BoxedPayload3D(
          Box3D(Coordinate3D(4, 5, 1), Coordinate3D(7, 7, 2)),
          "five",
          Some(Box3D(Coordinate3D(3, 5, 1), Coordinate3D(7, 7, 2)))
        ) // r u f
      )

    val emptyTree = shrunkenTree.copy
    emptyTree.clear()
    assert(emptyTree.toIterable.isEmpty)
