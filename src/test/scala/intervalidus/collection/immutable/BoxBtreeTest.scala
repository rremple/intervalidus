package intervalidus.collection.immutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxBtreeTest extends AnyFunSuite with Matchers:
  def box(start: Double, end: Double): Box1D =
    Box1D(Coordinate1D(start), Coordinate1D(end))

  test("Immutable: BoxBtree methods"):
    // Create a B-tree with a small capacity per node and insert some boxes
    val boundary = box(-8, 8)
    val tree = BoxBtree[String](boundary)
      .addOne(box(3, 5) -> "one") // right
      .addOne(box(-1, 3) -> "two") // left and right (split)
      .addOne(box(6, 7) -> "three") // right
      .addOne(box(7, 8) -> "four") // right

    tree.toIterable.groupMap(_.parentBox)(d => d.copy(parentBox = None)) should contain theSameElementsAs
      List(
        None -> Vector( // no split
          box(7, 8) -> "four", // right
          box(6, 7) -> "three", // right
          box(3, 5) -> "one" // right
        ),
        Some(box(-1, 3)) -> Vector( // split
          box(-1, 0) -> "two", // left bit
          box(0, 3) -> "two" // right bit
        )
      )
    val expected = List(
      box(3, 5) -> "one",
      box(-1, 3) -> "two", // healed
      box(6, 7) -> "three",
      box(7, 8) -> "four"
    )
    BoxedPayload.deduplicate(tree.toIterable) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    // Trees always start as a branch and immutable leaves never get copied, so forcing that to happen here just for
    // coverage -- won't happen in real life!
    val leaf = BoxBtreeLeaf[String](boundary, depth = 0, capacity = 1, depthLimit = 1)
      .addOne(box(3, 5) -> "one")
    val leafCopy = leaf.copy
    leaf.toIterable should contain theSameElementsAs List(box(3, 5) -> "one")
    leafCopy.toIterable should contain theSameElementsAs leaf.toIterable

    tree.get(box(4, 5)) should contain theSameElementsAs List(box(3, 5) -> "one")
    tree.get(box(40, 50)) shouldBe List.empty

    val treeSplit = BoxBtree
      .from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
      .addOne(box(5, 7) -> "five") // splits right
      .addOne(box(7, 8) -> "six")
    treeSplit.get(box(3, 5)) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload1D(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload1D(box(4, 5), "one", Some(box(3, 5))),
        // because they touch 3 or 5 (potential false positive from query)
        BoxedPayload1D(box(0, 3), "two", Some(box(-1, 3))), // only the right bit touching 3
        BoxedPayload1D(box(5, 6), "five", Some(box(5, 7))) // only the left bit touching 5
      )

    // because we use 3.01 and 5.99 below, the ones touching 3 and 5 aren't included
    treeSplit.get(box(3.01, 4.99)) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload1D(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload1D(box(4, 5), "one", Some(box(3, 5)))
      )

    val shrunkenTree = treeSplit
      .remove(BoxedPayload1D(box(-1, 3), "two"))
      .remove(BoxedPayload1D(box(5, 7), "five"))

    shrunkenTree.get(box(3, 5)) should contain theSameElementsAs
      List(
        BoxedPayload1D(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload1D(box(4, 5), "one", Some(box(3, 5)))
      )

    val emptyTree = BoxBtree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
      .clear

    assert(emptyTree.toIterable.isEmpty)