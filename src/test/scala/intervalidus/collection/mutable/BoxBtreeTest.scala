package intervalidus.collection.mutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxBtreeTest extends AnyFunSuite with Matchers:
  def box(start: Double, end: Double): Box1D =
    Box1D(Coordinate1D(start), Coordinate1D(end))

  test("Mutable: BoxBtree methods"):
    // Create a quadtree with a small capacity per node and insert some boxes
    val boundary = box(-8, 8)
    val tree = BoxBtree[String](boundary)
    tree.addOne(box(3, 5) -> "one") // right
    tree.addOne(box(-1, 3) -> "two") // left and right (box split)
    tree.addOne(box(6, 7) -> "three") // right
    tree.addOne(box(7, 8) -> "four") // right

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

    tree.get(box(4, 5)) should contain theSameElementsAs List(box(3, 5) -> "one")
    tree.get(box(40, 50)) shouldBe List.empty

    val treeSplit = BoxBtree.from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
    treeSplit.addOne(box(5, 7) -> "five") // splits right
    treeSplit.addOne(box(7, 8) -> "six")
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

    val shrunkenTree = treeSplit.copy
    shrunkenTree.remove(BoxedPayload1D(box(-1, 3), "two"))
    shrunkenTree.remove(BoxedPayload1D(box(5, 7), "five"))

    shrunkenTree.get(box(3, 5)) should contain theSameElementsAs
      List(
        BoxedPayload1D(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload1D(box(4, 5), "one", Some(box(3, 5)))
      )

    val emptyTree = BoxBtree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
    emptyTree.clear()

    assert(emptyTree.toIterable.isEmpty)
