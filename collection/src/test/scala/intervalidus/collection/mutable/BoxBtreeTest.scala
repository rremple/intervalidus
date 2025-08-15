package intervalidus.collection.mutable

import intervalidus.collection.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxBtreeTest extends AnyFunSuite with Matchers:
  def capacity(start: Double, end: Double): Capacity =
    Capacity(CoordinateFixed(start), CoordinateFixed(end))
  def box(start: Double, end: Double): Box =
    Box(Coordinate(Some(start)), Coordinate(Some(end)))

  test("Mutable: BoxTree (1D -- B-tree) methods"):
    // Create a B-tree with a small capacity per node and insert some boxes
    val boundary = Boundary(capacity(-8, 8))
    val tree = BoxTree[String](boundary)
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
    tree.getAndDeduplicate(box(-1, 8)) should contain theSameElementsAs expected
    BoxedPayload.deduplicate(tree.copy.toIterable) should contain theSameElementsAs expected

    tree.get(box(4, 5)) should contain theSameElementsAs List(box(3, 5) -> "one")
    tree.get(box(40, 50)) shouldBe List.empty

    val treeSplit = BoxTree.from(boundary, BoxedPayload.deduplicate(tree.toIterable), 4)
    treeSplit.addOne(box(5, 7) -> "five") // splits right
    treeSplit.addOne(box(7, 8) -> "six")
    treeSplit.get(box(3, 5)) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload(box(4, 5), "one", Some(box(3, 5))),
        // because they touch 3 or 5 (potential false positive from query)
        BoxedPayload(box(0, 3), "two", Some(box(-1, 3))), // only the right bit touching 3
        BoxedPayload(box(5, 6), "five", Some(box(5, 7))) // only the left bit touching 5
      )

    // because we use 3.01 and 5.99 below, the ones touching 3 and 5 aren't included
    treeSplit.get(box(3.01, 4.99)) should contain theSameElementsAs
      List(
        // the one we are actually interested in
        BoxedPayload(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload(box(4, 5), "one", Some(box(3, 5)))
      )

    val shrunkenTree = treeSplit.copy
    shrunkenTree.remove(BoxedPayload(box(-1, 3), "two"))
    shrunkenTree.remove(BoxedPayload(box(5, 7), "five"))

    shrunkenTree.get(box(3, 5)) should contain theSameElementsAs
      List(
        BoxedPayload(box(3, 4), "one", Some(box(3, 5))),
        BoxedPayload(box(4, 5), "one", Some(box(3, 5)))
      )

    val emptyTree = BoxTree
      .from(boundary, BoxedPayload.deduplicate(shrunkenTree.toIterable))
    emptyTree.clear()

    assert(emptyTree.toIterable.isEmpty)

    val boundary44 = Boundary(capacity(-4, 4)).withBox(box(-8, 8))
    val boundaryTree = BoxTree[String](boundary44)
    boundaryTree.get(box(10, 10)) shouldBe List.empty // outside the boundary box

    assertResult(boundary44)(boundaryTree.boundary) // start

    boundaryTree.addOne(box(3, 6) -> "one")
    //  new unscaled capacity = capacity(-4, 6), midPoint = 1
    //  scaling by 2x distance from 1, so 5*2=10, yielding capacity(-9, 11)
    assertResult(Boundary(capacity(-9, 11)).withBox(box(-8, 8)))(boundaryTree.boundary)

    boundaryTree.addOne(box(-9, 3) -> "two")
    //  new scaled box = box(-9, 8), capacity unaffected
    assertResult(Boundary(capacity(-9, 11)).withBox(box(-9, 8)))(boundaryTree.boundary)

    boundaryTree.addOne(box(6, 13) -> "three")
    //  new unscaled capacity = capacity(-9, 13), midPoint = 2
    //  scaling by 2x distance from 2, so 11*2=22, yielding capacity(-20, 24)
    assertResult(Boundary(capacity(-20, 24)).withBox(box(-9, 13)))(boundaryTree.boundary)

    boundaryTree.addOne(Box(Coordinate(None), Coordinate(Some(-9.0))) -> "four")
    //  new scaled box = box(<unbounded>, 8), capacity (weirdly) unaffected
    assertResult(
      Boundary(capacity(-20, 24)).withBox(Box(Coordinate(None), Coordinate(Some(13.0))))
    )(boundaryTree.boundary)
