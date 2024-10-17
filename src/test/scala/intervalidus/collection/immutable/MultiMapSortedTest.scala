package intervalidus.collection.immutable

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MultiMapSortedTest extends AnyFunSuite with Matchers:
  test("Immutable: MultiMapSorted on String -> Int"):
    val dictFrom = MultiMapSorted.from[String, Int](Seq("me" -> 3, "me" -> 1, "me" -> 2))
    val dict = MultiMapSorted[String, Int]()
      .addAll(Seq("me" -> 3, "me" -> 1, "me" -> 2))
    assertResult(dictFrom.get("me"))(dict.get("me"))
    val dict2 = dict
      .addOne("you" -> 2)
      .addOne("you" -> 1)
    assertResult(Set("me", "you"))(dict2.keySet)
    assertResult(Seq(1, 2, 3))(dict2.get("me"))
    assertResult(Seq(1, 2))(dict2.get("you"))
    assertResult(Seq())(dict2.get("nobody"))
    val dict3 = dict2
      .clone()
      .subtractOne("me" -> 2)
    assertResult(Seq(1, 2, 3))(dict2.get("me"))
    assertResult(Seq(1, 3))(dict3.get("me"))
    val dict4 = dict3.clear
    assertResult(Seq())(dict4.get("me"))
