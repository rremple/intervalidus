package intervalidus.collection.mutable

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MultiDictSortedTest extends AnyFunSuite with Matchers:
  test("Mutable: MultiDictSorted on String -> Int"):
    val dictFrom = MultiDictSorted.from[String, Int](Seq("me" -> 3, "me" -> 1, "me" -> 2))
    val dict = MultiDictSorted[String, Int]()
    dict.addAll(Seq("me" -> 3, "me" -> 1, "me" -> 2))
    assertResult(dictFrom.get("me"))(dict.get("me"))
    dict.addOne("you" -> 2)
    dict.addOne("you" -> 1)
    assertResult(Set("me", "you"))(dict.keySet)
    assertResult(Seq(1, 2, 3))(dict.get("me"))
    assertResult(Seq(1, 2))(dict.get("you"))
    assertResult(Seq())(dict.get("nobody"))
    val dict2 = dict.clone()
    dict2.subtractOne("me" -> 2)
    assertResult(Seq(1, 2, 3))(dict.get("me"))
    assertResult(Seq(1, 3))(dict2.get("me"))
    dict2.clear()
    assertResult(Seq())(dict2.get("me"))
