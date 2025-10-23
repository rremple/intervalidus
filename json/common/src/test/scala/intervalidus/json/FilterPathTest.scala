package intervalidus.json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FilterPathTest extends AnyFunSuite with Matchers:

  import PathComponent.{ArrayPathComponent, ObjectPathComponent}

  test("Filter path constructors"):
    FilterPath("key").path shouldBe List(ObjectPathComponent("key"))
    FilterPath(1).path shouldBe List(ArrayPathComponent(Some(1), Some(2)))
    FilterPath.all("key").path shouldBe List(ArrayPathComponent(None, None), ObjectPathComponent("key"))
    FilterPath.slice("key").path shouldBe List(ArrayPathComponent(None, None), ObjectPathComponent("key"))
    FilterPath.slice("key", 1, 4).path shouldBe List(ArrayPathComponent(Some(1), Some(4)), ObjectPathComponent("key"))
    FilterPath.parse("[:]").toString shouldBe "[*]"
    FilterPath("key").slice("key2").path shouldBe List(
      ArrayPathComponent(None, None),
      ObjectPathComponent("key2"),
      ObjectPathComponent("key")
    )
    FilterPath.parse("path/to/1/x[*]/y/[:4]/z/[1:]").toString shouldBe "path/to/[1]/x/[*]/y/[0:4]/z/[1:]"

  test("Filter path matching"):
    FilterPath
      .parse("path/to")
      .comparePathTo(FilterPath.parse("path/to/data"))(
        whenPrefix = succeed,
        whenMatches = fail("expected prefix, got match"),
        otherwise = fail("expected prefix, got otherwise")
      )

    FilterPath
      .parse("path/to/data")
      .comparePathTo(FilterPath.parse("path/to/data"))(
        whenPrefix = fail("expected match, got prefix"),
        whenMatches = succeed,
        otherwise = fail("expected match, got otherwise")
      )

    FilterPath
      .parse("path/to/something/else")
      .comparePathTo(FilterPath.parse("path/to/shorter"))(
        whenPrefix = fail("expected otherwise, got prefix"),
        whenMatches = fail("expected otherwise, got match"),
        otherwise = succeed
      )

    FilterPath
      .parse("path/to/shorter")
      .comparePathTo(FilterPath.parse("path/to/something/else"))(
        whenPrefix = fail("expected otherwise, got prefix"),
        whenMatches = fail("expected otherwise, got match"),
        otherwise = succeed
      )
