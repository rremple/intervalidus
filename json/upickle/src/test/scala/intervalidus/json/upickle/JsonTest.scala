package intervalidus.json.upickle

import upickle.default._
import intervalidus.*
import intervalidus.DiscreteInterval1D.*
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class JsonTest extends AnyFunSuite with Matchers:

  import DiscreteDomain1D.{Bottom, Point, Top}
  import Json.given
  extension (json: String) def as[T: Reader]: T = read(json)
  private def quote(s: String): String = s"\"$s\""
  private def isomorphic[T: Reader: Writer](t: T, json: String): Assertion =
    json.as[T] shouldBe t
    write(t) shouldBe json
  private def isomorphicData[T: Reader: Writer, Data](t: T, json: String, extractData: T => Iterable[Data]): Assertion =
    extractData(json.as[T]) should contain theSameElementsAs extractData(t)
    write(t) shouldBe json

  test("Domains encoded as strings/objects - 1D"):
    isomorphic[DiscreteDomain1D[Int]](Top, quote("Top"))
    isomorphic[DiscreteDomain1D[Int]](Bottom, quote("Bottom"))
    isomorphic[DiscreteDomain1D[Int]](Point(1), """{"point":1}""")

    // must be an object or string
    assertThrows[Exception]("1".as[DiscreteDomain1D[Int]])
    assertThrows[Exception](quote("Unknown string").as[DiscreteDomain1D[Int]])
    assertThrows[Exception]("""{ "point": "type mismatch" }""".as[DiscreteDomain1D[Int]])

  test("Domains encoded as strings/objects - 2D"):
    isomorphic(DiscreteDomain2D[Int, Int](Top, Top), """{"horizontalIndex":"Top","verticalIndex":"Top"}""")
    isomorphic(DiscreteDomain2D[Int, Int](Bottom, 1), """{"horizontalIndex":"Bottom","verticalIndex":{"point":1}}""")

    // must be an object
    assertThrows[Exception](quote("Top").as[DiscreteDomain2D[Int, Int]])

  test("Domains encoded as strings/objects - 3D"):
    isomorphic(
      DiscreteDomain3D[Int, Int, Int](Top, Top, Top),
      """{"horizontalIndex":"Top","verticalIndex":"Top","depthIndex":"Top"}"""
    )
    isomorphic(
      DiscreteDomain3D[Int, Int, Int](Bottom, 1, 2),
      """{"horizontalIndex":"Bottom","verticalIndex":{"point":1},"depthIndex":{"point":2}}"""
    )

    // must be an object
    assertThrows[Exception](quote("Top").as[DiscreteDomain3D[Int, Int, Int]])

  test("Intervals encoded as objects - 1D"):
    isomorphic(intervalFrom(0), """{"start":{"point":0},"end":"Top"}""")
    isomorphic(intervalTo(0), """{"start":"Bottom","end":{"point":0}}""")

  test("Intervals encoded as objects - 2D"):
    isomorphic(
      intervalFrom(0) x intervalTo(0),
      """{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}"""
        + "}"
    )
    isomorphic(
      intervalTo(0) x intervalFrom(0),
      """{"horizontal":{"start":"Bottom","end":{"point":0}}""" +
        ""","vertical":{"start":{"point":0},"end":"Top"}"""
        + "}"
    )

  test("Intervals encoded as objects - 3D"):
    isomorphic(
      intervalFrom(0) x intervalTo(0) x unbounded[Int],
      """{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}""" +
        ""","depth":{"start":"Bottom","end":"Top"}""" +
        "}"
    )
    isomorphic(
      intervalFrom(0) x unbounded[Int] x intervalTo(0),
      """{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":"Top"}""" +
        ""","depth":{"start":"Bottom","end":{"point":0}}""" +
        "}"
    )

  test("Valid data encoded as objects - 1D"):
    isomorphic(
      intervalFrom(0) -> "Hello",
      """{"value":"Hello"""" +
        ""","interval":{"start":{"point":0},"end":"Top"""" +
        "}}"
    )
    isomorphic(
      intervalTo(0) -> "Goodbye",
      """{"value":"Goodbye"""" +
        ""","interval":{"start":"Bottom","end":{"point":0}""" +
        "}}"
    )

  test("Valid data encoded as objects - 2D"):
    isomorphic(
      (intervalFrom(0) x intervalTo(0)) -> "Hello",
      """{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}"""
        + "}}"
    )
    isomorphic(
      (intervalTo(0) x intervalFrom(0)) -> "Goodbye",
      """{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":"Bottom","end":{"point":0}}""" +
        ""","vertical":{"start":{"point":0},"end":"Top"}"""
        + "}}"
    )

  test("Valid data encoded as objects - 3D"):
    isomorphic(
      (intervalFrom(0) x intervalTo(0) x unbounded[Int]) -> "Hello",
      """{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}""" +
        ""","depth":{"start":"Bottom","end":"Top"}""" +
        "}}"
    )
    isomorphic(
      (intervalFrom(0) x unbounded[Int] x intervalTo(0)) -> "Goodbye",
      """{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":"Top"}""" +
        ""","depth":{"start":"Bottom","end":{"point":0}}""" +
        "}}"
    )

  test("Diff actions encoded as objects - 1D"):
    isomorphic(
      DiffAction1D.Create(intervalFrom(0) -> "Hello"),
      """{"action":"Create"""" +
        ""","validData":{"value":"Hello"""" +
        ""","interval":{"start":{"point":0},"end":"Top"""" +
        "}}}"
    )
    isomorphic(
      DiffAction1D.Update(intervalTo(0) -> "Goodbye"),
      """{"action":"Update"""" +
        ""","validData":{"value":"Goodbye"""" +
        ""","interval":{"start":"Bottom","end":{"point":0}""" +
        "}}}"
    )

    isomorphic[DiffAction1D[String, Int]](
      DiffAction1D.Delete(Point(0)),
      """{"action":"Delete","key":{"point":0}}"""
    )

  test("Diff actions encoded as objects - 2D"):
    isomorphic(
      DiffAction2D.Create((intervalFrom(0) x intervalTo(0)) -> "Hello"),
      """{"action":"Create"""" +
        ""","validData":{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}"""
        + "}}}"
    )
    isomorphic(
      DiffAction2D.Update((intervalTo(0) x intervalFrom(0)) -> "Goodbye"),
      """{"action":"Update"""" +
        ""","validData":{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":"Bottom","end":{"point":0}}""" +
        ""","vertical":{"start":{"point":0},"end":"Top"}"""
        + "}}}"
    )
    isomorphic[DiffAction2D[String, Int, Int]](
      DiffAction2D.Delete(Point(0) x Point(1)),
      """{"action":"Delete"""" +
        ""","key":{"horizontalIndex":{"point":0},"verticalIndex":{"point":1}}}"""
    )

  test("Diff actions encoded as objects - 3D"):
    isomorphic(
      DiffAction3D.Create((intervalFrom(0) x intervalTo(0) x unbounded[Int]) -> "Hello"),
      """{"action":"Create"""" +
        ""","validData":{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}""" +
        ""","depth":{"start":"Bottom","end":"Top"}""" +
        "}}}"
    )
    isomorphic(
      DiffAction3D.Update((intervalFrom(0) x unbounded[Int] x intervalTo(0)) -> "Goodbye"),
      """{"action":"Update"""" +
        ""","validData":{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":{"point":0},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":"Top"}""" +
        ""","depth":{"start":"Bottom","end":{"point":0}}""" +
        "}}}"
    )
    isomorphic[DiffAction3D[String, Int, Int, Int]](
      DiffAction3D.Delete(Point(0) x Point(1) x Point(3)),
      """{"action":"Delete"""" +
        ""","key":{"horizontalIndex":{"point":0},"verticalIndex":{"point":1},"depthIndex":{"point":3}}}"""
    )

  test("Dimensional data encoded as arrays - 1D"):
    val data = Seq(
      intervalFrom(1) -> "Hello",
      intervalTo(0) -> "Goodbye"
    )
    val json = """[{"value":"Goodbye"""" +
      ""","interval":{"start":"Bottom","end":{"point":0}""" +
      "}}" +
      """,{"value":"Hello"""" +
      ""","interval":{"start":{"point":1},"end":"Top"""" +
      "}}]"

    isomorphicData(immutable.DataIn1D[String, Int](data), json, _.getAll)
    isomorphicData(mutable.DataIn1D[String, Int](data), json, _.getAll)

  test("Dimensional data encoded as arrays - 2D"):
    val data = Seq(
      (intervalFrom(1) x intervalTo(0)) -> "Hello",
      (intervalTo(0) x intervalFrom(1)) -> "Goodbye"
    )
    val json =
      """[{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":"Bottom","end":{"point":0}}""" +
        ""","vertical":{"start":{"point":1},"end":"Top"}"""
        + "}}" +
        """,{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":1},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}"""
        + "}}]"

    isomorphicData(immutable.DataIn2D[String, Int, Int](data), json, _.getAll)
    isomorphicData(mutable.DataIn2D[String, Int, Int](data), json, _.getAll)

  test("Dimensional data encoded as arrays - 3D"):
    val data = Seq(
      (intervalFrom(1) x intervalTo(0) x unbounded[Int]) -> "Hello",
      (intervalFrom(1) x intervalFrom(1) x intervalTo(0)) -> "Goodbye"
    )
    val json =
      """[{"value":"Hello"""" +
        ""","interval":{"horizontal":{"start":{"point":1},"end":"Top"}""" +
        ""","vertical":{"start":"Bottom","end":{"point":0}}""" +
        ""","depth":{"start":"Bottom","end":"Top"}""" +
        "}}" +
        """,{"value":"Goodbye"""" +
        ""","interval":{"horizontal":{"start":{"point":1},"end":"Top"}""" +
        ""","vertical":{"start":{"point":1},"end":"Top"}""" +
        ""","depth":{"start":"Bottom","end":{"point":0}}""" +
        "}}]"

    isomorphicData(immutable.DataIn3D[String, Int, Int, Int](data), json, _.getAll)
    isomorphicData(mutable.DataIn3D[String, Int, Int, Int](data), json, _.getAll)
