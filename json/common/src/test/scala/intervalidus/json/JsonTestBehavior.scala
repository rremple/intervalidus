package intervalidus.json

import intervalidus.*
import intervalidus.DiscreteValue.given
import intervalidus.Domain1D.{Bottom, OpenPoint, Point, Top}
import intervalidus.DomainLike.given
import intervalidus.Interval1D.{interval, intervalFrom, intervalFromAfter, intervalTo, intervalToBefore, unbounded}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.language.implicitConversions

/**
  * WeePickle and uPickle test only differ in the monad type names.
  *
  * @tparam W
  *   monad for writing from a type (Writer/From)
  * @tparam R
  *   monad for reading to a type (Reader/To)
  */
trait JsonTestBehavior[W[_], R[_]](using
  W[Domain1D[Int]],
  W[Domain.In1D[Int]],
  W[Domain.In2D[Int, Int]],
  W[Domain.In3D[Int, Int, Int]],
  W[Interval.In1D[Int]],
  W[Interval.In2D[Int, Int]],
  W[Interval.In3D[Int, Int, Int]],
  W[ValidData.In1D[String, Int]],
  W[ValidData.In2D[String, Int, Int]],
  W[ValidData.In3D[String, Int, Int, Int]],
  W[DiffAction.In1D[String, Int]],
  W[DiffAction.In2D[String, Int, Int]],
  W[DiffAction.In3D[String, Int, Int, Int]],
  W[mutable.Variable[String]],
  W[immutable.Variable[String]],
  W[mutable.Data.In1D[String, Int]],
  W[mutable.Data.In2D[String, Int, Int]],
  W[mutable.Data.In3D[String, Int, Int, Int]],
  W[immutable.Data.In1D[String, Int]],
  W[immutable.Data.In2D[String, Int, Int]],
  W[immutable.Data.In3D[String, Int, Int, Int]],
  W[mutable.DataVersioned.In1D[String, Int]],
  W[immutable.DataVersioned.In1D[String, Int]],
  W[mutable.DataMulti.In1D[String, Int]],
  W[immutable.DataMulti.In1D[String, Int]],
  R[Domain1D[Int]],
  R[Domain.In1D[Int]],
  R[Domain.In2D[Int, Int]],
  R[Domain.In3D[Int, Int, Int]],
  R[Interval.In1D[Int]],
  R[Interval.In2D[Int, Int]],
  R[Interval.In3D[Int, Int, Int]],
  R[ValidData.In1D[String, Int]],
  R[ValidData.In2D[String, Int, Int]],
  R[ValidData.In3D[String, Int, Int, Int]],
  R[DiffAction.In1D[String, Int]],
  R[DiffAction.In2D[String, Int, Int]],
  R[DiffAction.In3D[String, Int, Int, Int]],
  R[mutable.Variable[String]],
  R[immutable.Variable[String]],
  R[mutable.Data.In1D[String, Int]],
  R[mutable.Data.In2D[String, Int, Int]],
  R[mutable.Data.In3D[String, Int, Int, Int]],
  R[immutable.Data.In1D[String, Int]],
  R[immutable.Data.In2D[String, Int, Int]],
  R[immutable.Data.In3D[String, Int, Int, Int]],
  R[mutable.DataVersioned.In1D[String, Int]],
  R[immutable.DataVersioned.In1D[String, Int]],
  R[mutable.DataMulti.In1D[String, Int]],
  R[immutable.DataMulti.In1D[String, Int]]
):
  this: AnyFunSuite & Matchers =>

  protected def jsonTo[T](json: String)(using R[T]): T
  protected def toJsonString[T](t: T)(using W[T]): String

  extension (json: String) def as[T: R]: T = jsonTo(json)
  extension [T: W](t: T) def asJson: String = toJsonString(t)

  private def quote(s: String): String = s"\"$s\""
  private def isomorphic[T: R: W](t: T, json: String): Assertion =
    json.as[T] shouldBe t
    t.asJson shouldBe json
  private def isomorphicData[T: R: W, Data](t: T, json: String, extractData: T => Iterable[Data]): Assertion =
    extractData(json.as[T]) should contain theSameElementsAs extractData(t)
    t.asJson shouldBe json

  def commonBehaviors(prefix: String): Unit =
    test(s"$prefix: Domains encoded as strings/objects - 1D"):
      isomorphic[Domain1D[Int]](Top, quote("Top"))
      isomorphic[Domain1D[Int]](Bottom, quote("Bottom"))
      isomorphic[Domain1D[Int]](Point(1), """{"point":1}""")
      isomorphic[Domain1D[Int]](OpenPoint(1), """{"open":1}""")

      // must be an object or string
      assertThrows[Exception]("1".as[Domain1D[Int]])
      assertThrows[Exception](quote("Unknown string").as[Domain1D[Int]])
      assertThrows[Exception]("""{ "point": "type mismatch" }""".as[Domain1D[Int]])
      assertThrows[Exception]("""{ "notpoint": 1 }""".as[Domain1D[Int]])

    test(s"$prefix: Domains encoded as strings/objects - 2D"):
      isomorphic(Domain.in2D[Int, Int](Top, Top), """["Top","Top"]""")
      isomorphic(Domain.in2D[Int, Int](Bottom, 1), """["Bottom",{"point":1}]""")

      // must be an array
      assertThrows[Exception](quote("Top").as[Domain.In2D[Int, Int]])

    test(s"$prefix: Domains encoded as strings/objects - 3D"):
      isomorphic(
        Domain.in3D[Int, Int, Int](Top, Top, Top),
        """["Top","Top","Top"]"""
      )
      isomorphic(
        Domain.in3D[Int, Int, Int](Bottom, 1, 2),
        """["Bottom",{"point":1},{"point":2}]"""
      )

      // must be an object
      assertThrows[Exception](quote("Top").as[Domain.In3D[Int, Int, Int]])

    test(s"$prefix: Intervals encoded as objects - 1D"):
      isomorphic(intervalFrom(0): Interval.In1D[Int], """{"start":[{"point":0}],"end":["Top"]}""")
      isomorphic(intervalTo(0): Interval.In1D[Int], """{"start":["Bottom"],"end":[{"point":0}]}""")

    test(s"$prefix: Intervals encoded as objects - 2D"):
      isomorphic(
        intervalFrom(0) x intervalTo(0),
        """{"start":[{"point":0},"Bottom"],"end":["Top",{"point":0}]}"""
      )
      isomorphic(
        intervalTo(0) x intervalFrom(0),
        """{"start":["Bottom",{"point":0}],"end":[{"point":0},"Top"]}"""
      )

    test(s"$prefix: Intervals encoded as objects - 3D"):
      isomorphic(
        intervalFrom(0) x intervalTo(0) x unbounded[Int],
        """{"start":[{"point":0},"Bottom","Bottom"],"end":["Top",{"point":0},"Top"]}"""
      )
      isomorphic(
        intervalFrom(0) x unbounded[Int] x intervalTo(0),
        """{"start":[{"point":0},"Bottom","Bottom"],"end":["Top","Top",{"point":0}]}"""
      )

    test(s"$prefix: Valid data encoded as objects - 1D"):
      isomorphic(
        intervalFrom(0) -> "Hello",
        """{"value":"Hello","interval":{"start":[{"point":0}],"end":["Top"]}}"""
      )
      isomorphic(
        intervalTo(0) -> "Goodbye",
        """{"value":"Goodbye","interval":{"start":["Bottom"],"end":[{"point":0}]}}"""
      )

    test(s"$prefix: Valid data encoded as objects - 2D"):
      isomorphic(
        (intervalFrom(0) x intervalTo(0)) -> "Hello",
        """{"value":"Hello"""" +
          ""","interval":{"start":[{"point":0},"Bottom"],"end":["Top",{"point":0}]}"""
          + "}"
      )
      isomorphic(
        (intervalTo(0) x intervalFrom(0)) -> "Goodbye",
        """{"value":"Goodbye"""" +
          ""","interval":{"start":["Bottom",{"point":0}],"end":[{"point":0},"Top"]}"""
          + "}"
      )

    test(s"$prefix: Valid data encoded as objects - 3D"):
      isomorphic(
        (intervalFrom(0) x intervalTo(0) x unbounded[Int]) -> "Hello",
        """{"value":"Hello"""" +
          ""","interval":{"start":[{"point":0},"Bottom","Bottom"],"end":["Top",{"point":0},"Top"]}""" +
          "}"
      )
      isomorphic(
        (intervalFrom(0) x unbounded[Int] x intervalTo(0)) -> "Goodbye",
        """{"value":"Goodbye"""" +
          ""","interval":{"start":[{"point":0},"Bottom","Bottom"],"end":["Top","Top",{"point":0}]}""" +
          "}"
      )

    test(s"$prefix: Diff actions encoded as objects - 1D"):
      isomorphic[DiffAction.In1D[String, Int]](
        DiffAction.Create(intervalFrom(0) -> "Hello"),
        """{"action":"Create"""" +
          ""","validData":{"value":"Hello"""" +
          ""","interval":{"start":[{"point":0}],"end":["Top"]}""" +
          "}}"
      )
      isomorphic[DiffAction.In1D[String, Int]](
        DiffAction.Update(intervalTo(0) -> "Goodbye"),
        """{"action":"Update"""" +
          ""","validData":{"value":"Goodbye"""" +
          ""","interval":{"start":["Bottom"],"end":[{"point":0}]}""" +
          "}}"
      )

      isomorphic[DiffAction.In1D[String, Int]](
        DiffAction.Delete(Point(0)),
        """{"action":"Delete","key":[{"point":0}]}"""
      )

    test(s"$prefix: Diff actions encoded as objects - 2D"):
      isomorphic[DiffAction.In2D[String, Int, Int]](
        DiffAction.Create((intervalFrom(0) x intervalTo(0)) -> "Hello"),
        """{"action":"Create"""" +
          ""","validData":{"value":"Hello"""" +
          ""","interval":{"start":[{"point":0},"Bottom"],"end":["Top",{"point":0}]}""" +
          "}}"
      )
      isomorphic[DiffAction.In2D[String, Int, Int]](
        DiffAction.Update((intervalTo(0) x intervalFrom(0)) -> "Goodbye"),
        """{"action":"Update"""" +
          ""","validData":{"value":"Goodbye"""" +
          ""","interval":{"start":["Bottom",{"point":0}],"end":[{"point":0},"Top"]}""" +
          "}}"
      )
      isomorphic[DiffAction.In2D[String, Int, Int]](
        DiffAction.Delete(Point(0) x Point(1)),
        """{"action":"Delete","key":[{"point":0},{"point":1}]}"""
      )

    test(s"$prefix: Diff actions encoded as objects - 3D"):
      isomorphic[DiffAction.In3D[String, Int, Int, Int]](
        DiffAction.Create((intervalFrom(0) x intervalTo(0) x unbounded[Int]) -> "Hello"),
        """{"action":"Create"""" +
          ""","validData":{"value":"Hello"""" +
          ""","interval":{"start":[{"point":0},"Bottom","Bottom"],"end":["Top",{"point":0},"Top"]}""" +
          "}}"
      )
      isomorphic[DiffAction.In3D[String, Int, Int, Int]](
        DiffAction.Update((intervalFrom(0) x unbounded[Int] x intervalTo(0)) -> "Goodbye"),
        """{"action":"Update"""" +
          ""","validData":{"value":"Goodbye"""" +
          ""","interval":{"start":[{"point":0},"Bottom","Bottom"],"end":["Top","Top",{"point":0}]}""" +
          "}}"
      )
      isomorphic[DiffAction.In3D[String, Int, Int, Int]](
        DiffAction.Delete(Point(0) x Point(1) x Point(3)),
        """{"action":"Delete"""" +
          ""","key":[{"point":0},{"point":1},{"point":3}]}"""
      )

    test(s"$prefix: Variables encoded as arrays"):
      import VariableBase.given
      val now: Domain1D[Instant] = LocalDateTime.of(2025, 11, 19, 12, 15).toInstant(ZoneOffset.UTC)
      val data = Seq(
        intervalTo(now) -> "Hello",
        intervalFromAfter(now) -> "Goodbye"
      )
      val json =
        """[""" +
          """{"value":"Hello","interval":{"start":["Bottom"],"end":[{"point":"2025-11-19T12:15:00Z"}]}}""" +
          """,{"value":"Goodbye","interval":{"start":[{"point":"2025-11-19T12:15:00.000000001Z"}],"end":["Top"]}}""" +
          "]"
      isomorphicData[immutable.Variable[String], ValidData[String, VariableBase.Instant1D]](
        immutable.Variable.fromHistory(data),
        json,
        _.history.getAll
      )
      isomorphicData[mutable.Variable[String], ValidData[String, VariableBase.Instant1D]](
        mutable.Variable.fromHistory(data),
        json,
        _.history.getAll
      )

    test(s"$prefix: Dimensional data encoded as arrays - 1D"):
      val data = Seq(
        intervalFrom(1) -> "Hello",
        intervalTo(0) -> "Goodbye"
      )
      val json = """[{"value":"Goodbye"""" +
        ""","interval":{"start":["Bottom"],"end":[{"point":0}]""" +
        "}}" +
        """,{"value":"Hello"""" +
        ""","interval":{"start":[{"point":1}],"end":["Top"]""" +
        "}}]"

      isomorphicData[immutable.Data.In1D[String, Int], ValidData.In1D[String, Int]](
        immutable.Data(data),
        json,
        _.getAll
      )
      isomorphicData[mutable.Data.In1D[String, Int], ValidData.In1D[String, Int]](
        mutable.Data(data),
        json,
        _.getAll
      )

    test(s"$prefix: Dimensional data encoded as arrays - 2D"):
      val data = Seq(
        (intervalFrom(1) x intervalTo(0)) -> "Hello",
        (intervalTo(0) x intervalFrom(1)) -> "Goodbye"
      )
      val json =
        """[{"value":"Goodbye"""" +
          ""","interval":{"start":["Bottom",{"point":1}],"end":[{"point":0},"Top"]}""" +
          "}" +
          """,{"value":"Hello"""" +
          ""","interval":{"start":[{"point":1},"Bottom"],"end":["Top",{"point":0}]}""" +
          "}]"

      isomorphicData[immutable.Data.In2D[String, Int, Int], ValidData.In2D[String, Int, Int]](
        immutable.Data(data),
        json,
        _.getAll
      )
      isomorphicData[mutable.Data.In2D[String, Int, Int], ValidData.In2D[String, Int, Int]](
        mutable.Data(data),
        json,
        _.getAll
      )

    test(s"$prefix: Dimensional data encoded as arrays - 3D"):
      val data = Seq(
        (intervalFrom(1) x intervalTo(0) x unbounded[Int]) -> "Hello",
        (intervalFrom(1) x intervalFrom(1) x intervalTo(0)) -> "Goodbye"
      )
      val json =
        """[{"value":"Hello"""" +
          ""","interval":{"start":[{"point":1},"Bottom","Bottom"],"end":["Top",{"point":0},"Top"]}""" +
          "}" +
          """,{"value":"Goodbye"""" +
          ""","interval":{"start":[{"point":1},{"point":1},"Bottom"],"end":["Top","Top",{"point":0}]}""" +
          "}]"

      isomorphicData[immutable.Data.In3D[String, Int, Int, Int], ValidData.In3D[String, Int, Int, Int]](
        immutable.Data(data),
        json,
        _.getAll
      )
      isomorphicData[mutable.Data.In3D[String, Int, Int, Int], ValidData.In3D[String, Int, Int, Int]](
        mutable.Data(data),
        json,
        _.getAll
      )

    test(s"$prefix: Dimensional versioned data encoded as objects - 1D (underlying 2D)"):
      val initTime = LocalDateTime.of(2025, 11, 19, 12, 15)
      val incrementTime = initTime.plusMinutes(1)
      given CurrentDateTime = CurrentDateTime.simulated(initTime)

      val immutableDataVersioned = immutable.DataVersioned
        .of(intervalFrom(0) -> "Hello", 0, "custom init")
        .incrementCurrentVersion("to one")(using CurrentDateTime.simulated(incrementTime))
        .set(intervalToBefore(0) -> "Goodbye")

      val mutableDataVersioned = mutable.DataVersioned
        .of(intervalFrom(0) -> "Hello", 0, "custom init")
      mutableDataVersioned.incrementCurrentVersion("to one")(using CurrentDateTime.simulated(incrementTime))
      mutableDataVersioned.set(intervalToBefore(0) -> "Goodbye")

      val json =
        """{"initialVersion":0,"currentVersion":1""" +
          ""","versionTimestamps":[[0,["2025-11-19T12:15","custom init"]],[1,["2025-11-19T12:16","to one"]]]""" +
          ""","data":[""" +
          """{"value":"Hello","interval":{"start":[{"point":0},{"point":0}],"end":["Top","Top"]}},""" +
          """{"value":"Goodbye","interval":{"start":[{"point":1},"Bottom"],"end":["Top",{"point":-1}]}}""" +
          """]}"""

      type VersionedInt = DimensionalVersionedBase.Versioned[Domain.In1D[Int]]
      isomorphicData[immutable.DataVersioned.In1D[String, Int], ValidData[String, VersionedInt]](
        immutableDataVersioned,
        json,
        _.getVersionedData.getAll
      )
      isomorphicData[mutable.DataVersioned.In1D[String, Int], ValidData[String, VersionedInt]](
        mutableDataVersioned,
        json,
        _.getVersionedData.getAll
      )

    test(s"$prefix: Dimensional multi data encoded as arrays - 1D (underlying sets of values)"):
      val data = List(
        interval(0, 20) -> "A",
        interval(5, 25) -> "B",
        interval(10, 30) -> "C"
      )

      // Resulting sets of values:
      //  | 0 .. 4   | 5 .. 9   | 10 .. 20 | 21 .. 25 | 26 .. 30 |
      //  | {A}      |
      //             | {A,B}    |
      //                        | {A,B,C}  |
      //                                   | {B,C}    |
      //                                              | {C}      |

      val json =
        """[""" +
          """{"value":["A"],"interval":{"start":[{"point":0}],"end":[{"point":4}]}},""" +
          """{"value":["A","B"],"interval":{"start":[{"point":5}],"end":[{"point":9}]}},""" +
          """{"value":["A","B","C"],"interval":{"start":[{"point":10}],"end":[{"point":20}]}},""" +
          """{"value":["B","C"],"interval":{"start":[{"point":21}],"end":[{"point":25}]}},""" +
          """{"value":["C"],"interval":{"start":[{"point":26}],"end":[{"point":30}]}}""" +
          """]"""

      isomorphicData[immutable.DataMulti.In1D[String, Int], ValidData.In1D[Set[String], Int]](
        immutable.DataMulti.from(data),
        json,
        _.getAll
      )
      isomorphicData[mutable.DataMulti.In1D[String, Int], ValidData.In1D[Set[String], Int]](
        mutable.DataMulti.from(data),
        json,
        _.getAll
      )
