package intervalidus.json

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

case class ProviderName(last: String, first: Option[String] = None)

case class Hours(start: String, end: String)

case class OfficeOpen(day: String, hours: List[Hours])

case class OfficeHours(times: List[OfficeOpen])

/**
  * WeePickle and uPickle test only differ in the monad type names.
  *
  * @tparam W
  *   monad for writing from a type (Writer/From)
  * @tparam R
  *   monad for reading to a type (Reader/To)
  */
trait FilteredFoldingVisitorTestBehavior[W[_], R[_]](using
  W[Hours],
  W[OfficeOpen],
  W[OfficeHours],
  R[Hours],
  R[OfficeOpen],
  R[OfficeHours],
  R[ProviderName],
  R[List[OfficeOpen]],
  R[String]
) extends FilteredFoldingTestData:
  this: AnyFunSuite & Matchers =>

  protected def filteredFoldingVisitor[A: R, B](filterPath: FilterPath, zero: B)(op: (B, A) => B): R[B]

  protected def transform[T, S](t: T)(using W[T], R[S]): S

  protected def testDoc[T](using R[T]): T

  protected def rUnit: R[Unit]

  def foldToGather(filterPath: FilterPath): R[List[OfficeOpen]] =
    filteredFoldingVisitor[OfficeOpen, List[OfficeOpen]](
      filterPath = filterPath,
      zero = List[OfficeOpen]()
    ): (reversedList, data) => // data is an individual element
      data :: reversedList // prepend, and we can reverse at the end

  /*
   * Checks parsing primary, alternate, and other string representations of the path. For example, if the primary
   * representation is "provider/locations/[0]/officeHours/times/[*]" then the alternate representation
   * "provider/locations[0]/officeHours/times[*]" which should also parse to the same filter path. And so should the
   * other representation "provider/locations/0/officeHours/times/[*]"
   */
  def checkPath(path: FilterPath, primaryPathString: String, otherPathStrings: String*): Assertion =
    path.toString shouldBe primaryPathString
    val altPathString = primaryPathString.replace("/[", "[")
    (altPathString +: otherPathStrings).map(FilterPath.parse).foreach(_ shouldBe path)
    FilterPath.parse(primaryPathString) shouldBe path

  val weekdayHours = List(Hours("06:30:00", "15:00:00"))
  val shorterWeekendHours = List(Hours("07:00:00", "11:00:00"))

  // order follows order in the testDoc -- it is weird, but important
  val expectedOfficeHours = List(
    OfficeHours(
      List(
        OfficeOpen("thursday", weekdayHours),
        OfficeOpen("wednesday", weekdayHours),
        OfficeOpen("saturday", shorterWeekendHours),
        OfficeOpen("monday", weekdayHours),
        OfficeOpen("tuesday", weekdayHours),
        OfficeOpen("friday", weekdayHours)
      )
    ),
    OfficeHours(
      List(
        OfficeOpen("thursday-alt", weekdayHours),
        OfficeOpen("wednesday-alt", weekdayHours),
        OfficeOpen("saturday-alt", shorterWeekendHours),
        OfficeOpen("monday-alt", weekdayHours),
        OfficeOpen("tuesday-alt", weekdayHours),
        OfficeOpen("friday-alt", weekdayHours)
      )
    )
  )

  def fromExpectedOfficeHoursHead[V](using R[V]): V = transform(expectedOfficeHours.head)

  def commonBehaviors(prefix: String): Unit =
    test(s"$prefix: the test doc parses without errors"):
      testDoc(using rUnit) shouldBe ()

    test(s"$prefix: return a single element -- grab a structure from JSON"):
      val path = FilterPath("provider")("name")
      checkPath(path, "provider/name")
      val foldToFind = filteredFoldingVisitor[ProviderName, Option[ProviderName]](path, None): (prior, data) =>
        prior shouldBe None
        Some(data)
      testDoc(using foldToFind) shouldBe Some(ProviderName("Laboratory Corp Of America Holdings"))

    test(s"$prefix: return a single element -- grab a string from JSON"):
      val path = FilterPath("provider")("name")("last")
      checkPath(path, "provider/name/last")
      val foldToFind = filteredFoldingVisitor[String, Option[String]](path, None): (prior, data) =>
        prior shouldBe None
        Some(data)
      testDoc(using foldToFind) shouldBe Some("Laboratory Corp Of America Holdings")

    test(s"$prefix: return a single element -- grab a string from Scala"):
      val path = FilterPath("times")(0)("day")
      checkPath(path, "times/[0]/day", "times/0/day")
      val foldToFind = filteredFoldingVisitor[String, Option[String]](path, None): (prior, data) =>
        prior shouldBe None
        Some(data)
      fromExpectedOfficeHoursHead(using foldToFind) shouldBe Some("thursday")

    test(s"$prefix: count locations with office hours"):
      val path = FilterPath("provider").all("locations")("officeHours")
      checkPath(path, "provider/locations/[*]/officeHours", "provider/locations/[:]/officeHours")
      val foldToCount = filteredFoldingVisitor[OfficeHours, Int](path, 0): (i, data) =>
        data shouldBe expectedOfficeHours(i)
        i + 1
      testDoc(using foldToCount) shouldBe 2

    test(s"$prefix: count open days per week in the first location"):
      val path = FilterPath("provider")("locations")(0)("officeHours").all("times")
      checkPath(path, "provider/locations/[0]/officeHours/times/[*]", "provider/locations/0/officeHours/times/[:]")
      val foldToCount = filteredFoldingVisitor[OfficeOpen, Int](path, 0): (i, data) =>
        data shouldBe expectedOfficeHours.head.times(i)
        i + 1
      testDoc(using foldToCount) shouldBe 6

    test(s"$prefix: return office hour open times per location - array level"):
      val path = FilterPath("provider").all("locations")("officeHours")("times")
      checkPath(path, "provider/locations/[*]/officeHours/times", "provider/locations/[:]/officeHours/times")
      val foldToGather = filteredFoldingVisitor[List[OfficeOpen], List[OfficeOpen]](
        path,
        List[OfficeOpen]()
      ): (flattenedList, data) => // data is a list
        flattenedList ++ data
      testDoc(using foldToGather) shouldBe expectedOfficeHours.flatMap(_.times)

    test(
      s"$prefix: gather times, selecting element level, unconstrained"
    ):
      val path = FilterPath("provider").all("locations")("officeHours").all("times")
      checkPath(path, "provider/locations/[*]/officeHours/times/[*]", "provider/locations/[:]/officeHours/times/[:]")
      testDoc(using foldToGather(path)).reverse shouldBe expectedOfficeHours.flatMap(_.times)

    test(s"$prefix: gather times, selecting by constrained lower bound + range"):
      val path = FilterPath("provider").slice("locations", from = 1)("officeHours").slice("times", from = 3, until = 6)
      checkPath(path, "provider/locations/[1:]/officeHours/times/[3:6]")
      testDoc(using foldToGather(path)).reverse shouldBe expectedOfficeHours(1).times.slice(3, 6)

    test(s"$prefix: gather times, selecting by constrained upper bound + index"):
      val path = FilterPath("provider").slice("locations", until = 2)("officeHours")("times")(3)
      checkPath(path, "provider/locations/[0:2]/officeHours/times/[3]", "provider/locations/[0:2]/officeHours/times/3")
      testDoc(using foldToGather(path)).reverse shouldBe expectedOfficeHours.map(_.times(3))

    test(s"$prefix: gather times, selecting by constrained index + upper bound"):
      val path = FilterPath("provider")("locations")(0)("officeHours").slice("times", until = 4)
      checkPath(path, "provider/locations/[0]/officeHours/times/[0:4]", "provider/locations/0/officeHours/times/[0:4]")
      testDoc(using foldToGather(path)).reverse shouldBe expectedOfficeHours.head.times.take(4)
