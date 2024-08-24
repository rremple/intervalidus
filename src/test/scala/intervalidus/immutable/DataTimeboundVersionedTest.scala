package intervalidus.immutable

import intervalidus.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate
import scala.language.implicitConversions

class DataTimeboundVersionedTest extends AnyFunSuite with Matchers:

  import DataIn1DVersionedBase.VersionSelection
  import DiscreteInterval1D.{interval, intervalFrom, unbounded}

  type ValidString = ValidData1D[String, LocalDate]

  private val dayZero = LocalDate.of(2024, 6, 30)

  private def day(offsetDays: Int) = dayZero.plusDays(offsetDays)

  def validString(s: String, validTime: DiscreteInterval1D[LocalDate]): ValidString = validTime -> s

  private def testData(values: (String, DiscreteInterval1D[LocalDate])*): List[ValidString] =
    values.map(validString).toList

  private def dataVersionedAsTimebound(dataIn1DVersioned: DataIn1DVersioned[String, LocalDate]) =
    DataTimeboundVersioned(
      dataIn1DVersioned.getDataIn2D.getAll,
      dataIn1DVersioned.initialVersion,
      Some(dataIn1DVersioned.getCurrentVersion)
    )

  // increment current version with each data element
  def timeboundVersionedString(allData: Iterable[ValidString]): DataTimeboundVersioned[String] =
    dataVersionedAsTimebound(
      allData.foldLeft(DataIn1DVersioned[String, LocalDate]()): (data, validData) =>
        data
          .set(validData)
          .incrementCurrentVersion()
    )

  test("Immutable: Looking up data in intervals"):
    val fixture0: DataTimeboundVersioned[String] = DataTimeboundVersioned()
    assert(fixture0.getAll.isEmpty)

    DataTimeboundVersioned.of("Hello world").get shouldBe "Hello world"

    val allData =
      testData("Testing" -> unbounded, "Hello" -> interval(day(1), day(15)), "World" -> intervalFrom(day(10)))
    val fixture1 = timeboundVersionedString(allData)
    // Visualize(fixture1.getDataIn2D, 50000, "before Zoinks")
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)))
    val fixture2 = dataVersionedAsTimebound(
      fixture1.set(zoinks)(using VersionSelection.Unapproved)
    )
    // Visualize(fixture2.getDataIn2D, 5000, "after Zoinks")

    fixture2.getAt(day(5)) shouldBe Some("Hello")
    fixture2.getAt(day(15)) shouldBe Some("World")
    fixture2.getAt(day(0)) shouldBe Some("Testing")
    fixture2.getAt(day(0))(using VersionSelection.Unapproved) shouldBe Some("Zoinks!")

    // Visualize(fixture.underlying2D, 15000, "before zoinks is approved")

    val fixture3 = fixture2
      .incrementCurrentVersion()
      .approve(zoinks) // approves zoinks
      .map(dataVersionedAsTimebound) match
      case Some(f) =>
        f.approve(zoinks) shouldBe None // already approved
        f
      case None => fail("unexpected failure to approve")

    val fixture4 = dataVersionedAsTimebound(
      fixture3.remove(interval(day(-5), day(5)))(using VersionSelection.Unapproved)
    )
    // Visualize(fixture4.underlying2D, 15000, "after zoinks approved, before remove is approved")

    fixture4.getAt(day(0)) shouldBe Some(zoinks.value)
    fixture4.getIntersecting(interval(day(5), day(15))) shouldBe testData(
      "Hello" -> interval(day(1), day(9)),
      "World" -> intervalFrom(day(10))
    )

    val fixture5 = dataVersionedAsTimebound(
      fixture4
        .incrementCurrentVersion()
        .approveAll(unbounded) // approves the unapproved remove
    )
    // Visualize(fixture5.underlying2D, 15000, "after remove is approved")

    fixture5.getAt(day(0)) shouldBe None
    fixture5.getIntersecting(interval(day(5), day(15))) shouldBe testData(
      "Hello" -> interval(day(6), day(9)),
      "World" -> intervalFrom(day(10))
    )
