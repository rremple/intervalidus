package intervalidus.mutable

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

  // increment current version with each data element
  def timeboundVersionedString(allData: Iterable[ValidString]): DataTimeboundVersioned[String] =
    val data = DataTimeboundVersioned[String]()
    allData.foreach: validData =>
      data.set(validData)(using VersionSelection.Current)
      data.incrementCurrentVersion()
    data

  test("Mutable: Looking up data in intervals"):
    val fixture0: DataTimeboundVersioned[String] = DataTimeboundVersioned()
    assert(fixture0.getAll.isEmpty)

    DataTimeboundVersioned.of("Hello world").get shouldBe "Hello world"

    val allData =
      testData("Testing" -> unbounded, "Hello" -> interval(day(1), day(15)), "World" -> intervalFrom(day(10)))
    val fixture = timeboundVersionedString(allData)
    val zoinks = validString("Zoinks!", interval(day(-30), day(0)))
    fixture.set(zoinks)(using VersionSelection.Unapproved)
    // Visualize(fixture.underlying2D, 5000, "gets")

    fixture.getAt(day(5)) shouldBe Some("Hello")
    fixture.getAt(day(15)) shouldBe Some("World")
    fixture.getAt(day(0)) shouldBe Some("Testing")

    // Visualize(fixture.underlying2D, 15000, "before zoinks is approved")

    fixture.incrementCurrentVersion()
    // fixture.approveAll(unbounded) // approves zoinks
    assert(fixture.approve(zoinks)) // approves zoinks
    assert(!fixture.approve(zoinks)) // already approved

    fixture.remove(interval(day(-5), day(5)))(using VersionSelection.Unapproved)
    // Visualize(fixture.underlying2D, 15000, "after zoinks approved, before remove is approved")

    fixture.getAt(day(0)) shouldBe Some(zoinks.value)
    fixture.getIntersecting(interval(day(5), day(15))) shouldBe testData(
      "Hello" -> interval(day(1), day(9)),
      "World" -> intervalFrom(day(10))
    )

    fixture.incrementCurrentVersion()
    fixture.approveAll(unbounded) // approves the unapproved remove
    // Visualize(fixture.underlying2D, 15000, "after remove is approved")

    fixture.getAt(day(0)) shouldBe None
    fixture.getIntersecting(interval(day(5), day(15))) shouldBe testData(
      "Hello" -> interval(day(6), day(9)),
      "World" -> intervalFrom(day(10))
    )
