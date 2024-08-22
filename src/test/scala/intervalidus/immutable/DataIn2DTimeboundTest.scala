package intervalidus.immutable

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DataIn2DTimeboundTest extends AnyFunSuite with Matchers:

  test("Immutable: Looking up data in intervals"):
    val fixture0: DataIn2DTimebound[String] = DataIn2DTimebound()
    assert(fixture0.getAll.isEmpty)

    val fixture1: DataIn2DTimebound[String] = DataIn2DTimebound.of("Hello")
    fixture1.get shouldBe "Hello"
