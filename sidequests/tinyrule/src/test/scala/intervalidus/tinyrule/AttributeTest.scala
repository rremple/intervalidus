package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class AttributeTest extends AnyFunSuite with Matchers:

  test("Constructing"):
    val stringAttribute = Attribute("name", "hello world")
    assert(stringAttribute.isInstanceOf[StringAttribute])
    stringAttribute.name shouldBe "name"
    stringAttribute.value shouldBe "hello world"

    assert(Attribute("name", true).isInstanceOf[BooleanAttribute])
    assert(Attribute("name", 1).isInstanceOf[IntAttribute])
    assert(Attribute("name", 1.5).isInstanceOf[DoubleAttribute])
    assert(Attribute("name", LocalDate.now()).isInstanceOf[DateAttribute])

    val stringyAttribute = Attribute("name", BigInt(12345))
    assert(stringyAttribute.isInstanceOf[StringAttribute]) // fallback
    stringyAttribute.value shouldBe "12345"
