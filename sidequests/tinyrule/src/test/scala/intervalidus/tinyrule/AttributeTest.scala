package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class AttributeTest extends AnyFunSuite with Matchers:
  import Attribute.*

  test("Constructing"):
    val stringAttribute = "name" is "hello world"
    assert(stringAttribute.isInstanceOf[Attribute[String]])
    stringAttribute.name shouldBe "name"
    stringAttribute.value shouldBe "hello world"

    assert(("name" is true).isInstanceOf[Attribute[Boolean]])
    assert(("name" is 1).isInstanceOf[Attribute[Int]])
    assert(("name" is 1.5).isInstanceOf[Attribute[Double]])
    assert(("name" is LocalDate.now()).isInstanceOf[Attribute[LocalDate]])

    // No given instance of type intervalidus.tinyrule.AttributeValueLike[BigInt] was found
    // for a context parameter of method is in object Attribute
    """val _ = "name" is BigInt(12345)""" shouldNot typeCheck
