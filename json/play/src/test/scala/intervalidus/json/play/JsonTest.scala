package intervalidus.json.play

import intervalidus.{DiffAction, Domain1D}
import intervalidus.DiscreteAffineValue.given
import intervalidus.DomainLike.given
import intervalidus.VariableBase.given
import intervalidus.json.JsonTestBehavior
import intervalidus.json.play.Json.given
import play.api.libs.json.*
import play.api.libs.json.Json.{parse, stringify, toJson}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonTest extends AnyFunSuite with Matchers with JsonTestBehavior[Writes, Reads]:

  override def jsonTo[T](jsonString: String)(using reads: Reads[T]): T = parse(jsonString).as[T]

  override def toJsonString[T](t: T)(using writes: Writes[T]): String = stringify(toJson(t))

  testsFor(commonBehaviors("Play"))

  test("Play: invalid JSON code coverage"):
    def messages(errors: Iterable[(JsPath, Iterable[JsonValidationError])]): String =
      errors.flatMap(_._2).map(_.message).mkString("|")

    parse("""{ "action": "UnknownAction" }""").validate[DiffAction.In1D[String, Int]] match
      case JsError(errors) => messages(errors) shouldBe "Unknown DiffAction: UnknownAction"
      case _               => fail("Expected a JsError")

    parse(quote("TopBottom")).validate[Domain1D[Int]] match
      case JsError(errors) => messages(errors) shouldBe "Unknown Domain1D: TopBottom"
      case _               => fail("Expected a JsError")
