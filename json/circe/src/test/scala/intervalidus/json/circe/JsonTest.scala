package intervalidus.json.circe

import intervalidus.{DiffAction, Domain1D}
import intervalidus.DiscreteAffineValue.given
import intervalidus.DomainLike.given
import intervalidus.VariableBase.given
import intervalidus.json.JsonTestBehavior
import intervalidus.json.circe.Json.given
import io.circe.*
import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonTest extends AnyFunSuite with Matchers with JsonTestBehavior[Encoder, Decoder]:

  override def jsonTo[T](json: String)(using decoder: Decoder[T]): T = decode[T](json) match
    case Right(value) => value
    case Left(error)  => throw error

  override def toJsonString[T](t: T)(using encoder: Encoder[T]): String = encoder(t).noSpaces

  testsFor(commonBehaviors("Circe"))

  test("Circe: invalid JSON code coverage"):
    decode[DiffAction.In1D[String, Int]]("""{ "action": "UnknownAction" }""") match
      case Left(failure: DecodingFailure) =>
        failure.history shouldBe empty
        failure.message shouldBe "Unknown DiffAction: UnknownAction"
      case _ => fail("Expected a DecodingFailure")

    decode[Domain1D[Int]](quote("TopBottom")) match
      case Left(failure: DecodingFailure) =>
        failure.history shouldBe empty
        failure.message shouldBe "Unknown Domain1D: TopBottom"
      case _ =>
        fail("Expected a DecodingFailure")
