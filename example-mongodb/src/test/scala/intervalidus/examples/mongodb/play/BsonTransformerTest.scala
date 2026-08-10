package intervalidus.examples.mongodb.play

import org.bson.{BsonDocument, BsonValue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class BsonTransformerTest extends AnyFlatSpec with Matchers:

  import BsonTransformer.given
  import play.api.libs.json.*
  import play.api.libs.json.Json.{arr, obj}

  val arrValue = arr(1, arr(2, arr(3, 4)))
  val objValue = obj("hello" -> arr(1, 2, 3), "world" -> false)

  extension (bsonValue: BsonValue) def asJson(using e: Writes[BsonValue]): JsValue = e.writes(bsonValue)

  "Play transformer" should "be able to transform BSON types" in:
    arrValue.validate[BsonValue] match
      case JsError(errors)       => fail(errors.toString)
      case JsSuccess(arrBson, _) =>
        assert(arrBson.isArray)
        arrBson.asJson shouldBe arrValue

    objValue.validate[BsonValue] match
      case JsError(errors)       => fail(errors.toString)
      case JsSuccess(objBson, _) =>
        assert(objBson.isDocument)
        objBson.asJson shouldBe objValue

    objValue.validate[BsonDocument] match
      case JsError(errors)               => fail(errors.toString)
      case JsSuccess(objBsonDocument, _) =>
        assert(objBsonDocument.containsKey("hello") && objBsonDocument.containsKey("world"))
        objBsonDocument.get("world").asBoolean().getValue shouldBe false
        objBsonDocument.get("hello").asArray().getValues.asScala.map(_.asNumber().intValue()) shouldBe List(1, 2, 3)
        objBsonDocument.asJson shouldBe objValue
