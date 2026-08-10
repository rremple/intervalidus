package intervalidus.examples.mongodb.circe

import org.bson.{BsonDocument, BsonValue}
import io.circe.*
import io.circe.Json.{arr, obj}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

class BsonTransformerTest extends AnyFlatSpec with Matchers:

  import BsonTransformer.given

  given [T: Encoder]: Conversion[T, Json] = Encoder[T].apply(_)

  val arrValue = arr(1, arr(2, arr(3, 4)))
  val objValue = obj("hello" -> arr(1, 2, 3), "world" -> false)

  extension (bsonValue: BsonValue) def asJson(using e: Encoder[BsonValue]): Json = e(bsonValue)

  "Circe transformer" should "be able to transform BSON types" in:
    arrValue.as[BsonValue] match
      case Left(error)    => fail(error)
      case Right(arrBson) =>
        assert(arrBson.isArray)
        arrBson.asJson shouldBe arrValue

    objValue.as[BsonValue] match
      case Left(error)    => fail(error)
      case Right(objBson) =>
        assert(objBson.isDocument)
        objBson.asJson shouldBe objValue

    objValue.as[BsonDocument] match
      case Left(error)            => fail(error)
      case Right(objBsonDocument) =>
        assert(objBsonDocument.containsKey("hello") && objBsonDocument.containsKey("world"))
        objBsonDocument.get("world").asBoolean().getValue shouldBe false
        objBsonDocument.get("hello").asArray().getValues.asScala.map(_.asNumber().intValue()) shouldBe List(1, 2, 3)
        objBsonDocument.asJson shouldBe objValue
