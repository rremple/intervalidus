package intervalidus.examples.mongodb.upickle

import org.bson.{BsonDocument, BsonValue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class BsonTransformerTest extends AnyFlatSpec with Matchers:

  import BsonTransformer.given

  import ujson.{Arr, Obj, Value}
  import upickle.default.{Reader, Writer, transform}

  extension [T](value: T)(using Writer[T]) def as[S](using Reader[S]): S = transform(value).to[S]

  val arrValue = Arr(1, Arr(2, Arr(3, 4)))
  val objValue = Obj("hello" -> Arr(1, 2, 3), "world" -> false)

  "uPickle transformer" should "be able to transform BSON types" in:
    val arrBson = arrValue.as[BsonValue]
    assert(arrBson.isArray)
    arrBson.as[Value] shouldBe arrValue

    val objBson = objValue.as[BsonValue]
    assert(objBson.isDocument)
    objBson.as[Value] shouldBe objValue

    val objBsonDocument = objValue.as[BsonDocument]
    assert(objBsonDocument.containsKey("hello") && objBsonDocument.containsKey("world"))
    objBsonDocument.get("world").asBoolean().getValue shouldBe false
    objBsonDocument.get("hello").asArray().getValues.asScala.toList.map(_.asNumber().intValue()) shouldBe List(1, 2, 3)
    objBsonDocument.as[Value] shouldBe objValue
