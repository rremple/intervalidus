package intervalidus.examples.mongodb.weepickle

import org.bson.{BsonDocument, BsonValue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class BsonTransformerTest extends AnyFlatSpec with Matchers:

  import BsonTransformer.given

  import com.rallyhealth.weejson.v1.{Arr, Obj, Value}
  import com.rallyhealth.weepickle.v1.WeePickle.{From, To, fromScala, to}

  extension [T](value: T)(using f: From[T]) def as[S](using To[S]): S = fromScala(value).transform(to[S])

  val arrValue = Arr(1, Arr(2, Arr(3, 4)))
  val objValue = Obj("hello" -> Arr(1, 2, 3), "world" -> false)

  "WeePickle transformer" should "be able to transform BSON types" in:
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
