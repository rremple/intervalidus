package intervalidus.examples.mongodb.play

import org.bson.*
import play.api.libs.json.*

import scala.jdk.CollectionConverters.*

/**
  * This could be way more robust, but good enough for our purposes here...
  *
  * It only transforms BsonArray, BsonBoolean, BsonDocument, BsonNull, BsonNumber (only BsonDouble, BsonInt32,
  * BsonInt64), BsonString, and (from) BsonObjectId. The following are not transformed: BsonBinary, BsonDateTime,
  * BsonDbPointer, BsonJavaScript, BsonJavaScriptWithScope, BsonMaxKey, BsonMinKey, BsonNumber (BsonDecimal128),
  * BsonRegularExpression, BsonSymbol, BsonTimestamp, and BsonUndefined.
  */
object BsonTransformer:

  given Writes[BsonValue] = Writes(bsonTransformer)

  given Reads[BsonValue] = Reads(jsonTransformer)

  given (using bsonValueWrites: Writes[BsonValue]): Writes[BsonDocument] = bsonValueWrites.contramap(identity)

  given (using bsonValueReads: Reads[BsonValue]): Reads[BsonDocument] = Reads: json =>
    bsonValueReads
      .reads(json)
      .flatMap:
        case doc: BsonDocument => JsSuccess(doc)
        case other             => JsError(s"Expected BsonDocument but got ${other.getBsonType}")

  // Encoder

  def bsonTransformer(bsonValue: BsonValue): JsValue = bsonValue match
    case doc: BsonDocument  => JsObject(doc.entrySet().asScala.map(e => e.getKey -> bsonTransformer(e.getValue)).toMap)
    case arr: BsonArray     => JsArray(arr.asScala.map(bsonTransformer))
    case num: BsonInt32     => JsNumber(num.getValue)
    case num: BsonInt64     => JsNumber(num.getValue)
    case num: BsonDouble    => JsNumber(num.getValue)
    case _: BsonNull        => JsNull
    case bool: BsonBoolean  => JsBoolean(bool.getValue)
    case string: BsonString => JsString(string.getValue)
    case id: BsonObjectId   => JsString(id.getValue.toHexString)
    case theUnexpected      =>
      throw IllegalArgumentException(s"BsonTransformer: didn't expect $theUnexpected (${theUnexpected.getBsonType})")

    // Decoder

  def jsonTransformer(json: JsValue): JsResult[BsonValue] = json match
    case JsNull       => JsSuccess(BsonNull.VALUE)
    case JsBoolean(b) => JsSuccess(BsonBoolean.valueOf(b))
    case JsTrue       => JsSuccess(BsonBoolean.TRUE)
    case JsFalse      => JsSuccess(BsonBoolean.FALSE)
    case JsNumber(n)  =>
      if n.isValidInt then JsSuccess(BsonInt32(n.toInt))
      else if n.isValidLong then JsSuccess(BsonInt64(n.toLong))
      else JsSuccess(BsonDouble(n.toDouble))
    case JsString(s)  => JsSuccess(BsonString(s))
    case JsArray(arr) =>
      val result = arr.foldLeft[JsResult[Vector[BsonValue]]](JsSuccess(Vector.empty)): (acc, elem) =>
        for
          vector <- acc
          bsonValue <- jsonTransformer(elem)
        yield vector :+ bsonValue
      result.map(values => BsonArray(values.asJava))
    case JsObject(fields) =>
      val result = fields.foldLeft[JsResult[Vector[BsonElement]]](JsSuccess(Vector.empty)):
        case (acc, (key, value)) =>
          for
            vector <- acc
            bsonValue <- jsonTransformer(value)
          yield vector :+ BsonElement(key, bsonValue)
      result.map(elements => BsonDocument(elements.asJava))
