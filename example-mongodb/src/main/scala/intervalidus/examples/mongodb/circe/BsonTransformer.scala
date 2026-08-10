package intervalidus.examples.mongodb.circe

import io.circe.*

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

  import org.bson.*

  given Encoder[BsonValue] = Encoder.instance(bsonTransformer)

  given Decoder[BsonValue] = Decoder.decodeJson.map(jsonTransformer)

  given (using bsonValueEncoder: Encoder[BsonValue]): Encoder[BsonDocument] = bsonValueEncoder.contramap(identity)

  given (using bsonValueDecoder: Decoder[BsonValue]): Decoder[BsonDocument] = Decoder.instance: cursor =>
    bsonValueDecoder(cursor).flatMap:
      case doc: BsonDocument => Right(doc)
      case other => Left(DecodingFailure(s"Expected BsonDocument but got ${other.getBsonType}", cursor.history))

  // Encoder

  def bsonTransformer(bsonValue: BsonValue): Json = bsonValue match
    case doc: BsonDocument => Json.obj(doc.entrySet().asScala.toSeq.map(e => e.getKey -> bsonTransformer(e.getValue))*)
    case arr: BsonArray    => Json.arr(arr.asScala.toSeq.map(bsonTransformer)*)
    case num: BsonInt32    => Json.fromInt(num.getValue)
    case num: BsonInt64    => Json.fromLong(num.getValue)
    case num: BsonDouble   =>
      Json
        .fromDouble(num.getValue)
        .getOrElse:
          throw IllegalArgumentException(s"BsonTransformer: didn't expect $num (not finite)")
    case _: BsonNull        => Json.Null
    case bool: BsonBoolean  => Json.fromBoolean(bool.getValue)
    case string: BsonString => Json.fromString(string.getValue)
    case id: BsonObjectId   => Json.fromString(id.getValue.toHexString)
    case theUnexpected      =>
      throw IllegalArgumentException(s"BsonTransformer: didn't expect $theUnexpected (${theUnexpected.getBsonType})")

    // Decoder

  def jsonTransformer(json: Json): BsonValue =
    json.fold[BsonValue](
      BsonNull.VALUE,
      jsonBoolean => BsonBoolean.valueOf(jsonBoolean),
      jsonNumber =>
        jsonNumber.toInt
          .map(BsonInt32(_))
          .orElse(jsonNumber.toLong.map(BsonInt64(_)))
          .getOrElse(BsonDouble(jsonNumber.toDouble)),
      jsonString => BsonString(jsonString),
      jsonArray => BsonArray(jsonArray.map(jsonTransformer).asJava),
      jsonObject => BsonDocument(jsonObject.toList.map((k, v) => BsonElement(k, jsonTransformer(v))).asJava)
    )
