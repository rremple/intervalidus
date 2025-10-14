package intervalidus.examples.mongodb.weepickle

import com.rallyhealth.weejson.v1.AstTransformer
import com.rallyhealth.weepickle.v1.WeePickle.{From, To}
import com.rallyhealth.weepickle.v1.core.{ArrVisitor, ObjVisitor, Visitor}
import org.bson.*

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

  given From[BsonValue] with
    def transform0[Out](in: BsonValue, out: Visitor[?, Out]): Out = bsonValueTransformer.transform(in, out)

  given To[BsonValue] = To.Delegate(bsonValueTransformer)

  given (using fromBsonValue: From[BsonValue]): From[BsonDocument] = fromBsonValue.narrow

  given (using toBsonValue: To[BsonValue]): To[BsonDocument] = toBsonValue.map(_.asDocument())

  private val bsonValueTransformer: AstTransformer[BsonValue] = new AstTransformer:

    // From

    override def transform[T](bsonValue: BsonValue, to: Visitor[?, T]): T = bsonValue match
      case doc: BsonDocument  => transformObject(to, doc.entrySet().asScala.map(e => e.getKey -> e.getValue))
      case arr: BsonArray     => transformArray(to, arr.asScala)
      case num: BsonInt32     => to.visitInt32(num.getValue)
      case num: BsonInt64     => to.visitInt64(num.getValue)
      case num: BsonDouble    => to.visitFloat64(num.getValue)
      case _: BsonNull        => to.visitNull()
      case bool: BsonBoolean  => if bool.getValue then to.visitTrue() else to.visitFalse()
      case string: BsonString => to.visitString(string.getValue)
      case id: BsonObjectId   => to.visitString(id.getValue.toHexString)
      case theUnexpected =>
        throw Exception(s"BsonTransformer: didn't expect $theUnexpected (${theUnexpected.getBsonType})")

    // To

    override def visitObject(length: Int): ObjVisitor[BsonValue, BsonValue] =
      AstObjVisitor[Seq[(String, BsonValue)]](a => BsonDocument(a.map(BsonElement(_, _)).asJava))

    override def visitArray(length: Int): ArrVisitor[BsonValue, BsonValue] =
      AstArrVisitor[Seq](a => BsonArray(a.asJava))

    override def visitFloat64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int): BsonValue =
      val double = cs.toString.toDouble
      if double.isValidInt then BsonInt32(double.intValue)
      else if double.isWhole then BsonInt64(double.longValue)
      else BsonDouble(double)

    override def visitNull(): BsonValue = BsonNull.VALUE
    override def visitFalse(): BsonValue = BsonBoolean.FALSE
    override def visitTrue(): BsonValue = BsonBoolean.TRUE
    override def visitString(cs: CharSequence): BsonValue = BsonString(cs.toString)
