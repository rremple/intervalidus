package intervalidus.examples.mongodb.upickle

import ujson.AstTransformer
import upickle.default.{Reader, Writer}
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}
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

  given Writer[BsonValue] with
    def write0[Out](out: Visitor[?, Out], in: BsonValue): Out = bsonValueTransformer.transform(in, out)

  given Reader[BsonValue] = Reader.Delegate(bsonValueTransformer)

  given (using bsonValueWriter: Writer[BsonValue]): Writer[BsonDocument] = bsonValueWriter.narrow

  given (using bsonValueReader: Reader[BsonValue]): Reader[BsonDocument] = bsonValueReader.map(_.asDocument())

  private val bsonValueTransformer: AstTransformer[BsonValue] = new AstTransformer:

    // Writer

    override def transform[T](bsonValue: BsonValue, to: Visitor[?, T]): T = bsonValue match
      case doc: BsonDocument  => transformObject(to, doc.entrySet().asScala.map(e => e.getKey -> e.getValue))
      case arr: BsonArray     => transformArray(to, arr.asScala)
      case num: BsonInt32     => to.visitInt32(num.getValue, -1)
      case num: BsonInt64     => to.visitInt64(num.getValue, -1)
      case num: BsonDouble    => to.visitFloat64(num.getValue, -1)
      case _: BsonNull        => to.visitNull(-1)
      case bool: BsonBoolean  => if bool.getValue then to.visitTrue(-1) else to.visitFalse(-1)
      case string: BsonString => to.visitString(string.getValue, -1)
      case id: BsonObjectId   => to.visitString(id.getValue.toHexString, -1)
      case theUnexpected      =>
        throw Exception(s"BsonTransformer: didn't expect $theUnexpected (${theUnexpected.getBsonType})")

    // Reader

    override def visitJsonableObject(length: Int, index: Int): ObjVisitor[BsonValue, BsonValue] =
      AstObjVisitor[Seq[(String, BsonValue)]](a => BsonDocument(a.map(BsonElement(_, _)).asJava))

    override def visitArray(length: Int, index: Int): ArrVisitor[BsonValue, BsonValue] =
      AstArrVisitor[Seq](a => BsonArray(a.asJava))

    override def visitFloat64StringParts(cs: CharSequence, decIndex: Int, expIndex: Int, index: Int): BsonValue =
      val double = cs.toString.toDouble
      if double.isValidInt then BsonInt32(double.intValue)
      else if double.isWhole then BsonInt64(double.longValue)
      else BsonDouble(double)

    override def visitNull(index: Int): BsonValue = BsonNull.VALUE
    override def visitFalse(index: Int): BsonValue = BsonBoolean.FALSE
    override def visitTrue(index: Int): BsonValue = BsonBoolean.TRUE
    override def visitString(cs: CharSequence, index: Int): BsonValue = BsonString(cs.toString)
