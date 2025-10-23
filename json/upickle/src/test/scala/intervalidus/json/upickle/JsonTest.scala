package intervalidus.json.upickle

import upickle.default.{Reader, Writer, write}
import ujson.StringParser
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.json.JsonTestBehavior
import intervalidus.json.upickle.Json.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonTest extends AnyFunSuite with Matchers with JsonTestBehavior[Writer, Reader]:
  override def jsonTo[T](json: String)(using readT: Reader[T]): T = StringParser.transform(json, readT)
  override def toJsonString[T](t: T)(using Writer[T]): String = write(t)

  testsFor(commonBehaviors("uPickle"))
