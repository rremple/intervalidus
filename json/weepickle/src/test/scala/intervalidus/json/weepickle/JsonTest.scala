package intervalidus.json.weepickle

import com.rallyhealth.weejson.v1.jackson.{FromJson, ToJson}
import com.rallyhealth.weepickle.v1.WeePickle.{From, To}
import intervalidus.DiscreteValue.given
import intervalidus.DomainLike.given
import intervalidus.json.JsonTestBehavior
import intervalidus.json.weepickle.Json.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonTest extends AnyFunSuite with Matchers with JsonTestBehavior[From, To]:
  override def jsonTo[T](json: String)(using toT: To[T]): T = FromJson(json).transform(toT)
  override def toJsonString[T](t: T)(using fromT: From[T]): String = fromT.transform(t, ToJson.string)

  testsFor(commonBehaviors("WeePickle"))
