package intervalidus.examples.mongodb.play

import intervalidus.DiscreteValue.given
import intervalidus.examples.mongodb.play.BsonTransformer.given
import intervalidus.examples.mongodb.{DataPersistenceTestBehavior, LevelWord, Word}
import intervalidus.json.play.Json.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.*

given Format[Word] = Json.format[Word]
given Format[LevelWord] = Json.format[LevelWord]

class DataPersistenceTest extends AnyFunSuite with Matchers with DataPersistenceTestBehavior[Writes, Reads]:

  override def transform[T: Writes, S: Reads](t: T): S =
    val jsValue = Json.toJson(t)
    jsValue.validate[S] match
      case JsSuccess(value, _) => value
      case JsError(errors)     => throw Exception(s"Couldn't transform $t: $errors")

  testsFor(commonBehaviors("Play"))
