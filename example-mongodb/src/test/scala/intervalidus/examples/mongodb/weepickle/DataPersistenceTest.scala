package intervalidus.examples.mongodb.weepickle

import com.rallyhealth.weepickle.v1.WeePickle.{From, FromTo, To, macroFromTo}
import intervalidus.DiscreteValue.given
import intervalidus.examples.mongodb.weepickle.BsonTransformer.given
import intervalidus.examples.mongodb.{DataPersistenceTestBehavior, Word}
import intervalidus.json.weepickle.Json.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

given FromTo[Word] = macroFromTo

/**
  * Demonstrate how dimensional data can be managed in a database. Uses MongoDB (via Testcontainers) to store, retrieve,
  * and update data, where JSON (actually BSON) pickling is provided by WeePickle.
  */
class DataPersistenceTest extends AnyFunSuite with Matchers with DataPersistenceTestBehavior[From, To]:
  override def transform[T, S](t: T)(using fromT: From[T], toS: To[S]): S = fromT.transform(t, toS)

  testsFor(commonBehaviors("WeePickle"))
