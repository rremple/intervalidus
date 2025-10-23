package intervalidus.examples.mongodb.upickle

import upickle.default.{ReadWriter, Reader, Writer, macroRW}
import intervalidus.DiscreteValue.given
import intervalidus.examples.mongodb.upickle.BsonTransformer.given
import intervalidus.examples.mongodb.{DataPersistenceTestBehavior, Word}
import intervalidus.json.upickle.Json.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

given ReadWriter[Word] = macroRW

/**
  * Demonstrate how dimensional data can be managed in a database. Uses MongoDB (via Testcontainers) to store, retrieve,
  * and update data, where JSON (actually BSON) pickling is provided by uPickle.
  */
class DataPersistenceTest extends AnyFunSuite with Matchers with DataPersistenceTestBehavior[Writer, Reader]:
  override def transform[T, S](t: T)(using writeT: Writer[T], readS: Reader[S]): S = writeT.transform(t, readS)

  testsFor(commonBehaviors("uPickle"))
