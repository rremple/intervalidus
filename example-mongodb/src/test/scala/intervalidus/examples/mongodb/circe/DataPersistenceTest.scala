package intervalidus.examples.mongodb.circe

import intervalidus.DiscreteValue.given
import intervalidus.examples.mongodb.circe.BsonTransformer.given
import intervalidus.examples.mongodb.{DataPersistenceTestBehavior, LevelWord, Word}
import intervalidus.json.circe.Json.given
import io.circe.*
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

given Codec[Word] = Codec.AsObject.derived
given Codec[LevelWord] = Codec.AsObject.derived

/**
  * Demonstrate how dimensional data can be managed in a database. Uses MongoDB (via Testcontainers) to store, retrieve,
  * and update data, where JSON (actually BSON) pickling is provided by Circe.
  */
class DataPersistenceTest extends AnyFunSuite with Matchers with DataPersistenceTestBehavior[Encoder, Decoder]:
  override def transform[T: Encoder, S: Decoder](t: T): S = t.asJson.as[S] match {
    case Left(error)  => throw Exception(s"Couldn't transform $t")
    case Right(value) => value
  }

  testsFor(commonBehaviors("Circe"))
