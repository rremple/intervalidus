package intervalidus.examples.mongodb

import com.mongodb.client.model.{IndexOptions, Indexes}
import intervalidus.{Domain, ValidData}
import intervalidus.DiffAction.{Create, Delete, Update}
import intervalidus.DiscreteValue.given
import intervalidus.Domain.In1D
import intervalidus.Interval1D.{interval, intervalFrom, intervalTo}
import intervalidus.immutable.Data
import org.bson.{BsonDocument, BsonValue}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

case class Word(english: String, italian: String)

/**
  * Demonstrate how dimensional data can be managed in a database. Uses MongoDB (via Testcontainers) to store, retrieve,
  * and update data, where JSON (actually BSON).WeePickle and uPickle test only differ in the monad type names.
  *
  * @tparam W
  *   monad for writing from a type (Writer/From)
  * @tparam R
  *   monad for reading to a type (Reader/To)
  */
trait DataPersistenceTestBehavior[W[_], R[_]](using
  W[BsonValue],
  W[BsonDocument],
  W[Iterable[BsonDocument]],
  W[Domain.In1D[Int]],
  W[ValidData[Word, In1D[Int]]],
  W[Data[Word, In1D[Int]]],
  R[BsonValue],
  R[BsonDocument],
  R[Seq[BsonDocument]],
  R[Data[Word, In1D[Int]]]
) extends MongoDBContainerLike:
  this: AnyFunSuite with Matchers =>

  protected def transform[T, S](t: T)(using W[T], R[S]): S

  extension [T: W](value: T) def as[S: R]: S = transform(value)

  type DataIn1D = Data[Word, In1D[Int]]
  type ValidIn1D = ValidData[Word, In1D[Int]]

  def commonBehaviors(prefix: String): Unit =
    test(s"$prefix: MongoDB container should be able to represent evolving intervalidus data"):
      withContainers: container =>
        val client = container.client
        val collection = client.collection(prefix)
        val intervalStartPath = "interval.start"
        collection.createIndex(Indexes.ascending(intervalStartPath), IndexOptions().name("PK").unique(true))

        // Define locally
        val initialData = List(
          intervalTo(4) -> Word("Hey", "Ciao"),
          interval(5, 15) -> Word("to", "al"),
          intervalFrom(16) -> Word("World", "Mondo")
        )
        val definedLocally: DataIn1D = Data(initialData)

        // Store in the database
        val insertResult = collection.insertMany(definedLocally.as[Seq[BsonDocument]].asJava)
        insertResult.getInsertedIds.size() shouldBe initialData.size

        // Retrieve from the database -- should match the local definition
        val initialRetrieved: DataIn1D = collection.find().asScala.as[DataIn1D]
        initialRetrieved.getAll.toList shouldBe definedLocally.getAll.toList

        // Modify locally
        val modifiedLocally = definedLocally.remove(interval(1, 19))
        modifiedLocally.getAll.toList shouldBe List(
          intervalTo(0) -> Word("Hey", "Ciao"), // truncated
          // interval(5, 15) -> Word("to", "al") // deleted
          intervalFrom(20) -> Word("World", "Mondo") // new start: deleted old and created new
        )

        // Determine the differences applied
        val modifications = modifiedLocally.diffActionsFrom(definedLocally)
        modifications shouldBe List(
          Update(intervalTo(0) -> Word("Hey", "Ciao")),
          Delete(Domain.in1D(5)),
          Delete(Domain.in1D(16)),
          Create(intervalFrom(20) -> Word("World", "Mondo"))
        )

        // Modify the database by applying each of the diff actions
        def byKey(intervalStart: In1D[Int]): BsonDocument = BsonDocument(intervalStartPath, intervalStart.as[BsonValue])
        modifications.foreach:
          case Create(data: ValidIn1D) => collection.insertOne(data.as[BsonDocument])
          case Update(data: ValidIn1D) => collection.replaceOne(byKey(data.interval.start), data.as[BsonDocument])
          case Delete(intervalStart)   => collection.deleteOne(byKey(intervalStart))

        // Retrieve modifications from the database -- should match the local modifications
        val modifiedRetrieved: DataIn1D = collection.find().asScala.as[DataIn1D]
        modifiedRetrieved.getAll.toList shouldBe modifiedLocally.getAll.toList

        // probably not necessary
        // collection.drop()
        // client.close()
