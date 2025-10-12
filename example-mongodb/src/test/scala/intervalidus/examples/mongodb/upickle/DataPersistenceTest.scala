package intervalidus.examples.mongodb.upickle

import com.mongodb.client.model.{IndexOptions, Indexes}
import intervalidus.DiffAction.{Create, Delete, Update}
import intervalidus.DiscreteValue.given
import intervalidus.Domain.In1D
import intervalidus.Interval1D.{interval, intervalFrom, intervalTo}
import intervalidus.examples.mongodb.MongoDBContainerLike
import intervalidus.immutable.Data
import intervalidus.{Domain, ValidData}
import org.bson.{BsonDocument, BsonValue}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

/**
  * Demonstrate how dimensional data can be managed in a database. Uses MongoDB (via Testcontainers) to store, retrieve,
  * and update data, where JSON (actually BSON) pickling is provided by Upickle.
  */
class DataPersistenceTest extends AnyFlatSpec with Matchers with MongoDBContainerLike:

  import BsonTransformer.given

  import intervalidus.json.upickle.Json.given
  import upickle.default.{Reader, Writer, transform}

  extension [T](value: T)(using Writer[T]) def as[S](using Reader[S]): S = transform(value).to[S]

  case class Word(english: String, italian: String) derives Writer, Reader

  "Upickle/MongoDB container" should "be able to represent evolving intervalidus data" in withContainers: container =>
    val client = container.client
    val collection = client.collection("upickle")
    val intervalStartPath = "interval.start"
    collection.createIndex(Indexes.ascending(intervalStartPath), IndexOptions().name("PK").unique(true))

    type DataIn1D = Data[Word, In1D[Int]]
    type ValidIn1D = ValidData[Word, In1D[Int]]

    // Define locally
    val initialData = List(
      intervalTo(4) -> Word("Hey", "Ciao"),
      interval(5, 15) -> Word("to", "al"),
      intervalFrom(16) -> Word("World", "Mondo")
    )
    val definedLocally: DataIn1D = Data(initialData)

    // Store in the database
    val insertResult = collection.insertMany(definedLocally.as[Seq[BsonDocument]].asJava)
    insertResult.getInsertedIds should not be null
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
