package intervalidus.examples.mongodb

import com.mongodb.client.model.{IndexOptions, Indexes}
import intervalidus.{Domain, ValidData}
import intervalidus.DiffAction.{Create, Delete, Update}
import intervalidus.DiscreteValue.given
import intervalidus.Domain.In1D
import intervalidus.Interval1D.{interval, intervalFrom, intervalTo}
import intervalidus.immutable.Data
import org.bson.{BsonDocument, BsonInt32, BsonValue}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

case class Word(english: String, italian: String)

type WordIn1D = Data[Word, In1D[Int]]
type ValidWord = ValidData[Word, In1D[Int]]

case class LevelWord(level: Int, word: WordIn1D)

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
  W[ValidWord],
  W[WordIn1D],
  W[LevelWord],
  R[BsonValue],
  R[BsonDocument],
  R[Seq[BsonDocument]],
  R[WordIn1D],
  R[LevelWord]
) extends MongoDBContainerLike:
  this: AnyFunSuite with Matchers =>

  protected def transform[T, S](t: T)(using W[T], R[S]): S

  extension [T: W](value: T) def as[S: R]: S = transform(value)

  def commonBehaviors(prefix: String): Unit =
    val initialData = List(
      intervalTo(4) -> Word("Hey", "Ciao"),
      interval(5, 15) -> Word("to", "al"),
      intervalFrom(16) -> Word("World", "Mondo")
    )

    test(s"$prefix: MongoDB container should be able to represent evolving intervalidus data"):
      withContainers: container =>
        val client = container.client
        val collection = client.collection(prefix)
        val intervalStartPath = "interval.start"
        collection.createIndex(Indexes.ascending(intervalStartPath), IndexOptions().name("PK").unique(true))

        def byKey(intervalStart: In1D[Int]): BsonDocument = BsonDocument(intervalStartPath, intervalStart.as[BsonValue])

        def retrieve(): WordIn1D = collection.find().asScala.as[WordIn1D]

        // Define locally
        val definedLocally: WordIn1D = Data(initialData)

        // Store in the database
        val insertResult = collection.insertMany(definedLocally.as[Seq[BsonDocument]].asJava)
        insertResult.getInsertedIds.size() shouldBe initialData.size

        // Retrieve from the database -- should match the local definition
        retrieve() shouldBe definedLocally

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
        modifications.foreach:
          case Create(data: ValidWord) => collection.insertOne(data.as[BsonDocument])
          case Update(data: ValidWord) => collection.replaceOne(byKey(data.interval.start), data.as[BsonDocument])
          case Delete(intervalStart)   => collection.deleteOne(byKey(intervalStart))

        // Retrieve modifications from the database -- should match the local modifications
        retrieve() shouldBe modifiedLocally

        // probably not necessary
        // collection.drop()
        // client.close()

    test(s"$prefix: MongoDB container should be able to represent intervalidus data as elements"):
      withContainers: container =>
        val client = container.client
        val collection = client.collection(s"$prefix-level")
        val levelPath = "level"
        collection.createIndex(Indexes.ascending(levelPath), IndexOptions().name("PK").unique(true))

        def byKey(level: Int): BsonDocument = BsonDocument(levelPath, BsonInt32(level))

        def retrieve(level: Int): Option[LevelWord] =
          collection.find(byKey(level)).asScala.headOption.map(_.as[LevelWord])

        // Define locally
        val definedLocally: LevelWord = LevelWord(level = 1, Data(initialData))

        val insertResult = collection.insertOne(definedLocally.as[BsonDocument])
        assert(Option(insertResult.getInsertedId).isDefined)
        retrieve(definedLocally.level) shouldBe Some(definedLocally)

        val modifiedLocally = definedLocally.copy(word = definedLocally.word.remove(interval(1, 19)))
        collection.replaceOne(byKey(modifiedLocally.level), modifiedLocally.as[BsonDocument])
        retrieve(modifiedLocally.level) shouldBe Some(modifiedLocally)

        // probably not necessary
        // collection.drop()
        // client.close()
