package intervalidus.examples.mongodb

import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class MongoDBContainerTest extends AnyFlatSpec with Matchers with MongoDBContainerLike:

  given Conversion[String, BsonString] = BsonString(_)

  "MongoDB container" should "be able to insert and retrieve documents" in withContainers: container =>
    val client = container.client
    val collection = client.collection("users")
    val userDoc = BsonDocument("name", "Alice").append("email", "alice@example.com")
    val insertResult = collection.insertOne(userDoc)
    insertResult.getInsertedId should not be null

    val filter = BsonDocument("name", "Alice")
    val foundDoc = collection.find(filter).first()
    foundDoc should not be null

    foundDoc.getString("email").getValue shouldBe "alice@example.com"

    // probably not necessary
    // collection.drop()
    // client.close()
