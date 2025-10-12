package intervalidus.examples.mongodb

import com.dimafeng.testcontainers.MongoDBContainer
import com.dimafeng.testcontainers.scalatest.TestContainersForAll
import com.mongodb.client.{MongoClient, MongoClients, MongoCollection, MongoDatabase}
import org.bson.BsonDocument
import org.scalatest.Suite
import org.testcontainers.utility.DockerImageName

trait MongoDBContainerLike extends TestContainersForAll:
  self: Suite =>

  // Explicitly tell the trait what type of container(s) to expect.
  override type Containers = MongoDBContainer

  // Define a MongoDB container to be used for all tests in this suite
  override def startContainers(): MongoDBContainer =
    MongoDBContainer.Def(DockerImageName.parse("mongodb/mongodb-community-server:latest")).start()

  extension (container: MongoDBContainer) def client: MongoClient = MongoClients.create(container.replicaSetUrl)

  extension (client: MongoClient)
    def collection(collectionName: String, databaseName: String = "test_db"): MongoCollection[BsonDocument] =
      val database: MongoDatabase = client.getDatabase(databaseName)
      database.getCollection(collectionName, classOf[BsonDocument])
