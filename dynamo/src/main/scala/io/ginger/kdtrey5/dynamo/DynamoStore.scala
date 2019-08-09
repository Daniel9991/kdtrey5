package io.ginger.kdtrey5.dynamo

import scala.reflect.ClassTag

import io.ginger.kdtrey5.data._

import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.auto._
import org.scanamo.error.DynamoReadError

import cats._
import cats.data.EitherT
import cats.implicits._

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import org.scanamo.error.ScanamoError
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement
import com.amazonaws.services.dynamodbv2.model.KeyType
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsync
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput

object Helpers {
  def raise(e: ScanamoError) = throw new Exception(e.toString)
}

object DynamoStore {
  val rootNodeIdKey = "@@@@@@@@_ROOT_NODE_ID_@@@@@@@@"
}

trait DynamoStore[K, V] extends KVStore[K, V] {
  import DynamoStore._
  import Helpers._

  implicit val kClassTag: ClassTag[K]
  implicit val vClassTag: ClassTag[V]

  val keyCodec: Codec[K, NodeId]
  val valueCodec: Codec[V, String]
  
  val baseTableName: String

  protected val dynamo: AmazonDynamoDBAsync

  lazy val versions = Table[Version](s"$baseTableName-versions")

  lazy val nodes = {
    val latestVersion = {
      val allVersions = EitherT(Scanamo(dynamo).exec(versions.scan())).fold(raise, identity)
      allVersions.map(_.version).maximumOption.get
    }
    println(s"latestVersion: $latestVersion")
    Table[Node](s"$baseTableName-$latestVersion")
  }

  override def rootId: NodeId = {
    Scanamo(dynamo).exec(nodes.get('id -> rootNodeIdKey)).get.fold(raise, _.keys.head)
  }

  override def rootId_=(id: NodeId): Unit = {
    val newNode = Node(
      id = rootNodeIdKey, 
      keys = Seq(id),
      nodes = Seq.empty,
      values = Seq.empty,
      size = 1)
    Scanamo(dynamo).exec(nodes.put(newNode))
  }

  override def load(id: NodeId): KDNode[K, V] = {
    val node = Scanamo(dynamo).exec(nodes.get('id -> id)).get.fold(raise, identity)
    if (node.values.nonEmpty) {
      KDLeaf(
        id = node.id,
        keys = node.keys.map(keyCodec.decode).toArray,
        values = node.values.map(valueCodec.decode).toArray,
        size = node.size
      )
    } else {
      KDBranch(
        id = node.id,
        keys = node.keys.map(keyCodec.decode).toArray,
        nodes = node.values.toArray,
        size = node.size
      )
    }
  }

  override def store(id: NodeId, node: KDNode[K, V]): Unit = {
    val newNode: Node = node match {
      case leaf: KDLeaf[K, V] =>
        Node(
          id = id, 
          keys = node.keys.map(keyCodec.encode),
          nodes = Seq.empty,
          values = leaf.values.map(valueCodec.encode),
          size = node.size)
      case branch: KDBranch[K, V] =>
        Node(
          id = id, 
          keys = node.keys.map(keyCodec.encode),
          nodes = Seq.empty,
          values = branch.nodes,
          size = node.size)
    }
    Scanamo(dynamo).exec(nodes.put(newNode))
  }

  def createVersionsTable(): Unit = {
    val request = new CreateTableRequest()
      .withTableName(s"$baseTableName-versions")
      .withKeySchema(
        new KeySchemaElement().withAttributeName("version").withKeyType(KeyType.HASH))
      .withAttributeDefinitions(
        new AttributeDefinition().withAttributeName("version").withAttributeType(ScalarAttributeType.S))
      .withProvisionedThroughput(new ProvisionedThroughput(1, 1))
    dynamo.createTable(request)
  }

  def createNewVersion(version: String): Unit = {
    val request = new CreateTableRequest()
      .withTableName(s"$baseTableName-$version")
      .withKeySchema(
        new KeySchemaElement().withAttributeName("id").withKeyType(KeyType.HASH))
      .withAttributeDefinitions(
        new AttributeDefinition().withAttributeName("id").withAttributeType(ScalarAttributeType.S))
      .withProvisionedThroughput(new ProvisionedThroughput(1, 1))
    val result = dynamo.createTable(request)
    val newVersion = Version(version)
    Scanamo(dynamo).exec(versions.put(newVersion))
  }
}

case class Version(version: String)

case class Node(
  id: NodeId,
  keys: Seq[String],
  nodes: Seq[String],
  values: Seq[String],
  size: Int)

