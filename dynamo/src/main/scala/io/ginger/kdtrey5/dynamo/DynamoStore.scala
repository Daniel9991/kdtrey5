package io.ginger.kdtrey5.dynamo

import scala.reflect.ClassTag

import io.ginger.kdtrey5.data._

import org.scanamo._
import org.scanamo.syntax._

import cats._
import cats.data.EitherT
import cats.implicits._

import org.scanamo.generic.auto._

import software.amazon.awssdk.services.dynamodb.model.CreateTableRequest
import software.amazon.awssdk.services.dynamodb.model.KeySchemaElement
import software.amazon.awssdk.services.dynamodb.model.KeyType
import software.amazon.awssdk.services.dynamodb.model.AttributeDefinition
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType
import software.amazon.awssdk.services.dynamodb.model.ProvisionedThroughput
import software.amazon.awssdk.services.dynamodb.model.TableStatus
import software.amazon.awssdk.services.dynamodb.model.ResourceNotFoundException
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.DescribeTableRequest

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

  protected val dynamo: DynamoDbClient

  lazy val versions = Table[Version](s"$baseTableName-versions")

  lazy val nodes = {
    val latestVersion = {
      val allVersions = EitherT(Scanamo(dynamo).exec(versions.scan())).fold(raise, identity)
      allVersions.map(_.version).maximumOption.get
    }
    println(s"latestVersion: $baseTableName-$latestVersion")
    Table[Node](s"$baseTableName-$latestVersion")
  }

  override def rootId: NodeId = {
    Scanamo(dynamo).exec(nodes.get("id" -> rootNodeIdKey)).get.fold(raise _, _.keys.head)
  }

  override def rootId_=(id: NodeId): Unit = {
    val newNode = Node(
      id = rootNodeIdKey,
      keys = Seq(id),
      nodes = Seq.empty,
      lastKey = None,
      values = Seq.empty,
      size = 1
    )
    Scanamo(dynamo).exec(nodes.put(newNode))
  }

  override def load(id: NodeId): KDNode[K, V] = {
    val node: Node = Scanamo(dynamo).exec(nodes.get("id" -> id)).get.fold(raise _, identity(_))
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
        lastKey =
          if (node.lastKey.isDefined) keyCodec.decode(node.lastKey.get) else null.asInstanceOf[K],
        nodes = node.nodes.toArray,
        size = node.size
      )
    }
  }

  override def store(id: NodeId, node: KDNode[K, V]): Unit = {
    val newNode: Node = node match {
      case leaf: KDLeaf[K, V] =>
        Node(
          id = id,
          keys = node.keys.filter(_ != null).map(keyCodec.encode),
          lastKey = None,
          nodes = Seq.empty,
          values = leaf.values.filter(_ != null).map(valueCodec.encode),
          size = node.size
        )
      case branch: KDBranch[K, V] =>
        Node(
          id = id,
          keys = node.keys.filter(_ != null).map(keyCodec.encode),
          lastKey = if (node.lastKey != null) Some(keyCodec.encode(node.lastKey)) else None,
          nodes = branch.nodes.filter(_ != null),
          values = Seq.empty,
          size = node.size
        )
    }
    Scanamo(dynamo).exec(nodes.put(newNode))
  }

  def createVersionsTableIfNecessary(): Unit = {
    val tableName = s"$baseTableName-versions"
    if (tableExists(tableName)) return

    val request = CreateTableRequest
      .builder()
      .tableName(tableName)
      .keySchema(KeySchemaElement.builder.attributeName("version").keyType(KeyType.HASH).build())
      .attributeDefinitions(
        AttributeDefinition
          .builder()
          .attributeName("version")
          .attributeType(ScalarAttributeType.S)
          .build()
      )
      .provisionedThroughput(
        ProvisionedThroughput.builder().readCapacityUnits(1).writeCapacityUnits(1).build
      )
      .build()
    dynamo.createTable(request)
    waitForTableReady(tableName)
  }

  def createNewVersion(version: String): Unit = {
    val tableName = s"$baseTableName-$version"
    val request = CreateTableRequest
      .builder()
      .tableName(tableName)
      .keySchema(KeySchemaElement.builder.attributeName("id").keyType(KeyType.HASH).build())
      .attributeDefinitions(
        AttributeDefinition
          .builder()
          .attributeName("id")
          .attributeType(ScalarAttributeType.S)
          .build()
      )
      .provisionedThroughput(
        ProvisionedThroughput.builder().readCapacityUnits(1).writeCapacityUnits(1).build
      )
      .build()
    val result = dynamo.createTable(request)
    waitForTableReady(tableName)
    val newVersion = Version(version)
    Scanamo(dynamo).exec(versions.put(newVersion))
  }

  private def waitForTableReady(tableName: String): Unit = {
    var retriesLeft = 120
    while ({
      val response = dynamo.describeTable(DescribeTableRequest.builder().tableName(tableName).build)
      println(response)
      (response.table.tableStatus != TableStatus.ACTIVE) && (retriesLeft > 0)
    }) {
      Thread.sleep(1000L)
      retriesLeft -= 1
    }
    if (retriesLeft == 0) {
      throw new Exception(s"Timeout waiting for table creation: $tableName")
    }
  }

  private def tableExists(tableName: String): Boolean = {
    try {
      val response =
        dynamo.describeTable(DescribeTableRequest.builder().tableName(tableName).build())
      true
    } catch {
      case e: ResourceNotFoundException => false
    }
  }
}

case class Version(version: String)

case class Node(
  id: NodeId,
  keys: Seq[String],
  lastKey: Option[String],
  nodes: Seq[String],
  values: Seq[String],
  size: Int)
