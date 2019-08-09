package io.ginger.kdtrey5.dynamo

import io.ginger.kdtrey5.data._

import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.auto._

import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._

import org.scalatest._
import org.scalatest.Matchers._

import java.util.BitSet
import io.ginger.kdtrey5.coordinates.BitsetCoordinateSystem
import scala.reflect.ClassTag

class DynamoStoreTest extends FunSuite {

  val prefix = "kdtrey5"

  val leaf = KDLeaf(
    id = "root", 
    keys = Array(BitSetUtils.bitSetFromString("1011")), 
    values = Array("11"), 
    size = 1)
  
  test("create tables") {
    val store = new TestStore()
    store.createVersionsTable()
    store.createNewVersion("v1")
  }

  test("store a node and set rootId") {
    val store = new TestStore()
    store.store("root", leaf)
    store.rootId = "root"
  }

  test("get rootId and load node") {
    val store = new TestStore()
    store.rootId shouldBe "root"
    store.load("root") shouldBe leaf
  }
}

object BitSetUtils {
  // note: unoptimized
  def bitSetFromString(binary: String): BitSet = {
    val bs = new BitSet()
    binary.reverse.zipWithIndex foreach { case (bit, pos) =>
      bit match {
        case '0' => // false by default
        case '1' => bs.set(pos, true)
        case _ => throw new Exception(s"illegal value for bits: $binary")
      }
    }
    bs
  }

  def bitSetToString(bits: BitSet) = {
    val sb = new StringBuilder()
    var pos = bits.length - 1
    while (pos >= 0) {
      val ch = if (bits.get(pos)) '1' else '0'
      sb.append(ch)
      pos -= 1
    }
    sb.toString
  }
}

class TestStore extends DynamoStore[BitSet, String] {
  import BitSetUtils._

  override val baseTableName: String = "dummy"
  override val dynamo = LocalDynamoDB.client()

  override implicit val kClassTag = ClassTag(classOf[BitSet])
  override implicit val vClassTag = ClassTag(classOf[String])

  override val keyCodec = new Codec[BitSet, NodeId] {
    override def encode(bits: BitSet): String = bitSetToString(bits)
    override def decode(str: String): BitSet = bitSetFromString(str)
  }

  override val valueCodec = new Codec[String, String] {
    override def decode(str: String): String = str
    override def encode(str: String): String = str
  }
}