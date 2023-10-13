package io.ginger.kdtrey5.dynamo

import java.util.BitSet
import io.ginger.kdtrey5.data._
import org.scanamo._
import scala.reflect.ClassTag

class TestStore extends DynamoStore[BitSet, String] {
  import BitSetUtils._

  override val baseTableName: String = "dummy"
  override val dynamo = LocalDynamoDB.syncClient()

  override implicit val kClassTag: ClassTag[BitSet] = ClassTag(classOf[BitSet])
  override implicit val vClassTag: ClassTag[String] = ClassTag(classOf[String])

  override val keyCodec = new Codec[BitSet, NodeId] {
    override def encode(bits: BitSet): String = bitSetToString(bits)
    override def decode(str: String): BitSet = bitSetFromString(str)
  }

  override val valueCodec = new Codec[String, String] {
    override def decode(str: String): String = str
    override def encode(str: String): String = str
  }
}
