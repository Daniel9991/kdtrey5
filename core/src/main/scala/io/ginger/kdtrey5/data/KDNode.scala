package io.ginger.kdtrey5.data

import io.ginger.kdtrey5.Utils._

sealed trait KDNode[K, V] {
  /** Unique id for this node */
  def id: NodeId
  def keys: Array[K]

  final def keyCount: Int = {
    var i = keys.length - 1
    while (i >= 0 && keys(i) == null) i -= 1
    i + 1
  }

  final def lastKey: K = keys(keyCount - 1)

  override def hashCode: Int = id.hashCode

  final def shallowEquals(x: Any): Boolean = x match {
    case other: KDNode[_, _] => (this.id == other.id)
    case _ => false
  }
}

case class KDBranch[K, V](
  /** Unique id for this node */
  override val id: NodeId,
  /** Pointers to children nodes */
  val nodes: Array[NodeId],
  /**  Min keys of each of the child nodes.
   *   Length of this array is (nodes.length + 1) since the max key of node `n` is <= node.keys(n + 1)
   */
  val keys: Array[K],
) extends KDNode[K, V] {

  assert(keys.length == nodes.length + 1)

  override def toString = {
    s"KDBranch($id, nodes=${nodes.mkString("{", ",", "}")} keys=${keys.mkString("{", ",", "}")}"
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: KDBranch[K, V] @unchecked =>
        this.id == other.id &&
          this.keyCount == other.keyCount &&
          arrayEquals(this.nodes, other.nodes, keyCount - 1) &&
          arrayEquals(this.keys, other.keys, keyCount)
      case _ => false
    }
  }
}

case class KDLeaf[K, V](
  override val id: NodeId,
  val keys: Array[K],
  val values: Array[V],
) extends KDNode[K, V] {
  override def toString =
    s"KDLeaf($id, keys=${keys.mkString("{", ",", "}")}, values=${values.mkString("{", ",", "}")})"

  override def equals(other: Any): Boolean = {
    other match {
      case other: KDLeaf[K, V] @unchecked =>
        this.id == other.id &&
          this.keyCount == other.keyCount &&
          arrayEquals(this.keys, other.keys, keyCount) &&
          arrayEquals(this.values, other.values, keyCount - 1)
      case _ => false
    }
  }
}
