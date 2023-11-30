package io.ginger.kdtrey5

import io.ginger.kdtrey5.coordinates._
import io.ginger.kdtrey5.data._

import java.util.Comparator
import java.util.concurrent.PriorityBlockingQueue
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import scala.collection._

import skiis2.Skiis

case class RangeFindStats(branchesRetrieved: Int, leavesRetrieved: Int)

trait RangeFindResult[K, V] {
  val values: Skiis[(K, V)]
  def stats: RangeFindStats
}

/**
 * A KD-Tree is a tree-like data structure that supports efficient range queries over multi-dimensional
 * key space.
 *
 * Similarly to B+Trees, this implementation supports n-ary nodes (aka pages) for efficient retrieval
 * over high-latency storage.
 */
trait KDTree {
  import CoordinateSystem.Distance

  type COORDS <: CoordinateSystem


  /** Implementations must supply a coordinate system that defines notions of POINTs in a multi-dimensional space
      and DISTANCE between such POINTs. */
  val coords: COORDS

  type Point = coords.POINT

  /** Keys of the tree are POINTs, values are left fully abstract */
  type K = Point
  type V

  /** Nodes are defined in terms of the above key and value types */
  type Node = KDNode[K, V]
  type Branch = KDBranch[K, V]
  type Leaf = KDLeaf[K, V]

  /** A backing store is needed to retrieve keys and values */
  val store: KVStore[K, V]

  private case class NodeDistance(nodeId: NodeId, minDistance: Distance)
  private case class PointDistance(key: K, value: V, distance: Distance)

  private object NodeDistanceComparator extends Comparator[NodeDistance] {
    override def compare(d1: NodeDistance, d2: NodeDistance): Int = {
      if (d1.minDistance > d2.minDistance) return 1
      else if (d1.minDistance < d2.minDistance) return -1
      else return 0
    }
  }

  private object PointDistanceComparator extends Comparator[PointDistance] {
    override def compare(d1: PointDistance, d2: PointDistance): Int = {
      if (d1.distance > d2.distance) return 1
      else if (d1.distance < d2.distance) return -1
      else return 0
    }
  }

  /** Find all existing points within `distance` of the given `point */
  def rangeFind(
    target: Point,
    distance: Distance
  )(implicit skiisContext: Skiis.Context
  ): RangeFindResult[K, V] = {
    val branchesRetrieved = new AtomicInteger(0)
    val leavesRetrieved = new AtomicInteger(0)

    def updateStats(node: Node) = node match {
      case b: Branch => branchesRetrieved.incrementAndGet()
      case l: Leaf   => leavesRetrieved.incrementAndGet()
    }

    // backpressure happens through queue created in `parMapWithQueue`
    val nodeQueue = new Skiis.Queue[Node](Int.MaxValue, maxAwaiting = skiisContext.parallelism)

    // start at the root of the tree
    val root = store.load(store.rootId)
    updateStats(root)
    nodeQueue += root

    /** implements a depth-first range search */
    val results = nodeQueue.parMapWithQueue[(Point, V)]((node, values) => {
      try {
        // debug(s"findNext() target=$target distance=$distance")
        node match {
          case branch: KDBranch[K, V] =>
            // evaluate all branches to see if they contain values that are possibly
            // within the range, if so push the child node (branch or leaf) on the stack

            // debug(s"findNext() $branch")

            var pos = 0
            while (pos < branch.nodes.length && branch.nodes(pos) != null) {
              val node = branch.nodes(pos)
              val min = branch.keys(pos)
              val max = branch.keys(pos + 1)
              // debug(s"findNext() node $node min $min max $max")

              // debug(s"minDistance ${coords.minDistance(target, min, max)}")
              if (coords.minDistance(target, min, max) <= distance) {
                val child = store.load(node)
                // debug(s"enqueue: ${node}")
                updateStats(child)
                nodeQueue += child
              } else {
                // debug(s"skip: ${node}")
              }

              pos += 1
            }

          case leaf: KDLeaf[K, V] =>
            // debug(s"findNext() leaf ${leaf.id}")
            var pos = 0
            while (pos < leaf.keyCount) {
              val key = leaf.keys(pos)
              // debug(s"findNext() leaf ${leaf.id} ${key} distance ${coords.distance(key, target)}")
              if (coords.distance(key, target) <= distance) {
                // debug(s"findNext() result found ${(key, leaf.values(pos))}")
                values += (key, leaf.values(pos))
              } else {
                // debug(s"findNext() skip key ${(key, leaf.values(pos))}")
              }
              pos += 1
            }
        } // match
      } catch {
        case e: Exception => e.printStackTrace(); throw e
      }
    })(skiisContext)
    new RangeFindResult[K, V] {
      override val values = results
      override def stats = RangeFindStats(branchesRetrieved.get, leavesRetrieved.get)
    }
  }

  /** Find all existing points within `distance` of the given `point */
  def kNearestSearch(
    target: Point,
    k: Int,
    maxDistance: Distance
  )(implicit skiisContext: Skiis.Context
  ): RangeFindResult[K, V] = {
    val branchesRetrieved = new AtomicInteger(0)
    val leavesRetrieved = new AtomicInteger(0)

    def updateStats(node: Node) = node match {
      case b: Branch => branchesRetrieved.incrementAndGet()
      case l: Leaf   => leavesRetrieved.incrementAndGet()
    }

    // backpressure happens through queue created in `parMapWithQueue`
    val pendingNodes = new Skiis.Queue[Node](Int.MaxValue, maxAwaiting = skiisContext.parallelism)

    // start at the root of the tree
    val root = store.load(store.rootId)
    updateStats(root)
    pendingNodes += root

    val initialCapacity = 100
    val candidateNodes = new PriorityBlockingQueue[NodeDistance](initialCapacity, NodeDistanceComparator)
    val candidatePoints = new PriorityBlockingQueue[PointDistance](initialCapacity, PointDistanceComparator)
    val topKRef = new PriorityBlockingQueue[PointDistance](k, PointDistanceComparator)
    val actualK = new AtomicInteger(0)

    val results = pendingNodes.parMapWithQueue[(Point, V)]((node, resultQueue) => {
      try {
        // debug(s"kNearestSearch() target=$target k=$k maxDistance=$maxDistance")
        // debug(s"kNearestSearch() current node ${node.id}")
        node match {
          case branch: KDBranch[K, V] =>
            // evaluate all branches to see if they contain values that are possibly
            // within the range, if so push the child node (branch or leaf) on the stack

            var pos = 0
            while (pos < branch.nodes.length && branch.nodes(pos) != null) {
              val nodeId = branch.nodes(pos)
              val min = branch.keys(pos)
              val max = branch.keys(pos + 1)
              val maxPoint = topKRef.peek()
              // debug(s"kNearestSearch() maxPoint=${maxPoint}")
              val minDistance = coords.minDistance(target, min, max)
              // debug(s"kNearestSearch() DistanceNode nodeId=${nodeId} minDistance=${minDistance}")
              if (minDistance <= maxDistance && (maxPoint == null || minDistance < maxPoint.distance)) {
                // debug(s"kNearestSearch() candidateNodes += ${nodeId}")
                candidateNodes.add(NodeDistance(nodeId, minDistance))
              }
              pos += 1
            }

          case leaf: KDLeaf[K, V] =>
            // debug(s"kNearestSearch() leaf ${leaf.id}")
            var pos = 0
            while (pos < leaf.keyCount) {
              val point = leaf.keys(pos)
              val value = leaf.values(pos)
              val distance = coords.distance(point, target)
              // debug(s"kNearestSearch() leaf ${leaf.id} ${point} ${value} distance ${distance}")
              val currentK = topKRef.size
              val maxPoint = topKRef.peek()
              // debug(s"kNearestSearch() currentK2=${currentK} maxPoint2=${maxPoint}")
              var added = false
              if (currentK < k && (maxPoint == null || distance < maxPoint.distance)) {
                // debug(s"kNearestSearch() candidate point ${(point, value)}")
                candidatePoints.add(PointDistance(point, value, distance))
                added = true
              }
              if (added && topKRef.size > k) {
                candidatePoints.poll()
              }
              pos += 1
            }
        } // match

        val distanceNode = candidateNodes.poll()
        if (distanceNode != null) {
          val currentK = topKRef.size
          val maxPoint = topKRef.peek()
          // debug(s"kNearestSearch() currentK=${currentK} maxPoint=${maxPoint}")
          if (currentK < k &&
          (maxPoint == null || distanceNode.minDistance < maxPoint.distance)
          && distanceNode.minDistance <= maxDistance) {
            val node = store.load(distanceNode.nodeId)
            // debug(s"kNearestSearch() pendingNodes += ${node}")
            updateStats(node)
            pendingNodes += node
          }
        }
        var doneWithResults = false
        while (!doneWithResults && actualK.get < k) {
          val point = candidatePoints.poll()
          if (point != null) {
            resultQueue += (point.key, point.value)
            actualK.addAndGet(1)
          } else {
            doneWithResults = true
          }
        }
      } catch {
        case e: Exception => e.printStackTrace(); throw e
      }
    })(skiisContext)
    new RangeFindResult[K, V] {
      override val values = results
      override def stats = RangeFindStats(branchesRetrieved.get, leavesRetrieved.get)
    }
  }

  /* debug facilities commentted out for performance but left intact to facilitate eventual
     debugging (or understanding of the algorithm for the curious) */

  /* uncomment this if needed
  private def debug(s: String) = {
    if (false) {
      println(s)
    }
  }
  */
}

/** A KD-Tree using a bitset-based coordinate system */
trait BitsetKDTree extends KDTree {
  override type COORDS = BitsetCoordinateSystem.type
  override val coords = BitsetCoordinateSystem
}

trait VectorKDTree extends KDTree {
  override type COORDS = VectorCoordinateSystem.type
  override val coords = VectorCoordinateSystem
}
