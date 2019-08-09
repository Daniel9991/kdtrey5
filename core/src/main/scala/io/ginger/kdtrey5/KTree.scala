package io.ginger.kdtrey5

import scala.collection._

import io.ginger.kdtrey5.coordinates._
import io.ginger.kdtrey5.data._

/**
 * A KD-Tree is a tree-like data structure that supports efficient range queries over multi-dimensional 
 * key space.
 * 
 * Similarly to B+Trees, this implementation supports n-ary nodes (aka pages) for efficient retrieval 
 * over high-latency storage.
 */
trait KDTree {

  /** Implementations must supply a coordinate system that defines notions of POINTs in a multi-dimensional space
      and DISTANCE between such POINTs. */
  val coords: CoordinateSystem

  type Point = coords.POINT
  type Distance = coords.DISTANCE

  /** Keys of the tree are POINTs, values are left fully abstract */
  type K = Point
  type V

  /** Nodes are defined in terms of the above key and value types */
  type Node = KDNode[K, V]
  type Branch = KDBranch[K, V]
  type Leaf = KDLeaf[K, V]

  /** A backing store is needed to retrieve keys and values */
  val store: KVStore[K, V]

  /** Find all existing points within `distance` of the given `point */
  def rangeFind(target: Point, distance: Distance): Iterator[(Point, V)] = {
    /** implements a depth-first iterator-based range search */
    new Iterator[(Point, V)] {
      // candidate positions are queued up in a stack
      case class Position(node: Node, pos: Int)

      val _stack = mutable.Stack[Position]()

      // start at the root of the tree
      _stack.push(Position(store.load(store.rootId), 0)) 

      // prep internal state for `hasNext` method
      var _next = findNext()
      
      override def hasNext: Boolean = _next.isDefined

      override def next(): (Point, V) = {
        val next = _next.get
        _next = findNext()
        next
      }
      
      def findNext(): Option[(Point, V)] = {
        //debug(s"findNext() target=$target distance=$distance")

        while (_stack.nonEmpty) {
          var Position(node, pos) = _stack.pop()
          //debug(s"findNext() current node $node")
          node match {
            case branch: KDBranch[K, V] =>
              // evaluate all branches to see if they contain values that are possibly
              // within the range, if so push the child node (branch or leaf) on the stack

              pos = 0
              while (pos < branch.keys.length && branch.keys(pos) != null) {
                val p_current = branch.keys(pos)
                //debug(s"findNext() p_current $p_current")

                val possiblyWithinRange = {
                  // need a valid next node to compute common prefix
                  if (pos < branch.keys.length-1 && branch.keys(pos+1) != null) {
                    val p_next = branch.keys(pos+1)
                    val commonPrefix = coords.commonPrefixFromPoints(p_current, p_next)
                    //debug(s"findNext() p_next $p_next")
                    //debug(s"findNext() commonPrefix $commonPrefix")
                    //debug(s"findNext() minDistance ${coords.distance(target, commonPrefix)}")
                    (coords.distance(target, commonPrefix) <= distance)
                  } else true
                }
                
                if (possiblyWithinRange) {
                  //debug(s"possiblyWithinRange")
                  val child = store.load(branch.nodes(pos))
                  _stack.push(Position(child, 0))
                }

                pos += 1
              }

            case leaf: KDLeaf[K, V] =>
              while (pos < leaf.keys.length && leaf.keys(pos) != null) {
                val p_current = leaf.keys(pos)
                if (coords.distance(target, p_current) <= distance) {
                  // found a match, push cursor unto stack
                  if (pos < leaf.keys.length - 1) {
                    _stack.push(Position(leaf, pos + 1))
                  }
                  _next = Some((p_current, leaf.values(pos)))
                  return _next
                }
                pos += 1
              }              
          }
        }
        _next = None
        return _next
      }
    }
  }

  /* debug facilities commentted out for performance but left intact to facilitate eventual 
     debugging (or understanding of the algorithm for the curious) */

  /* uncomment this if needed
  private def debug(s: String) = {
    println(s)
  }
  */
}

/** A KD-Tree using a bitset-based coordinate system */
trait BitsetKDTree extends KDTree {
  override val coords = BitsetCoordinateSystem
}