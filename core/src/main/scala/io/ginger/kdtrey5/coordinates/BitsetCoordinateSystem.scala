package io.ginger.kdtrey5.coordinates

import java.util.BitSet

/**
 * An arbitrary-length binary (biset-based) coordinate system.
 *
 * e.g. points can be expressed as `011011101`, `10111011`, etc.
 *
 * Distance is defined as the Hamming Distance (https://en.wikipedia.org/wiki/Hamming_distance) between two points.
 */
object BitsetCoordinateSystem extends CoordinateSystem {
  override type POINT = BitsetPoint

  override val ordering: Ordering[BitsetPoint] = new Ordering[BitsetPoint] {
    override def compare(x: BitsetPoint, y: BitsetPoint): Int = {
      if (x.length != y.length) throw new Exception("Cannot compare points of different lengths")
      var pos = x.length - 1 // start with the most significant bit
      while (pos >= 0) {
        val xbit = x.bits.get(pos)
        val ybit = y.bits.get(pos)
        if (xbit && !ybit) return 1
        if (!xbit && ybit) return -1
        pos -= 1
      }
      return 0
    }
  }

  case class BitsetPoint(val bits: BitSet, val length: Int) {
    override def toString = {
      s"BitsetPoint(${asBinaryString})"
    }

    def asBinaryString: String = {
      val sb = new StringBuilder()
      var pos = length - 1
      while (pos >= 0) {
        val ch = if (bits.get(pos)) '1' else '0'
        sb.append(ch)
        pos -= 1
      }
      sb.toString
    }

    def asByteArray: Array[Byte] = {
      bits.toByteArray
    }
  }

  object BitsetPoint {
    implicit val ordering: Ordering[BitsetPoint] = BitsetCoordinateSystem.ordering
  }

  override def distance(p1: BitsetPoint, p2: BitsetPoint): Distance = {
    val xored = BitsetCoordinateSystem.clone(p1.bits)
    xored.xor(p2.bits)
    xored.cardinality()
  }


  /** Create a BitsetPoint from a string, e.g. "01100111" */
  def pointFromBinaryString(bits: String): BitsetPoint = {
    BitsetPoint(bitSetFromString(bits), bits.length)
  }

  /** Create a BitsetPoint from a `BitSet` */
  def pointFromBitSet(bits: BitSet, len: Int): BitsetPoint = {
    BitsetPoint(bits, len)
  }

  /** Create a BitsetPoint from a byte array */
  def pointFromByteArray(bytes: Array[Byte], bits: Int): BitsetPoint = {
    BitsetPoint(BitSet.valueOf(bytes), bits)
  }

  private[BitsetCoordinateSystem] def clone(bs: BitSet): BitSet = {
    // BitSet in Java forces use of mutation so need to clone often ...
    bs.clone.asInstanceOf[BitSet]
  }

  // note: unoptimized
  def bitSetFromString(binary: String): BitSet = {
    val bs = new BitSet()
    binary.reverse.zipWithIndex foreach {
      case (bit, pos) =>
        bit match {
          case '0' => // false by default
          case '1' => bs.set(pos, true)
          case _   => throw new Exception(s"illegal value for bits: $binary")
        }
    }
    bs
  }

  override def minDistance(target: POINT, rangeMin: POINT, rangeMax: POINT): Distance = {
    import BitsetPoint.ordering._

    if (gteq(target, rangeMin) && lteq(target, rangeMax)) {
      0.0d
    } else {
      var prefixDiff = 0.0d
      var continue = true
      var i = target.length - 1
      while (continue) {
        val tg = target.bits.get(i)
        val min = rangeMin.bits.get(i)
        val max = rangeMax.bits.get(i)
        if (min == max) {
          if (tg != min) prefixDiff += 1.0d
        } else {
          continue = false
        }
        i -= 1
        continue &&= (i >= 0)
      }
      math.min(1.0d + prefixDiff, math.min(distance(target, rangeMin), distance(target, rangeMax)))
    }
  }
}
