package io.ginger.kdtrey5.coordinates

import java.util.BitSet

object BitsetCoordinateSystem extends CoordinateSystem {
  type DISTANCE = BitsetDistance
  type POINT = BitsetPoint
  type PLANE = BitsetPlane

  case class BitsetDistance(value: Long) extends super.Distance {
    override def compare(that: DISTANCE): Int = {
      if (this.value > that.value) 1
      else if (this.value == that.value) 0
      else -1
    }
  }

  implicit val bitsetOrdering = new Ordering[POINT] {
    override def compare(x: BitsetPoint, y: BitsetPoint): Int = {
      if (x.length != y.length) throw new Exception("Cannot compare points of different lengths")
      var pos = x.length - 1
      while (pos >= 0) {
        val xbit = x.bits.get(pos)
        val ybit = y.bits.get(pos)
        if ( xbit && !ybit) return  1
        if (!xbit &&  ybit) return -1
        pos -= 1
      }
      return 0
    }
  }

  case class BitsetPoint(val bits: BitSet, val length: Int) extends super.Point {
    override def toString = {
      val sb = new StringBuilder()
      var pos = length - 1
      while (pos >= 0) {
        val ch = if (bits.get(pos)) '1' else '0'
        sb.append(ch)
        pos -= 1
      }
      s"BitsetPoint($sb)"
    }
  }

  case class BitsetPlane(val bits: BitSet, val mask: BitSet, val length: Int) extends super.Plane {
    override def toString = {
      val sb = new StringBuilder()
      var pos = length - 1
      while (pos >= 0) {
        val ch = 
          if (mask.get(pos)) 'x'
          else if (bits.get(pos)) '1' 
          else '0'
        sb.append(ch)
        pos -= 1
      }
      s"BitsetPlane($sb)"
    }
  }

  override def distance(p1: POINT, p2: POINT): DISTANCE = {
    val xored = clone(p1.bits)
    xored.xor(p2.bits)
    BitsetDistance(xored.cardinality())
  }

  override def distance(p1: PLANE, p2: PLANE): DISTANCE = {
    var distance = 0L
    var pos = 0
    while (pos < math.max(p1.length, p2.length)) {
      (p1.mask.get(pos), p2.mask.get(pos)) match {
        case (false, false) =>
          if (p1.bits.get(pos) != p2.bits.get(pos)) distance += 1
        case (true,  false) => // could be the same
        case (false, true)  => // could be the same
        case (true,  true)  => // could be the same
      }
      pos += 1
    }
    BitsetDistance(distance)
  }

  override def distance(p1: POINT, p2: PLANE): DISTANCE = {
    if (p1.length != p2.length) throw new IllegalArgumentException("Both point and plane must have same length")
    var distance = 0L
    var pos = 0
    while (pos < p1.length) {
      p2.mask.get(pos) match {
        case false =>
          if (p1.bits.get(pos) != p2.bits.get(pos)) distance += 1
        case true => // could be the same
      }
      pos += 1
    }
    BitsetDistance(distance)
  }

  /** Create a BitsetPoint from a string, e.g. "01100111" */
  def pointFromBinaryString(bits: String): BitsetPoint = {
    BitsetPoint(bitSetFromString(bits), bits.length)
  }

  /** Create a BitsetPoint from a `BitSet` */
  def pointFromBitSet(bits: BitSet, len: Int): BitsetPoint = {
    BitsetPoint(bits, len)
  }

  /** Create a BitsetPlane from a bitmask string where 'x' represents an undefined dimension, 
   *  e.g. "011xx00x1"
   */
  def planeFromBinaryString(bitMask: String): BitsetPlane = {
    val bits = new BitSet()
    val mask = new BitSet()
    bitMask.reverse.zipWithIndex foreach { case (bit, pos) =>
      bit match {
        case '0' => // false by default
        case '1' => bits.set(pos, true)
        case 'x' => mask.set(pos, true)
        case _ => throw new Exception(s"illegal value: $bitMask")
      }
    }
    BitsetPlane(bits, mask, bitMask.length)
  }

  override def commonPrefixFromPoints(p1: POINT, p2: POINT): PLANE = {
    if (p1.length != p2.length) throw new IllegalArgumentException("Both points must have same length")
    val bits = new BitSet()
    val mask = new BitSet()
    var pos = p1.length - 1
    var matching = true
    while (pos >= 0) {
      if (matching && p1.bits.get(pos) == p2.bits.get(pos)) {
        bits.set(pos, p1.bits.get(pos))
      } else {
        matching = false
        mask.set(pos, true)
      }
      pos -= 1
    }
    BitsetPlane(bits, mask, p1.length)
  }

  private def clone(bs: BitSet): BitSet = {
    // BitSet in Java forces use of mutation so need to clone often ...
    bs.clone.asInstanceOf[BitSet]
  }

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
}