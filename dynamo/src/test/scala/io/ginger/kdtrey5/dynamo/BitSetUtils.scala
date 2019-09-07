package io.ginger.kdtrey5.dynamo

import java.util.BitSet

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