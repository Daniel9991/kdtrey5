package io.ginger.kdtrey5

object Utils {

  /** A less frustrating version of `Arrays.arraysEquals` due to Java <-> Scala type system impedance.
   *  Also allows providing a `size` parameter for our own convenience.
   */
  private[kdtrey5] def arrayEquals[T](a1: Array[T], a2: Array[T], size: Int): Boolean = {
    var i = 0
    while (i < size) {
      if (a1(i) != a2(i)) return false
      i += 1
    }
    return true
  }
}
