package io.ginger.kdtrey5

import io.ginger.kdtrey5.data._
import io.ginger.kdtrey5.mapreduce._
import io.ginger.kdtrey5.coordinates._

import org.scalatest._
import org.scalatest.Matchers._

import scala.reflect.ClassTag
import java.util.{BitSet, Random}


class KDTreeTest extends FunSuite {
  import BitsetCoordinateSystem._

  private def debug(s: String) = {
    println(s)
  }

  val builder = new KDTreeBuilder {
    override def newDataset[T](): Dataset[T] = InMemoryDataset(Iterable.empty)
  }

  def generateRandomBits(len: Int)(implicit random: Random): BitsetPoint = {
    val bits = new BitSet()
    for (pos <- 0 until len) {
      if (random.nextBoolean()) bits.set(pos)
    }
    pointFromBitSet(bits, len)
  }

  def randomRange(low: Int, high: Int)(implicit random: Random): Int = {
    (math.abs(random.nextInt()) % (high-low)) + low
  }

  test("deterministic") {
    implicit val random = new Random(1339L)
    val points: Seq[(BitsetPoint, String)] = 
      (1 to 14).map { i => generateRandomBits(len = 6) -> s"value-$i" }
    val kddata = builder.build(InMemoryDataset(points), fanout = 2)
    val kdstore = new InMemoryKVStore[BitsetPoint, String]()
    kddata.store(kdstore)
    val kdtree = new KDTree {
      override type K = BitsetPoint
      override type V = String
      override val coords = BitsetCoordinateSystem
      override val store: KVStore[K,V] = kdstore
    }
    val target = generateRandomBits(len = 6)
    /*
    debug("points")
    points foreach { p => debug(p.toString) }
    debug(s"target: $target")
    */
    val iter = kdtree.rangeFind(target, distance = BitsetDistance(2))
    val expectedPoints = points
      .filter { case (p, value) => distance(p, target) <= BitsetDistance(2) }
    iter.toSet shouldBe expectedPoints.toSet
  }

  test("torture") {
    val randomSeed = new Random().nextLong()
    debug(s"randomSeed $randomSeed")
    implicit val random = new Random(randomSeed)
    for (i <- 1 to 100000) {
      //debug(s"iteration $i")
      val bitlen = randomRange(4, 16)
      //debug(s"bitlen $bitlen")
      val fanout = randomRange(2, 6)
      //debug(s"fanout $fanout")
      val combinations = math.pow(bitlen, 2).toInt
      val points: Seq[(BitsetPoint, String)] = 
        (1 to randomRange(combinations / 2, combinations*3/2)).map { i => generateRandomBits(bitlen) }
          .sorted
          .zipWithIndex
          .map { case (p, index) => p -> s"value-$index" }
      //debug(s"points.size ${points.size}")
      val kddata = builder.build(InMemoryDataset(points), fanout)
      val kdstore = new InMemoryKVStore[BitsetPoint, String]()
      kddata.store(kdstore)
      val kdtree = new KDTree {
        override type K = BitsetPoint
        override type V = String
        override val coords = BitsetCoordinateSystem
        override val store: KVStore[K,V] = kdstore
      }

      val target = generateRandomBits(bitlen)
      //debug(s"target $target")

      val maxDistance = BitsetDistance(randomRange(1, bitlen-2))
      //debug(s"maxDistance $maxDistance")

      val result = kdtree.rangeFind(target, maxDistance).toList.sorted
      val expectedPoints = points
        .filter { case (p, value) => distance(p, target) <= maxDistance }
        .sorted
      if (result != expectedPoints) {
        debug(s"points ${points.size}")
        points foreach { p => debug(p.toString) }
        debug(s"target: $target")
        debug(s"result ${result.size}")
        result foreach { p => debug(p.toString) }
        debug(s"expected ${expectedPoints.size}")
        expectedPoints foreach { p => debug(p.toString) }
        for (i <- 0 until result.size) {
          result(i) shouldBe expectedPoints(i)
        }
      }
      result shouldBe expectedPoints
    }
  }
}