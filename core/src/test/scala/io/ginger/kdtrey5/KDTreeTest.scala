package io.ginger.kdtrey5

import io.ginger.kdtrey5.data._
import io.ginger.kdtrey5.mapreduce._
import io.ginger.kdtrey5.coordinates._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.reflect.ClassTag
import java.util.{BitSet, Random}
import skiis2.Skiis

class KDTreeTest extends AnyFunSuite {
  import BitsetCoordinateSystem._
  import VectorCoordinateSystem._

  implicit val skiisContext: Skiis.Context = new Skiis.Context {
    override final val parallelism = 1
    override final val queue = 100 // must be > KDTree fanout
    override final val batch = 1
    override final val shutdownExecutor = true
    override final lazy val executor = Skiis.newFixedThreadPool("KDTreeTest", threads = 1)
  }

  private def debug(s: String) = {
    if (true) {
      println(s)
    }
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

  def generateRandomPoints(dims: Int)(implicit random: Random): Point = {
    val bits = new BitSet()
    val array = new Array[VectorCoordinateSystem.Coordinate](dims)
    for (d <- 0 until dims) {
      array(d) = random.nextDouble()
    }
    Point(array)
  }


  def randomRange(low: Int, high: Int)(implicit random: Random): Int = {
    if (low == high) return low
    (math.abs(random.nextInt()) % (high - low)) + low
  }

  test("rangeFind() deterministic w/ bitset") {
    implicit val random = new Random(1339L)
    val points: Seq[(BitsetPoint, String)] =
      (1 to 14).map { i =>
        generateRandomBits(len = 6) -> s"value-$i"
      }
    val kddata = builder.build(InMemoryDataset(points), fanout = 2, BitsetCoordinateSystem)
    val kdstore = new InMemoryKVStore[BitsetPoint, String]()
    kddata.store(kdstore)
    val kdtree = new BitsetKDTree {
      override type V = String
      override val store: KVStore[K, V] = kdstore
    }
    val target = generateRandomBits(len = 6)
    debug("points")
    points foreach { p => debug(p.toString) }
    debug(s"target: $target")
    import BitsetCoordinateSystem.distance
    import BitsetCoordinateSystem.ordering.compare
    val results = kdtree.rangeFind(target, distance = 2)
    val actual = results.values.force()
    val expectedPoints = points
      .filter { case (p, value) => distance(p, target) <= 2 }
    actual.toSet shouldBe expectedPoints.toSet
    println(results.stats)
  }

  test("kNearestSearch() deterministic w/ bitset") {
    implicit val random = new Random(1339L)
    val points: Seq[(BitsetPoint, String)] =
      (1 to 14).map { i =>
        generateRandomBits(len = 16) -> s"value-$i"
      }
    val kddata = builder.build(InMemoryDataset(points), fanout = 2, BitsetCoordinateSystem)
    val kdstore = new InMemoryKVStore[BitsetPoint, String]()
    kddata.store(kdstore)
    val kdtree = new BitsetKDTree {
      override type V = String
      override val store: KVStore[K, V] = kdstore
    }
    val target = generateRandomBits(len = 16)
    debug("points")
    points foreach { p => debug(p.toString) }
    debug(s"target: $target")
    import BitsetCoordinateSystem.distance
    import BitsetCoordinateSystem.ordering.compare
    val results = kdtree.kNearestSearch(target, k = 3, maxDistance = Double.MaxValue)
    val actual = results.values.force()
    val expectedPoints = points
      // .map { case (p, value) => (distance(p, target), p) }
      .sortBy { case (p, value) => distance(p, target) }
      .take(3)
    debug("expectedPoints")
    expectedPoints foreach { p => debug(p.toString) }
    debug("actual")
    actual foreach { p => debug(p.toString) }
    actual.toSet shouldBe expectedPoints.toSet
    println(results.stats)
  }

  test("rangeFind() torture w/ bitset") {
    val randomSeed = new Random().nextLong()
    debug(s"randomSeed $randomSeed")
    implicit val random = new Random(randomSeed)
    var totalNodesCreated = 0L
    var totalNodesVisited = 0L
    var totalRangeFinds = 0L
    var totalResults = 0L
    for (i <- 1 to 10) {
      //debug(s"iteration $i")
      val bitlen = 128 // randomRange(96, 128)
      //debug(s"bitlen $bitlen")
      val fanout = 16 //randomRange(2, 2)
      //debug(s"fanout $fanout")
      val combinations = BigInt(2).pow(bitlen) //math.pow(2, bitlen).toLong
      debug(s"combinations ${combinations}")
      val points: Seq[(BitsetPoint, String)] =
        (1 to randomRange(164000, 228000))
          .map { i =>
            generateRandomBits(bitlen)
          }
          .sorted
          .zipWithIndex
          .map { case (p, index) => p -> s"value-$index" }
      debug(s"points.size ${points.size}")
      val kddata = builder.build(InMemoryDataset(points), fanout, BitsetCoordinateSystem)
      totalNodesCreated += kddata.nodesPerLevel.sum
      val kdstore = new InMemoryKVStore[BitsetPoint, String]()
      kddata.store(kdstore)
      val kdtree = new BitsetKDTree {
        override type V = String
        override val store: KVStore[K, V] = kdstore
      }

      val target = generateRandomBits(bitlen)
      //debug(s"target $target")

      val maxDistance = (1 /*randomRange(2, 6) */ )
      //debug(s"maxDistance $maxDistance")

      val rangeFindResult = kdtree.rangeFind(target, maxDistance)
      val result = rangeFindResult.values.force().sorted
      import BitsetCoordinateSystem.distance
      totalNodesVisited += (rangeFindResult.stats.branchesRetrieved + rangeFindResult.stats.leavesRetrieved)
      val expectedPoints = points.filter { case (p, value) => distance(p, target) <= maxDistance }.sorted
      if (result != expectedPoints) {
        val actual = result.toSet
        debug(s"points ${points.size}")
        points foreach { p =>
          //debug(p.toString)
        }
        debug(s"target: $target")
        debug(s"result ${result.size}")
        result foreach { p =>
          debug(p.toString)
        }
        debug(s"expected ${expectedPoints.size}")
        expectedPoints foreach { p =>
          debug(p.toString)
        }
        println("actual -- expected: " + (actual -- expectedPoints))
        println("expected -- actual: " + (expectedPoints.toSet -- actual))

        // for (i <- 0 until result.size) {
        //   result(i) shouldBe expectedPoints(i)
        // }
      }
      result shouldBe expectedPoints
      totalResults += result.size
      totalRangeFinds += 1
    }
    println(s"totalResults $totalResults")
    println(s"totalRangeFinds $totalRangeFinds")
    println(s"ratio ${totalResults.toDouble / totalRangeFinds.toDouble}}")
    println(s"totalNodesVisited $totalNodesVisited")
    println(s"totalNodesCreated $totalNodesCreated")
    println(f"ratio ${totalNodesVisited.toDouble / totalNodesCreated.toDouble}")
  }

  test("rangeFind() selectivity") {
    val points: Seq[(Point, String)] =
      for (x <- 1 to 5;
           y <- 1 to 5) yield (Point(Array(x, y)), (x, y).toString)
    val kddata = builder.build(InMemoryDataset(points), fanout = 2, VectorCoordinateSystem)
    kddata.nodesPerLevel shouldBe Seq(13, 7, 4, 2, 1)
    // println("nodes")
    // kddata.nodes.toSeq foreach println
    val kdstore = new InMemoryKVStore[Point, String]()
    kddata.store(kdstore)
    val kdtree = new VectorKDTree {
      override type V = String
      override val store: KVStore[K, V] = kdstore
    }
    val target = Point(Array(1, 2))
    val r2 = kdtree.rangeFind(target, distance = 2)
    val results = r2.values.force()
    import VectorCoordinateSystem.ordering.compare
    import VectorCoordinateSystem.distance
    val expectedPoints = points
      .filter { case (p, value) => distance(p, target) <= 2 }
    results.toSet shouldBe expectedPoints.toSet
    r2.stats.leavesRetrieved should be <= 7
    r2.stats.branchesRetrieved should be <= 8
  }

  test("rangeFind() torture w/ vector-coords") {
    val randomSeed = new Random().nextLong()
    debug(s"randomSeed $randomSeed")
    implicit val random = new Random(randomSeed)
    var totalNodesCreated = 0L
    var totalNodesVisited = 0L
    var totalRangeFinds = 0L
    var totalResults = 0L
    for (i <- 1 to 1) {
      val dims = 10
      val fanout = 32
      val points: Seq[(Point, String)] =
        (1 to randomRange(1000, 100000))
          .map { i =>
            generateRandomPoints(dims)
          }
          .sorted
          .zipWithIndex
          .map { case (p, index) => p -> s"value-$index" }
      debug(s"points.size ${points.size}")
      val kddata = builder.build(InMemoryDataset(points), fanout, VectorCoordinateSystem)
      totalNodesCreated += kddata.nodesPerLevel.sum
      val kdstore = new InMemoryKVStore[Point, String]()
      kddata.store(kdstore)
      val kdtree = new VectorKDTree {
        override type V = String
        override val store: KVStore[K, V] = kdstore
      }

      val target = generateRandomPoints(dims)

      val maxDistance = 0.001

      val rangeFindResult = kdtree.rangeFind(target, maxDistance)
      val result = rangeFindResult.values.force().sorted
      import VectorCoordinateSystem.distance
      totalNodesVisited += (rangeFindResult.stats.branchesRetrieved + rangeFindResult.stats.leavesRetrieved)
      val expectedPoints = points.filter { case (p, value) => distance(p, target) <= maxDistance }.sorted
      if (result != expectedPoints) {
        val actual = result.toSet
        debug(s"points ${points.size}")
        points foreach { p =>
          //debug(p.toString)
        }
        debug(s"target: $target")
        debug(s"result ${result.size}")
        result foreach { p =>
          debug(p.toString)
        }
        debug(s"expected ${expectedPoints.size}")
        expectedPoints foreach { p =>
          debug(p.toString)
        }
        println("actual -- expected: " + (actual -- expectedPoints))
        println("expected -- actual: " + (expectedPoints.toSet -- actual))

        // for (i <- 0 until result.size) {
        //   result(i) shouldBe expectedPoints(i)
        // }
      }
      result shouldBe expectedPoints
      totalResults += result.size
      totalRangeFinds += 1
    }
    println(s"totalResults $totalResults")
    println(s"totalRangeFinds $totalRangeFinds")
    println(s"ratio ${totalResults.toDouble / totalRangeFinds.toDouble}}")
    println(s"totalNodesVisited $totalNodesVisited")
    println(s"totalNodesCreated $totalNodesCreated")
    println(f"ratio ${totalNodesVisited.toDouble / totalNodesCreated.toDouble}")
  }

}
