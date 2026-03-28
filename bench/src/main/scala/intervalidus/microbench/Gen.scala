package intervalidus.microbench

import scala.util.Random

object RandomNumbers:
  def withSeed(seed: Long): RandomNumbers = new RandomNumbers(Random(seed))

class RandomNumbers(random: Random):
  def double(): Double = random.nextDouble()

  // from min to max inclusive
  def int(min: Int, max: Int): Int = random.nextInt(max - min + 1) + min

/* emulate random value generation done in ScalaCheck (but not as well) */

object Gen:
  def choose(min: Int, max: Int)(using random: RandomNumbers): Gen[Int] = Gen:
    Iterator.continually(random.int(min, max))

  def const[T](x: T): Gen[T] = Gen:
    Iterator.continually(x)

  def listOfN[T](size: Int, gen: Gen[T]): Gen[List[T]] = Gen:
    gen.iterator.grouped(size).map(_.toList)

  def someOf[T](shards: Iterable[T], atLeast: Double = 0.0)(using random: RandomNumbers): Gen[Iterable[T]] = Gen:
    Iterator.continually:
      val factor = random.double() // how much to keep
      shards.filter(_ => random.double() - atLeast < factor)

  def oneOf[T](shards: IndexedSeq[T])(using random: RandomNumbers): Gen[T] = Gen:
    Iterator.continually:
      shards(random.int(0, shards.size - 1))

  def frequency[T](gs: (Int, Gen[T])*)(using random: RandomNumbers): Gen[T] =
    val weightTailSums = gs.map(_._1).tails.map(_.sum).toList
    val totalWeight = weightTailSums.head
    val thresholds = weightTailSums.tail.map(sum => 1.0 - (sum.toDouble / totalWeight)).zip(gs.map(_._2))
    val it = Iterator.unfold(()): _ =>
      for
        (_, g) <- thresholds.find(_._1 > random.double())
        t <- g.iterator.nextOption()
      yield (t, ())
    Gen(it)

  def zip[T1, T2](g1: Gen[T1], g2: Gen[T2]): Gen[(T1, T2)] = Gen:
    g1.iterator.zip(g2.iterator)

  def zip[T1, T2, T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): Gen[(T1, T2, T3)] = Gen:
    zip(g1, g2).iterator
      .zip(g3.iterator)
      .map:
        case ((t1, t2), t3) => (t1, t2, t3)

  def zip[T1, T2, T3, T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]): Gen[(T1, T2, T3, T4)] = Gen:
    zip(g1, g2, g3).iterator
      .zip(g4.iterator)
      .map:
        case ((t1, t2, t3), t4) => (t1, t2, t3, t4)

  def zip[T1, T2, T3, T4, T5](
    g1: Gen[T1],
    g2: Gen[T2],
    g3: Gen[T3],
    g4: Gen[T4],
    g5: Gen[T5]
  ): Gen[(T1, T2, T3, T4, T5)] = Gen:
    zip(g1, g2, g3, g4).iterator
      .zip(g5.iterator)
      .map:
        case ((t1, t2, t3, t4), t5) => (t1, t2, t3, t4, t5)

// a thin wrapper around Iterator
class Gen[T](val iterator: Iterator[T]):
  def map[U](f: T => U): Gen[U] = Gen:
    iterator.map(f(_))

  // take(1) here prevents having a Gen that returns the first element of underlying forever
  def flatMap[U](f: T => Gen[U]): Gen[U] = Gen:
    iterator.flatMap(f(_).iterator.take(1))
