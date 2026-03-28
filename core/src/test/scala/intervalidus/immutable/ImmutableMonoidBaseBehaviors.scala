package intervalidus.immutable

import intervalidus.*
import intervalidus.DomainLike.given
import intervalidus.Interval1D.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Test behaviors that do not differ between discrete or continuous interval-based shapes.
  */
trait ImmutableMonoidBaseBehaviors(using DomainValueLike[Int]):
  this: AnyFunSuite & Matchers =>

  type Dim = Domain.In2D[Int, Int]

  // assert equivalence/non-equivalence
  extension [V: Monoid, D <: NonEmptyTuple: DomainLike](lhs: DataMonoid[V, D])
    infix def ≡≡(rhs: DataMonoid[V, D]): Assertion = assert(lhs ≡ rhs, s"\nExpected: $lhs\nActual: $rhs\n")
    infix def !≡(rhs: DataMonoid[V, D]): Assertion = assert(!(lhs ≡ rhs))

  val origin: Dim = Domain.in2D[Int, Int](0, 0)
  val quadrantOneSample: Dim = Domain.in2D[Int, Int](5, 5)
  val quadrantTwoSample: Dim = Domain.in2D[Int, Int](-5, 5)
  val quadrantThreeSample: Dim = Domain.in2D[Int, Int](-5, -5)
  val quadrantFourSample: Dim = Domain.in2D[Int, Int](5, -5)

  val quadrantSamples: List[(Dim, Int)] = List(
    quadrantOneSample -> 1,
    quadrantTwoSample -> 2,
    quadrantThreeSample -> 3,
    quadrantFourSample -> 4
  )

  val fromOrigin: Interval1D[Int] = intervalFrom(0)
  val toBeforeOrigin: Interval1D[Int] = intervalToBefore(0)
  val toOrigin: Interval1D[Int] = intervalTo(0)
  val fromAfterOrigin: Interval1D[Int] = intervalFromAfter(0)

  // Some methods (e.g., flatMap) return a different type. This helps us convert back
  extension [T: Monoid](data: DimensionalBase[T, Dim]) def toDataMonoid: DataMonoid[T, Dim] = DataMonoid(data.getAll)

  def commonBehaviors(prefix: String): Unit =
    import DataMonoid.*
    import Monoid.given

    test(s"$prefix: Int DataMonoid basics"):

      // Ints are monoids
      val mInt = summon[Monoid[Int]]
      mInt.identity shouldBe 0
      mInt.combine(1, 1) shouldBe 2
      (1 combineWith 2) combineWith 3 shouldBe (1 combineWith (2 combineWith 3))

      // Units are monoids
      val mUnit = summon[Monoid[Unit]]
      mUnit.identity shouldBe ()
      mUnit.combine((), ()) shouldBe ()

      // Option[String] can be a monoid
      given Semigroup[String] with
        override def combine(lhs: String, rhs: String): String = s"$lhs $rhs"
      val mString = summon[Monoid[Option[String]]]
      mString.identity shouldBe None
      mString.combine(None, None) shouldBe None
      mString.combine(Some("Hello"), None) shouldBe Some("Hello")
      mString.combine(None, Some("World")) shouldBe Some("World")
      Some("Hello") combineWith Some("World") shouldBe Some("Hello World")
      (Some("a") combineWith Some("b")) combineWith Some("c") shouldBe
        (Some("a") combineWith (Some("b") combineWith Some("c")))

      val withoutQuadrantOne = DataMonoid( // origin missing (would be in I)
        Seq(
          (toBeforeOrigin x toBeforeOrigin) -> 3, // III
          (toBeforeOrigin x fromOrigin) -> 2 // II
        )
      ) + ((fromOrigin x toBeforeOrigin) -> 4) // IV

      // Longs are monoids
      val mLong = summon[Monoid[Long]]
      mLong.identity shouldBe 0L
      mLong.combine(1L, 1L) shouldBe 2L

      val withoutQuadrantTwo = DataMonoid.of( // origin present (in III)
        (fromAfterOrigin x fromAfterOrigin) -> 1L // I
      ) ++ Seq(
        (toOrigin x toOrigin) -> 3L, // III
        (fromAfterOrigin x toOrigin) -> 4L // IV
      )

      withoutQuadrantOne.copy == withoutQuadrantOne shouldBe true
      withoutQuadrantOne.toImmutable ≡≡ withoutQuadrantOne.toMutable.toImmutable
      withoutQuadrantOne.recompressAll() == withoutQuadrantOne shouldBe true
      withoutQuadrantOne.recompressAll(withoutQuadrantTwo.allIntervals) ≡≡ withoutQuadrantOne

      withoutQuadrantOne == ("bogus": Any) shouldBe false // different types
      withoutQuadrantOne == withoutQuadrantTwo shouldBe false // different quadrants missing
      withoutQuadrantOne !≡ withoutQuadrantTwo.mapValues(_.toInt).toDataMonoid // different quadrants missing

      val withoutOneQuadrantCount = withoutQuadrantOne.foldLeft(0): (acc, data) =>
        acc + quadrantSamples.count(sample => sample._1 ∈ data.interval && sample._2 == data.value)
      withoutOneQuadrantCount shouldBe 3
      quadrantSamples.count(sample => withoutQuadrantOne.isDefinedAt(sample._1)) shouldBe 3

      withoutQuadrantOne.size shouldBe 3
      Interval.compress(withoutQuadrantOne.allIntervals).toList shouldBe List(
        unbounded x toBeforeOrigin, // III & IV merged (horizontally)
        toBeforeOrigin x fromOrigin // II
      )

      val yQuadrantFour: DataMonoid[Int, Domain.In1D[Int]] = withoutQuadrantOne.getByHeadDimension(quadrantOneSample._1)
      yQuadrantFour.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        toBeforeOrigin // IV, below I
      )
      withoutQuadrantOne.isDefinedAt(origin) shouldBe false
      withoutQuadrantOne.isDefinedAt(quadrantOneSample) shouldBe false
      withoutQuadrantOne.isDefinedAt(quadrantTwoSample) shouldBe true
      withoutQuadrantOne.isDefinedAt(quadrantThreeSample) shouldBe true
      withoutQuadrantOne.isDefinedAt(quadrantFourSample) shouldBe true

      val withoutTwoQuadrantCount = withoutQuadrantTwo.foldLeft(0): (acc, data) =>
        acc + quadrantSamples.count(sample => sample._1 ∈ data.interval && sample._2 == data.value)
      withoutTwoQuadrantCount shouldBe 3
      quadrantSamples.count(sample => withoutQuadrantTwo.isDefinedAt(sample._1)) shouldBe 3

      withoutQuadrantTwo.size shouldBe 3
      Interval.compress(withoutQuadrantTwo.allIntervals).toList shouldBe List(
        unbounded x toOrigin, // III & IV merged (horizontally)
        fromAfterOrigin x fromAfterOrigin // I
      )
      val yQuadrantOne: DataMonoid[Long, Domain.In1D[Int]] = withoutQuadrantTwo.getByDimension(1, quadrantTwoSample._2)
      yQuadrantOne.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        fromAfterOrigin // I, to the right of II
      )
      withoutQuadrantTwo.isDefinedAt(origin) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantOneSample) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantTwoSample) shouldBe false
      withoutQuadrantTwo.isDefinedAt(quadrantThreeSample) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantFourSample) shouldBe true

      // Unit is a monoid
      val withoutQuadrantOneDouble = DataMonoid(withoutQuadrantOne.mapValues(_ => ()).getAll)
      val withoutQuadrantTwoDouble = DataMonoid(withoutQuadrantTwo.mapValues(_ => ()).getAll)

      // setIfNoConflict ensures no overlaps, subsequent checks ensure no gaps
      val complete1 = withoutQuadrantOneDouble.setIfNoConflict((fromOrigin x fromOrigin) -> ()) // I
      val complete2 = withoutQuadrantTwoDouble.setIfNoConflict((toOrigin x fromAfterOrigin) -> ()) // II
      (complete1, complete2) match
        case (Some(c1), Some(c2)) =>
          c1 ≡≡ c2
          c1 ≡≡ ξ
        case _ =>
          fail(s"expected add to succeed in $complete1 and $complete2")

      // Doubles are monoids
      val empty = DataMonoid.newBuilder[Double, Dim].result()
      empty.isEmpty shouldBe true
      empty ≡≡ ∅
      empty.toString shouldBe "<nothing is valid>"

      val buildUniverse = DataMonoid.newBuilder[Double, Dim]
      buildUniverse.addOne((unbounded x toOrigin) -> 1.0)
      buildUniverse.clear()
      buildUniverse.addOne((unbounded x unbounded) -> 2.0)
      val full = buildUniverse.result()
      full(Domain.in2D(1, 1)) shouldBe 2.0

    test(s"$prefix: Int DataMonoid 2D collection operations"):
      /* When discrete, the donut (intervals a, b, c, and d) and, its complement, the hole (interval e) look like:

         +∞    b  b  b  c  c  c  c  c  c
         ..    b  b  b  c  c  c  c  c  c
          2    b  b  b  c  c  c  c  c  c
          1    b  b  b  e  e  e  d  d  d
          0    b  b  b  e  e  e  d  d  d
         -1    b  b  b  e  e  e  d  d  d
         -2    a  a  a  a  a  a  d  d  d
         ..    a  a  a  a  a  a  d  d  d
         -∞    a  a  a  a  a  a  d  d  d

              -∞ .. -2 -1  0  1  2 .. +∞

       */
      val a = intervalTo(1) x intervalToBefore(-1)
      val b = intervalToBefore(-1) x intervalFrom(-1)
      val c = intervalFrom(-1) x intervalFromAfter(1)
      val d = intervalFromAfter(1) x intervalTo(1)
      val e = interval(-1, 1) x interval(-1, 1)

      val donutFilling = 3.0
      val holeFilling = 7.0

      val donut = DataMonoid(Seq(a, b, c, d).map(_ -> donutFilling)) // no e
      val hole = DataMonoid.of(e -> holeFilling)

      extension [T](data: DataMonoid[T, Dim])(using monoid: Monoid[T])
        private def filledWith(v: T): DataMonoid[T, Dim] = DataMonoid(data.mapValues(_ => v).getAll)
        private def filledWithIdentity: DataMonoid[T, Dim] = DataMonoid(data.mapValues(_ => monoid.identity).getAll)

      extension (intervals: Seq[Interval[Dim]])
        private def valueFilled[T: Monoid](v: T) = DataMonoid(intervals.map(_ -> v))
        private def identityFilled[T](using m: Monoid[T]) = intervals.valueFilled(m.identity)
        private def donutFilled = intervals.valueFilled(donutFilling)
        private def holeFilled = intervals.valueFilled(holeFilling)

      donut.filledWithIdentity ≡≡ (ξ - e)
      donut.filledWithIdentity ≡≡ (ξ \ hole)
      donut.filledWithIdentity ≡≡ hole.c
      donut ≡≡ (ξ[Double, Dim].filledWith(donutFilling) - e)
      donut ≡≡ (ξ[Double, Dim].filledWith(donutFilling) \ hole)
      donut ≡≡ (∅[Double, Dim] ++ Seq(a, b, c, d).map(_ -> donutFilling))
      donut ≡≡ hole.c.filledWith(donutFilling)

      hole.filledWithIdentity ≡≡ (ξ -- Seq(a, b, c, d))
      hole.filledWithIdentity ≡≡ (ξ \ donut)
      hole.filledWithIdentity ≡≡ donut.c
      hole ≡≡ (ξ[Double, Dim].filledWith(holeFilling) -- Seq(a, b, c, d))
      hole ≡≡ (ξ[Double, Dim].filledWith(holeFilling) \ donut)
      hole ≡≡ (∅[Double, Dim] + (e -> holeFilling))
      hole ≡≡ donut.c.filledWith(holeFilling)

      donut ∩ hole ≡≡ ∅
      (donut ∪ hole).filledWithIdentity ≡≡ ξ
      (donut △ hole).filledWithIdentity ≡≡ ξ
      donut ∪ hole !≡ ξ[Double, Dim].filledWith(holeFilling) // because a-d have donut filling
      donut △ hole !≡ ξ[Double, Dim].filledWith(donutFilling) // because e has hole filling

      (Seq(a, b).donutFilled ∪ Seq(c, d).donutFilled) ≡≡ donut
      (Seq(a, b).donutFilled ∩ Seq(c, d).donutFilled) ≡≡ ∅
      (Seq(a, e).donutFilled ∩ Seq(e, d).donutFilled) ≡≡ Seq(e).valueFilled(donutFilling * 2)
      (Seq(a, e).holeFilled \ Seq(a).holeFilled) ≡≡ hole

      val x1 = Seq(a, b, c).donutFilled
      val y1 = Seq(c, d, e).holeFilled
      x1.c ≡≡ Seq(d, e).identityFilled
      y1.c ≡≡ Seq(a, b).identityFilled
      (x1 ∪ y1) ≡≡ (
        Seq(a, b).donutFilled ++ Seq(d, e).holeFilled.getAll ++ Seq(c).valueFilled(donutFilling + holeFilling).getAll
      )
      x1.zip(y1) ≡ Data(
        Seq(c -> (donutFilling, holeFilling))
      ) shouldBe true
      x1.zipAll(y1, -1.0, -1.0) ≡ Data(
        Seq(
          a -> (donutFilling, -1.0),
          b -> (donutFilling, -1.0),
          c -> (donutFilling, holeFilling),
          d -> (-1.0, holeFilling),
          e -> (-1.0, holeFilling)
        )
      ) shouldBe true

      (x1 ∩ y1) ≡≡ Seq(c).valueFilled(donutFilling + holeFilling)
      (x1 \ y1) ≡≡ Seq(a, b).donutFilled
      (y1 \ x1) ≡≡ Seq(d, e).holeFilled
      (x1 △ y1) ≡≡ (Seq(a, b).donutFilled ++ Seq(d, e).holeFilled.getAll)
      (y1 ⊆ x1) shouldBe false

      val x2 = Seq(a, b, c).donutFilled
      val y2 = Seq(a, b).donutFilled
      x2.c ≡≡ Seq(d, e).identityFilled
      y2.c ≡≡ Seq(c, d, e).identityFilled
      (x2 ∪ y2) ≡≡ (y2.filledWith(donutFilling * 2) ++ Seq(c).donutFilled.getAll)
      (x2 ∩ y2) ≡≡ y2.filledWith(donutFilling * 2)
      (x2 \ y2) ≡≡ Seq(c).donutFilled
      (y2 \ x2) ≡≡ ∅
      (x2 △ y2) ≡≡ Seq(c).donutFilled
      (y2 ⊆ x2) shouldBe true

      val clipInterval = interval(-10, 10) x interval(-10, 10)
      donut intersects clipInterval shouldBe true
      val clippedDonut = Seq(a, b, c, d).flatMap(_ ∩ clipInterval).donutFilled
      (donut ∩ clipInterval) ≡≡ clippedDonut
      clippedDonut ≡≡ clippedDonut.mapIntervals(_.swapDimensions[Dim](0, 1))

      val flatMapped = donut.flatMap: v =>
        (v.interval ∩ clipInterval).toSeq.donutFilled
      clippedDonut ≡≡ flatMapped.toDataMonoid

      val filtered = clippedDonut.filter(_.interval.start > Domain.in2D(0, 0))
      Seq(intervalFromAfter(1).to(10) x interval(-10, 1)).donutFilled ≡≡ filtered

      val collected = clippedDonut.collect:
        case x if x.interval.start > Domain.in2D(0, 0) => x.copy(interval = x.interval.swapDimensions[Dim](0, 1))
      Seq(interval(-10, 1) x intervalFromAfter(1).to(10)).donutFilled ≡≡ collected.toDataMonoid
