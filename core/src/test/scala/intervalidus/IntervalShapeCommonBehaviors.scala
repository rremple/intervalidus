package intervalidus

import intervalidus.*
import intervalidus.Interval1D.*
import intervalidus.IntervalShape.*
import intervalidus.DomainLike.given

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Test behaviors that do not differ between discrete or continuous interval-based shapes.
  */
trait IntervalShapeCommonBehaviors(using DomainValueLike[Int]):
  this: AnyFunSuite & Matchers =>

  type Dim = Domain.In2D[Int, Int]

  // assert equivalence/non-equivalence
  extension [D <: NonEmptyTuple: DomainLike](lhs: IntervalShape[D])
    infix def ≡≡(rhs: IntervalShape[D]): Assertion = assert(lhs ≡ rhs, s"\nExpected: $lhs\nActual: $rhs\n")
    infix def !≡(rhs: IntervalShape[D]): Assertion = assert(!(lhs ≡ rhs))

  val origin: Dim = Domain.in2D[Int, Int](0, 0)
  val quadrantOneSample: Dim = Domain.in2D[Int, Int](5, 5)
  val quadrantTwoSample: Dim = Domain.in2D[Int, Int](-5, 5)
  val quadrantThreeSample: Dim = Domain.in2D[Int, Int](-5, -5)
  val quadrantFourSample: Dim = Domain.in2D[Int, Int](5, -5)

  val quadrantSamples: List[Dim] = List(quadrantOneSample, quadrantTwoSample, quadrantThreeSample, quadrantFourSample)

  val fromOrigin: Interval1D[Int] = intervalFrom(0)
  val toBeforeOrigin: Interval1D[Int] = intervalToBefore(0)
  val toOrigin: Interval1D[Int] = intervalTo(0)
  val fromAfterOrigin: Interval1D[Int] = intervalFromAfter(0)

  def commonBehaviors(prefix: String): Unit =

    test(s"$prefix: Int IntervalShape basics"):
      assertThrows[IllegalArgumentException]: // not disjoint
        val _ = IntervalShape( // origin in both I and II
          Seq(
            fromOrigin x fromOrigin, // I
            toOrigin x fromOrigin // II
          )
        )

      val withoutQuadrantOne = IntervalShape( // origin missing (would be in I)
        Seq(
          toBeforeOrigin x toBeforeOrigin, // III
          toBeforeOrigin x fromOrigin // II
        )
      ) + (fromOrigin x toBeforeOrigin) // IV

      val withoutQuadrantOneNoCheck = IntervalShape.withoutChecks( // origin missing (would be in I)
        Seq(
          toBeforeOrigin x toBeforeOrigin, // III
          toBeforeOrigin x fromOrigin, // II
          fromOrigin x toBeforeOrigin // IV
        )
      )

      val withoutQuadrantTwo = IntervalShape.of( // origin present (in III)
        fromAfterOrigin x fromAfterOrigin // I
      ) ++ Seq(
        toOrigin x toOrigin, // III
        fromAfterOrigin x toOrigin // IV
      )

      withoutQuadrantOne.copy == withoutQuadrantOne shouldBe true
      withoutQuadrantOne.recompressAll() == withoutQuadrantOne shouldBe false
      withoutQuadrantOneNoCheck ≡≡ withoutQuadrantOne
      withoutQuadrantOne.recompressAll(withoutQuadrantTwo) ≡≡ withoutQuadrantOne

      withoutQuadrantOne == ("bogus": Any) shouldBe false // different types
      withoutQuadrantOne == withoutQuadrantTwo shouldBe false // different quadrants missing
      withoutQuadrantOne !≡ withoutQuadrantTwo // different quadrants missing

      Interval.intervalFrom(quadrantOneSample) ∈ withoutQuadrantOne shouldBe false
      val withoutOneQuadrantCount = withoutQuadrantOne.foldLeft(0): (acc, intervalComponent) =>
        acc + quadrantSamples.count(_ ∈ intervalComponent)
      withoutOneQuadrantCount shouldBe 3
      quadrantSamples.count(withoutQuadrantOne.contains) shouldBe 3

      withoutQuadrantOne.size shouldBe 2
      withoutQuadrantOne.allIntervals shouldBe Seq(
        toBeforeOrigin x unbounded, // II & III merged (vertically)
        fromOrigin x toBeforeOrigin // IV
      )

      val withoutOneNoCheckCount = withoutQuadrantOneNoCheck.foldLeft(0): (acc, intervalComponent) =>
        acc + quadrantSamples.count(_ ∈ intervalComponent)
      withoutOneNoCheckCount shouldBe 3
      quadrantSamples.count(withoutQuadrantOneNoCheck.contains) shouldBe 3

      withoutQuadrantOneNoCheck.size shouldBe 3 // uncompressed
      withoutQuadrantOneNoCheck.allIntervals shouldBe Seq(
        toBeforeOrigin x toBeforeOrigin, // III
        toBeforeOrigin x fromOrigin, // II
        fromOrigin x toBeforeOrigin // IV
      )

      val yQuadrantFour: IntervalShape[Domain.In1D[Int]] = withoutQuadrantOne.getByHeadDimension(quadrantOneSample._1)
      yQuadrantFour.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        toBeforeOrigin // IV, below I
      )
      origin ∈ withoutQuadrantOne shouldBe false
      withoutQuadrantOne contains quadrantOneSample shouldBe false
      withoutQuadrantOne contains quadrantTwoSample shouldBe true
      withoutQuadrantOne contains quadrantThreeSample shouldBe true
      withoutQuadrantOne contains quadrantFourSample shouldBe true

      Interval.intervalFrom(quadrantOneSample) ∈ withoutQuadrantTwo shouldBe true
      val withoutTwoQuadrantCount = withoutQuadrantTwo.foldLeft(0): (acc, intervalComponent) =>
        acc + quadrantSamples.count(_ ∈ intervalComponent)
      withoutTwoQuadrantCount shouldBe 3
      quadrantSamples.count(withoutQuadrantTwo.contains) shouldBe 3

      withoutQuadrantTwo.size shouldBe 2
      withoutQuadrantTwo.allIntervals shouldBe Seq(
        unbounded x toOrigin, // III & IV merged (horizontally)
        fromAfterOrigin x fromAfterOrigin // I
      )
      val yQuadrantOne: IntervalShape[Domain.In1D[Int]] = withoutQuadrantTwo.getByDimension(1, quadrantTwoSample._2)
      yQuadrantOne.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        fromAfterOrigin // I, to the right of II
      )
      origin ∈ withoutQuadrantTwo shouldBe true
      withoutQuadrantTwo contains quadrantOneSample shouldBe true
      withoutQuadrantTwo contains quadrantTwoSample shouldBe false
      withoutQuadrantTwo contains quadrantThreeSample shouldBe true
      withoutQuadrantTwo contains quadrantFourSample shouldBe true

      // addIfNoConflict ensures no overlaps, subsequent checks ensure no gaps
      val complete1 = withoutQuadrantOne.addIfNoConflict(fromOrigin x fromOrigin) // I
      val complete2 = withoutQuadrantTwo.addIfNoConflict(toOrigin x fromAfterOrigin) // II
      (complete1, complete2) match
        case (Some(c1), Some(c2)) =>
          c1 ≡≡ c2
          c1 ≡≡ ξ
          c1.isUniverse shouldBe true
        case _ =>
          fail(s"expected add to succeed in $complete1 and $complete2")

      val empty = IntervalShape.newBuilder[Dim].result()
      empty.isEmpty shouldBe true
      empty ≡≡ ∅
      empty.toString shouldBe "∅"
      empty.toCodeLikeString shouldBe "∅"

      val buildUniverse = IntervalShape.newBuilder[Dim]
      buildUniverse.addOne(unbounded x toOrigin)
      buildUniverse.clear()
      buildUniverse.addOne(unbounded x unbounded)
      val full = buildUniverse.result()
      full.toString shouldBe "ξ"
      full.toCodeLikeString shouldBe "ξ"

    test(s"$prefix: Int IntervalShape 2D collection operations"):
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

      type Shape = IntervalShape.In2D[Int, Int]
      val donut: Shape = Seq(a, b, c, d).toShape // no e
      val hole: Shape = e // implicit conversion

      donut ≡≡ (ξ - e)
      donut ≡≡ (ξ \ hole)
      donut ≡≡ (∅ ++ Seq(a, b, c, d))
      donut ≡≡ hole.c

      hole ≡≡ (ξ -- Seq(a, b, c, d))
      hole ≡≡ (ξ \ donut)
      hole ≡≡ (∅ + e)
      hole ≡≡ donut.c

      donut ∪ hole ≡≡ ξ
      donut ∩ hole ≡≡ ∅
      donut △ hole ≡≡ ξ

      (Seq(a, b).toShape ∪ Seq(c, d).toShape) ≡≡ donut
      (Seq(a, b).toShape ∩ Seq(c, d).toShape) ≡≡ ∅
      (Seq(a, e).toShape \ a) ≡≡ hole

      val x1 = Seq(a, b, c).toShape
      val y1 = Seq(c, d, e).toShape
      x1.c ≡≡ Seq(d, e).toShape
      y1.c ≡≡ Seq(a, b).toShape
      (x1 ∪ y1) ≡≡ ξ
      (x1 ∩ y1) ≡≡ c
      (x1 \ y1) ≡≡ Seq(a, b).toShape
      (x1 △ y1) ≡≡ Seq(a, b, d, e).toShape
      (y1 ⊆ x1) shouldBe false

      val x2 = Seq(a, b, c).toShape
      val y2 = Seq(a, b).toShape
      x2.c ≡≡ Seq(d, e).toShape
      y2.c ≡≡ Seq(c, d, e).toShape
      (x2 ∪ y2) ≡≡ x2
      (x2 ∩ y2) ≡≡ y2
      (x2 \ y2) ≡≡ c
      (x2 △ y2) ≡≡ c
      (y2 ⊆ x2) shouldBe true

      val clipInterval = interval(-10, 10) x interval(-10, 10)
      donut intersects clipInterval shouldBe true
      val clippedDonut = Seq(a, b, c, d).flatMap(_ ∩ clipInterval).toShape
      (donut ∩ clipInterval) ≡≡ clippedDonut
      clippedDonut ≡≡ clippedDonut.map(_.swapDimensions[Dim](0, 1))
      clippedDonut ≡≡ donut.flatMap: i =>
        (i ∩ clipInterval).map(z => z: Shape).getOrElse(∅)
      (intervalFromAfter(1).to(10) x interval(-1, 1)) ≡≡ clippedDonut.filter(_.start > Domain.in2D(0, 0))
      (interval(-1, 1) x intervalFromAfter(1).to(10)) ≡≡ clippedDonut.collect:
        case i if i.start > Domain.in2D(0, 0) => i.swapDimensions[Dim](0, 1)

      val holeData = hole -> "hole"
      holeData.getAll.toList shouldBe List(e -> "hole")
      holeData shouldBe an[immutable.Data[String, Dim]]

      val holeDataMonoid = hole +> 1
      holeDataMonoid.getAll.toList shouldBe List(e -> 1)
      holeDataMonoid shouldBe an[immutable.DataMonoid[Int, Dim]]

      val donutData = donut.collectWithValue:
        case i => s"donut: ${i.start(0).isUnbounded}, ${i.end(0).isUnbounded}"
      donutData.getAll.toList shouldBe List(
        a.withDimensionUpdate[Int](0, _.toTop) -> "donut: true, true", // compressed to a + bottom half of d
        b -> "donut: true, false",
        c -> "donut: false, true",
        d.withDimensionUpdate[Int](1, _.from(-1)) -> "donut: false, true" // compressed to top half of d
      )

      val quadrant1 = fromOrigin x fromOrigin // I
      val quadrant2 = toOrigin x fromOrigin // II
      val quadrant3 = toBeforeOrigin x toBeforeOrigin // III
      val quadrant4 = fromOrigin x toBeforeOrigin // IV
      val quadrants = Seq(quadrant1, quadrant2, quadrant3, quadrant4)

      // Leaves out portions in quadrant IV
      val clippedDonutDataInQuadrants = clippedDonut.collectWithValue(alignTo = quadrants):
        case i if i ⊆ quadrant1 => "I"
        case i if i ⊆ quadrant2 => "II"
        case i if i ⊆ quadrant3 => "III"
      clippedDonutDataInQuadrants.compressAll().getAll.toList shouldBe List(
        (intervalFrom(-10).toBefore(0) x intervalFrom(-10).toBefore(-1)) -> "III",
        (intervalFrom(-10).toBefore(-1) x intervalFrom(-1).toBefore(0)) -> "III",
        (intervalFrom(-10).toBefore(-1) x interval(0, 10)) -> "II",
        (intervalFrom(-1).toBefore(0) x intervalFromAfter(1).to(10)) -> "II",
        (interval(0, 10) x intervalFromAfter(1).to(10)) -> "I",
        (intervalFromAfter(1).to(10) x interval(0, 1)) -> "I"
      )
