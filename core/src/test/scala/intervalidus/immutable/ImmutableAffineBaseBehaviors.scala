package intervalidus.immutable

import intervalidus.*
import intervalidus.Interval1D.*
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Test behaviors that do not differ between discrete or continuous interval-based shapes.
  */
trait ImmutableAffineBaseBehaviors(using DomainAffineValueLike[Int]):
  this: AnyFunSuite & Matchers & DataAffineBaseBehaviors =>

  val donut: DataAffine[Double, Dim] = DataAffine(Seq(aa, bb, cc, dd).map(_ -> donutFilling)) // no e
  val hole: DataAffine[Double, Dim] = DataAffine.of(ee -> holeFilling)
  override val clippedDonut: DataAffine[Double, Dim] = donut ∩ clipInterval

  extension [T](data: DataAffine[T, Dim]) private def filledWith(v: T): DataAffine[T, Dim] = data.mapValues(_ => v)

  extension (intervals: Seq[Interval[Dim]])
    private def valueFilled[T](v: T) = DataAffine(intervals.map(_ -> v))
    private def donutFilled = intervals.valueFilled(donutFilling)
    private def holeFilled = intervals.valueFilled(holeFilling)

  // donut-filled universe
  val ξ: DataAffine[Double, Dim] = DataAffine.of[Double, Dim](donutFilling)

  def commonBehaviors(prefix: String): Unit =
    import DataAffine.*

    test(s"$prefix: Int DataAffine operating"):

      val withoutQuadrantOne = DataAffine( // origin missing (would be in I)
        Seq(
          (toBeforeOrigin x toBeforeOrigin) -> 3, // III
          (toBeforeOrigin x fromOrigin) -> 2 // II
        )
      ) + ((fromOrigin x toBeforeOrigin) -> 4) // IV

      val withoutQuadrantTwo = DataAffine.of( // origin present (in III)
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
      withoutQuadrantOne !≡ withoutQuadrantTwo.mapValues(_.toInt).asDataAffine // different quadrants missing
      val f = (d: ValidData[Long, Dim]) => d.copy(value = d.value.toInt)
      withoutQuadrantTwo.map(f).asDataAffine ≡≡ withoutQuadrantTwo.mapValues(_.toInt).asDataAffine

      val fcv = withoutQuadrantOne.collectValues:
        case v if v < 4 => "I" * v
      fcv.getAll.toList shouldBe List(
        (toBeforeOrigin x toBeforeOrigin) -> "III",
        (toBeforeOrigin x fromOrigin) -> "II"
      )

      val fci = withoutQuadrantOne.collectIntervals:
        case i if i.contains(quadrantFourSample) => i.withDimensionUpdate[Int](1, _.to(0))
      fci.getAll.toList shouldBe List(
        (fromOrigin x toOrigin) -> 4
      )

      val withoutOneQuadrantCount = withoutQuadrantOne.foldLeft(0): (acc, data) =>
        acc + quadrantSamples.count(sample => data.interval.contains(sample._1) && sample._2 == data.value)
      withoutOneQuadrantCount shouldBe 3
      quadrantSamples.count(sample => withoutQuadrantOne.isDefinedAt(sample._1)) shouldBe 3

      withoutQuadrantOne.size shouldBe 3
      Interval.compress(withoutQuadrantOne.allIntervals).toList shouldBe List(
        unbounded x toBeforeOrigin, // III & IV merged (horizontally)
        toBeforeOrigin x fromOrigin // II
      )

      val yQuadrantFour: DataAffine[Int, Domain.In1D[Int]] = withoutQuadrantOne.getByHeadDimension(quadrantOneSample._1)
      yQuadrantFour.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        toBeforeOrigin // IV, below I
      )
      withoutQuadrantOne.isDefinedAt(origin) shouldBe false
      withoutQuadrantOne.isDefinedAt(quadrantOneSample) shouldBe false
      withoutQuadrantOne.isDefinedAt(quadrantTwoSample) shouldBe true
      withoutQuadrantOne.isDefinedAt(quadrantThreeSample) shouldBe true
      withoutQuadrantOne.isDefinedAt(quadrantFourSample) shouldBe true

      val withoutTwoQuadrantCount = withoutQuadrantTwo.foldLeft(0): (acc, data) =>
        acc + quadrantSamples.count(sample => data.interval.contains(sample._1) && sample._2 == data.value)
      withoutTwoQuadrantCount shouldBe 3
      quadrantSamples.count(sample => withoutQuadrantTwo.isDefinedAt(sample._1)) shouldBe 3

      withoutQuadrantTwo.size shouldBe 3
      Interval.compress(withoutQuadrantTwo.allIntervals).toList shouldBe List(
        unbounded x toOrigin, // III & IV merged (horizontally)
        fromAfterOrigin x fromAfterOrigin // I
      )
      val yQuadrantOne: DataAffine[Long, Domain.In1D[Int]] =
        withoutQuadrantTwo.getByDimension[Int, Domain.In1D[Int]](1, quadrantTwoSample._2)
      yQuadrantOne.allIntervals shouldBe Seq[Interval[Domain.In1D[Int]]](
        fromAfterOrigin // I, to the right of II
      )
      withoutQuadrantTwo.isDefinedAt(origin) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantOneSample) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantTwoSample) shouldBe false
      withoutQuadrantTwo.isDefinedAt(quadrantThreeSample) shouldBe true
      withoutQuadrantTwo.isDefinedAt(quadrantFourSample) shouldBe true

      val withoutQuadrantOneDouble = withoutQuadrantOne.mapValues(_ => ())
      val withoutQuadrantTwoDouble = withoutQuadrantTwo.mapValues(_ => ())

      // setIfNoConflict ensures no overlaps, subsequent checks ensure no gaps
      val complete1 = withoutQuadrantOneDouble.setIfNoConflict((fromOrigin x fromOrigin) -> ()) // I
      val complete2 = withoutQuadrantTwoDouble.setIfNoConflict((toOrigin x fromAfterOrigin) -> ()) // II
      (complete1, complete2) match
        case (Some(c1), Some(c2)) =>
          c1 ≡≡ c2
          c1 ≡≡ DataAffine.of[Unit, Dim](())
        case _ =>
          fail(s"expected add to succeed in $complete1 and $complete2")

      val empty = DataAffine.newBuilder[Double, Dim].result()
      empty.isEmpty shouldBe true
      empty ≡≡ ∅
      empty shouldBe DataAffine[Double, Dim]()
      empty.hashCode() shouldBe ∅[Double, Dim].hashCode()
      empty.toString shouldBe "<nothing is valid>"

      val buildUniverse = DataAffine.newBuilder[Double, Dim]
      buildUniverse.addOne((unbounded x toOrigin) -> 1.0)
      buildUniverse.clear()
      buildUniverse.addOne((unbounded x unbounded) -> 2.0)
      val full = buildUniverse.result()
      full(Domain.in2D(1, 1)) shouldBe 2.0

    test(s"$prefix: Int DataAffine collecting"):
      val donutFromData: DataAffine[Double, Dim] =
        Data(Seq(aa, bb, cc, dd).map(_ -> donutFilling)) // implicitly converted
      donutFromData ≡≡ donut

      donut ≡≡ (ξ - ee)
      donut ≡≡ (ξ \ hole)
      donut ≡≡ (ξ - ee)
      donut ≡≡ (ξ \ hole)
      donut ≡≡ (∅[Double, Dim] ++ Seq(aa, bb, cc, dd).map(_ -> donutFilling))
      assert(donut isSubsetOf ξ)
      assert(!donut.isEmpty)

      hole.filledWith(donutFilling) ≡≡ (ξ -- Seq(aa, bb, cc, dd))
      hole.filledWith(donutFilling) ≡≡ (ξ \ donut)
      hole ≡≡ (ξ -- Seq(aa, bb, cc, dd)).filledWith(holeFilling)
      hole ≡≡ (ξ \ donut).filledWith(holeFilling)
      hole ≡≡ (∅[Double, Dim] + (ee -> holeFilling))
      assert(hole isSubsetOf ξ)
      assert(!hole.isEmpty)

      (donut △ hole).filledWith(donutFilling) ≡≡ ξ
      donut △ hole !≡ ξ // because e has hole filling

      (Seq(aa, ee).holeFilled \ Seq(aa).holeFilled) ≡≡ hole

      val x1 = Seq(aa, bb, cc).donutFilled
      val y1 = Seq(cc, dd, ee).holeFilled
      x1.zip(y1) ≡ Data(
        Seq(cc -> (donutFilling, holeFilling))
      ) shouldBe true
      x1.zipAll(y1, -1.0, -1.0) ≡ Data(
        Seq(
          aa -> (donutFilling, -1.0),
          bb -> (donutFilling, -1.0),
          cc -> (donutFilling, holeFilling),
          dd -> (-1.0, holeFilling),
          ee -> (-1.0, holeFilling)
        )
      ) shouldBe true

      (x1 \ y1) ≡≡ Seq(aa, bb).donutFilled
      (y1 \ x1) ≡≡ Seq(dd, ee).holeFilled
      (x1 △ y1) ≡≡ (Seq(aa, bb).donutFilled ++ Seq(dd, ee).holeFilled.getAll)
      (y1 ⊆ x1) shouldBe false

      val x2 = Seq(aa, bb, cc).donutFilled
      val y2 = Seq(aa, bb).donutFilled
      (x2 \ y2) ≡≡ Seq(cc).donutFilled
      (y2 \ x2) ≡≡ ∅
      (x2 △ y2) ≡≡ Seq(cc).donutFilled
      (y2 ⊆ x2) shouldBe true

      donut intersects clipInterval shouldBe true
      clippedDonut ≡≡ Seq(aa, bb, cc, dd).flatMap(_ ∩ clipInterval).donutFilled
      clippedDonut ≡≡ clippedDonut.mapIntervals(_.swapDimensions[Dim](0, 1)).asDataAffine
      assert(clippedDonut isSubsetOf donut)

      val flatMapped = donut.flatMap: v =>
        (v.interval ∩ clipInterval).toSeq.donutFilled
      clippedDonut ≡≡ flatMapped.asDataAffine

      val filtered = clippedDonut.filter(_.interval.start > Domain.in2D(0, 0))
      Seq(intervalFromAfter(1).to(10) x interval(-10, 1)).donutFilled ≡≡ filtered

      val collected = clippedDonut.collect:
        case x if x.interval.start > Domain.in2D(0, 0) => x.copy(interval = x.interval.swapDimensions[Dim](0, 1))
      Seq(interval(-10, 1) x intervalFromAfter(1).to(10)).donutFilled ≡≡ collected.asDataAffine

      val donutIn3D: DataAffine[Double, Domain.In3D[Int, Int, Int]] =
        clippedDonut.extrudeDimension[Int, Domain.In3D[Int, Int, Int]](2, interval(-1, 1))
      val flattenedDonut: DataAffine[Double, Dim] = donutIn3D.collapseDimension[Dim](2, (takeFirst, _) => takeFirst)
      flattenedDonut shouldBe clippedDonut
