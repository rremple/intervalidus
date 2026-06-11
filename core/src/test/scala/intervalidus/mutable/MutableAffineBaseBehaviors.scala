package intervalidus.mutable

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
trait MutableAffineBaseBehaviors(using DomainAffineValueLike[Int]):
  this: AnyFunSuite & Matchers & DataAffineBaseBehaviors =>

  extension [T](data: DataAffine[T, Dim])
    def mutate(f: DataAffine[T, Dim] => Unit): DataAffine[T, Dim] =
      val dataCopy = data.copy
      f(dataCopy)
      dataCopy

  val donut: DataAffine[Double, Dim] = DataAffine(Seq(aa, bb, cc, dd).map(_ -> donutFilling)) // no e
  val hole: DataAffine[Double, Dim] = DataAffine.of(ee -> holeFilling)
  override val clippedDonut: DataAffine[Double, Dim] = donut.mutate(_ ∩ clipInterval)

  extension [T](data: DataAffine[T, Dim])
    private def filledWith(v: T): DataAffine[T, Dim] = data.mutate(_.mapValues(_ => v))

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
      ).mutate(_ + ((fromOrigin x toBeforeOrigin) -> 4)) // IV

      val withoutQuadrantTwo = DataAffine.of( // origin present (in III)
        (fromAfterOrigin x fromAfterOrigin) -> 1L // I
      )
      withoutQuadrantTwo ++ Seq(
        (toOrigin x toOrigin) -> 3L, // III
        (fromAfterOrigin x toOrigin) -> 4L // IV
      )

      withoutQuadrantOne.copy == withoutQuadrantOne shouldBe true
      withoutQuadrantOne.toImmutable ≡≡ withoutQuadrantOne.toMutable.toImmutable
      withoutQuadrantOne.mutate(_.recompressAll()) == withoutQuadrantOne shouldBe true
      withoutQuadrantOne.mutate(_.recompressAll(withoutQuadrantTwo.allIntervals)) ≡≡ withoutQuadrantOne

      withoutQuadrantOne == ("bogus": Any) shouldBe false // different types
      withoutQuadrantOne == withoutQuadrantTwo shouldBe false // different quadrants missing
      val withoutQuadrantTwoInt = DataAffine(withoutQuadrantTwo.getAll.map(d => d.copy(value = d.value.toInt)))
      withoutQuadrantOne !≡ withoutQuadrantTwoInt // different quadrants missing

      val fcv = withoutQuadrantOne.copy
      fcv.collectValues:
        case v if v < 4 => 100 * v
      fcv.getAll.toList shouldBe List(
        (toBeforeOrigin x toBeforeOrigin) -> 300,
        (toBeforeOrigin x fromOrigin) -> 200
      )

      val fci = withoutQuadrantOne.copy
      fci.collectIntervals:
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

      val withoutQuadrantOneUnit = withoutQuadrantOne.toImmutable.mapValues(_ => ()).toMutable
      val withoutQuadrantTwoUnit = withoutQuadrantTwo.toImmutable.mapValues(_ => ()).toMutable

      // setIfNoConflict ensures no overlaps, subsequent checks ensure no gaps
      val complete1 = withoutQuadrantOneUnit.copy
      complete1.setIfNoConflict((fromOrigin x fromOrigin) -> ()) shouldBe true // I
      val complete2 = withoutQuadrantTwoUnit.copy
      complete2.setIfNoConflict((toOrigin x fromAfterOrigin) -> ()) shouldBe true // II
      complete1 ≡≡ complete2
      complete1 ≡≡ DataAffine.of[Unit, Dim](())

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

      donut ≡≡ (ξ.mutate(_ - ee))
      donut ≡≡ (ξ.mutate(_ \ hole))
      donut ≡≡ (ξ.mutate(_ - ee))
      donut ≡≡ (ξ.mutate(_ \ hole))
      donut ≡≡ (∅[Double, Dim].mutate(_ ++ Seq(aa, bb, cc, dd).map(_ -> donutFilling)))
      assert(donut isSubsetOf ξ)
      assert(!donut.isEmpty)

      hole.filledWith(donutFilling) ≡≡ (ξ.mutate(_ -- Seq(aa, bb, cc, dd)))
      hole.filledWith(donutFilling) ≡≡ (ξ.mutate(_ \ donut))
      hole ≡≡ (ξ.mutate(_ -- Seq(aa, bb, cc, dd)).filledWith(holeFilling))
      hole ≡≡ (ξ.mutate(_ \ donut).filledWith(holeFilling))
      hole ≡≡ (∅[Double, Dim].mutate(_ + (ee -> holeFilling)))
      assert(hole isSubsetOf ξ)
      assert(!hole.isEmpty)

      (donut.mutate(_ △ hole)).filledWith(donutFilling) ≡≡ ξ
      donut.mutate(_ △ hole) !≡ ξ // because e has hole filling

      (Seq(aa, ee).holeFilled.mutate(_ \ Seq(aa).holeFilled)) ≡≡ hole

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

      (x1.mutate(_ \ y1)) ≡≡ Seq(aa, bb).donutFilled
      (y1.mutate(_ \ x1)) ≡≡ Seq(dd, ee).holeFilled
      (x1.mutate(_ △ y1)) ≡≡ (Seq(aa, bb).donutFilled.mutate(_ ++ Seq(dd, ee).holeFilled.getAll))
      (y1 ⊆ x1) shouldBe false

      val x2 = Seq(aa, bb, cc).donutFilled
      val y2 = Seq(aa, bb).donutFilled
      (x2.mutate(_ \ y2)) ≡≡ Seq(cc).donutFilled
      (y2.mutate(_ \ x2)) ≡≡ ∅
      (x2.mutate(_ △ y2)) ≡≡ Seq(cc).donutFilled
      (y2 ⊆ x2) shouldBe true

      donut intersects clipInterval shouldBe true
      clippedDonut ≡≡ Seq(aa, bb, cc, dd).flatMap(_ ∩ clipInterval).donutFilled
      clippedDonut ≡≡ clippedDonut.mutate(_.mapIntervals(_.swapDimensions[Dim](0, 1))).asDataAffine
      assert(clippedDonut isSubsetOf donut)

      val flatMapped = donut.mutate(_.flatMap: v =>
        (v.interval ∩ clipInterval).toSeq.donutFilled)
      clippedDonut ≡≡ flatMapped.asDataAffine

      val filtered = clippedDonut.mutate(_.filter(_.interval.start > Domain.in2D(0, 0)))
      Seq(intervalFromAfter(1).to(10) x interval(-10, 1)).donutFilled ≡≡ filtered

      val collected = clippedDonut.mutate(_.collect:
        case x if x.interval.start > Domain.in2D(0, 0) => x.copy(interval = x.interval.swapDimensions[Dim](0, 1)))
      Seq(interval(-10, 1) x intervalFromAfter(1).to(10)).donutFilled ≡≡ collected.asDataAffine

      val donutIn3D: DataAffine[Double, Domain.In3D[Int, Int, Int]] =
        clippedDonut.extrudeDimension[Int, Domain.In3D[Int, Int, Int]](2, interval(-1, 1))
      val flattenedDonut: DataAffine[Double, Dim] = donutIn3D.collapseDimension[Dim](2, (takeFirst, _) => takeFirst)
      flattenedDonut shouldBe clippedDonut
