package intervalidus.mutable

import intervalidus.*
import intervalidus.ContinuousValue.given
import intervalidus.DimensionalFunctionBase.{DomainFunction, ValidFunction}
import intervalidus.Domain1D.{Bottom, Top}
import intervalidus.DomainLike.given
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class DataFunctionContinuousTest extends AnyFunSuite with Matchers:

  import Interval1D.*
  import Domain.*

  type Dim1D = In1D[Double]
  type Dim2D = In2D[Double, Double]

  val q1 = intervalFrom(0.0) x intervalFrom(0.0)
  val q2 = intervalToBefore(0.0) x intervalFrom(0.0)
  val q3And4 = unbounded[Double] x intervalToBefore(0.0)

  def squaredDistance(x: Double, y: Double): Double = math.pow(x, 2) + math.pow(y, 2)

  /*
   * Function in 2d domain can be thought of as a 3d shape there the function defines z(x, y) piecewise:
   *
   * - Quadrant 1 (x >= 0, y >= 0) - Bivariate variation: z = exp(-(x^2 + y^2)) (the Gaussian bell curve).
   *
   * - Quadrant 2 (x < 0, y >= 0) - Univariate Constant: z = 1.0 (a horizontal shelf).
   *
   * - Quadrants 3 & 4 (y < 0) - Radial Transition: max(0.0, 1.0 - (x^2 + y^2)) (a parabaloid dome that drops
   *   continuously from 1.0 at the origin down to 0.0, but smoothly vanishes (is "clamped") outside the unit circle).
   *                      ^
   *                      |
   *        Q2: Plateau   |   Q1: Gaussian Curve
   *        (Flat z = 1)  |   (Exponential tail)
   *       ---------------+--------------- x
   *      /               |               \
   *     /                |                \
   *    /  Q3: Half Dome  |  Q4: Half Dome  \
   *     (Parabaloid drop |  (Parabaloid drop
   *       clamped at 0)  |   clamped at 0)
   *                      |
   *                      v  y
   */
  val surface2D: DataFunction[Double, Dim2D] = DataFunction.of(
    q1 -> {
      case Point.In2D(x, y) => math.exp(-squaredDistance(x, y))
      case _                => 0.0
    },
    q2 -> (_ => 1.0),
    q3And4 -> {
      case Point.In2D(x, y) => math.max(0.0, 1.0 - squaredDistance(x, y))
      case _                => 0.0
    }
  )

  // Delta tolerance for floating-point comparisons
  val epsilon = 1e-6

  extension (actual: Double)
    infix def shouldBeAbout(expected: Double): Assertion = assert(
      math.abs(actual - expected) < epsilon,
      s"\nExpected: \n$expected ± $epsilon\nActual: \n$actual\n"
    )

  extension (actualOption: Option[Double])
    infix def shouldBeAbout(expectedOption: Option[Double]): Assertion = (actualOption, expectedOption) match
      case (None, None)                   => succeed
      case (None, Some(expected))         => fail(s"\nExpected: \n$expected ± $epsilon\nActual: \nNone\n")
      case (Some(actual), None)           => fail(s"\nExpected: \nNone\nActual: \n$actual\n")
      case (Some(actual), Some(expected)) =>
        assert(math.abs(actual - expected) < epsilon, s"\nExpected: \n$expected ± $epsilon\nActual: \n$actual\n")

  extension (actual: (Double, Double))
    infix def shouldBeAbout(expected: (Double, Double)): Assertion = assert(
      math.abs(actual._1 - expected._1) < epsilon && math.abs(actual._2 - expected._2) < epsilon,
      s"\nExpected: \n$expected ± $epsilon\nActual: \n$actual\n"
    )

  extension [V, D <: NonEmptyTuple: DomainLike](lhs: DimensionalFunctionBase[V, D])
    infix def ≡≡(rhs: DimensionalFunctionBase[V, D]): Assertion =
      assert(lhs ≡ rhs, s"\nExpected (rhs): \n$rhs\nActual (lhs): \n$lhs\n")
    infix def !≡(rhs: DimensionalFunctionBase[V, D]): Assertion = assert(!(lhs ≡ rhs))

  extension [V, D <: NonEmptyTuple: DomainLike](data: DataFunction[V, D])
    def mutate(f: DataFunction[V, D] => Unit): DataFunction[V, D] =
      val dataCopy = data.copy
      f(dataCopy)
      dataCopy

  test("Mutable: Construction, mapping, and collecting"):
    surface2D ≡≡ surface2D.toMutable
    surface2D.copy ≡≡ surface2D.toImmutable
    val toData: immutable.Data[DomainFunction[Double, Dim2D], Dim2D] = surface2D // implicit conversion to underlying
    val andBack: DataFunction[Double, Dim2D] = toData // implicit conversion back again
    andBack ≡≡ surface2D
    andBack.hashCode() shouldBe surface2D.hashCode()
    andBack == ("bogus": Any) shouldBe false // different types
    DataFunction[Double, Dim2D]() ≡≡ DataFunction.∅[Double, Dim2D]
    val builder = DataFunction.newBuilder[Double, Dim2D]
    surface2D.getAll.foreach(builder.addOne)
    builder.result() ≡≡ surface2D

    surface2D.isEmpty shouldBe false
    surface2D.size shouldBe 3
    surface2D.foldLeft(0)((a, _) => a + 1) shouldBe 3

    surface2D(in2D(0.0, 0.0)) shouldBeAbout 1.0 // Center point where all regions converge
    surface2D(in2D(1.0, 1.0)) shouldBeAbout math.exp(-2.0) // Q1: Gaussian evaluation at (1.0, 1.0)
    surface2D(in2D(-1.5, 2.0)) shouldBeAbout 1.0 // Q2: Flat plateau at (-1.5, 2.0)
    surface2D(in2D(0.5, -0.5)) shouldBeAbout 0.5 // Q3/Q4: Clamped dome inside unit circle
    surface2D(in2D(1.0, -1.0)) shouldBeAbout 0.0 // Q3/Q4: Clamped dome outside unit circle

    // values at domain limits are defined
    surface2D(in2D[Double, Double](Top, Top)) shouldBeAbout 0.0 // Q1
    surface2D(in2D[Double, Double](Bottom, Top)) shouldBeAbout 1.0 // Q2
    surface2D(in2D[Double, Double](Bottom, Bottom)) shouldBeAbout 0.0 // Q3
    surface2D(in2D[Double, Double](Top, Bottom)) shouldBeAbout 0.0 // Q4

    val q1Mapped = intervalFromAfter(0.0) x intervalFromAfter(0.0)
    val q2Mapped = intervalTo(0.0) x intervalFromAfter(0.0)
    val q3And4Mapped = unbounded[Double] x intervalTo(0.0)

    val mapped: DataFunction[Double, Dim2D] = surface2D.mutate(_.map: // mutable can't change value type in map
      case ValidData(f, i) => Interval(i.start.rightAdjacent, i.end.rightAdjacent) -> f.andThen(d => (10 * d).toInt))

    mapped.getDataAt(in2D(1.0, 1.0)).map(_.interval) shouldBe Some(q1Mapped) // q1
    mapped.getDataAt(in2D(-1.0, 1.0)).map(_.interval) shouldBe Some(q2Mapped) // q2
    mapped.getDataAt(in2D(-1.0, -1.0)).map(_.interval) shouldBe Some(q3And4Mapped) // q3
    mapped.getDataAt(in2D(1.0, -1.0)).map(_.interval) shouldBe Some(q3And4Mapped) // q4
    mapped(in2D(0.0, 0.0)) shouldBe 10.0 // Center point where all regions converge
    mapped(in2D(1.0, 1.0)) shouldBe 1.0 // Q1: Gaussian evaluation at (1.0, 1.0), (10 * exp(-2.0)).toInt = 1
    mapped(in2D(-1.5, 2.0)) shouldBe 10.0 // Q2: Flat plateau at (-1.5, 2.0)
    mapped(in2D(0.5, -0.5)) shouldBe 5.0 // Q3/Q4: Clamped dome inside unit circle (10 * 0.5 = 5)
    mapped(in2D(1.0, -1.0)) shouldBe 0.0 // Q3/Q4: Clamped dome outside unit circle

    val mappedValues = surface2D.mutate(_.mapValues(d => (10 * d).toInt))

    mappedValues.getDataAt(in2D(1.0, 1.0)).map(_.interval) shouldBe Some(q1) // q1 - no change
    mappedValues.getDataAt(in2D(-1.0, 1.0)).map(_.interval) shouldBe Some(q2) // q2 - no change
    mappedValues.getDataAt(in2D(-1.0, -1.0)).map(_.interval) shouldBe Some(q3And4) // q3 - no change
    mappedValues.getDataAt(in2D(1.0, -1.0)).map(_.interval) shouldBe Some(q3And4) // q4 - no change
    mappedValues(in2D(0.0, 0.0)) shouldBe 10.0 // Center point where all regions converge
    mappedValues(in2D(1.0, 1.0)) shouldBe 1.0 // Q1: Gaussian evaluation at (1.0, 1.0), (10 * exp(-2.0)).toInt = 1)
    mappedValues(in2D(-1.5, 2.0)) shouldBe 10.0 // Q2: Flat plateau at (-1.5, 2.0)
    mappedValues(in2D(0.5, -0.5)) shouldBe 5.0 // Q3/Q4: Clamped dome inside unit circle (10 * 0.5 = 5)
    mappedValues(in2D(1.0, -1.0)) shouldBe 0.0 // Q3/Q4: Clamped dome outside unit circle

    val mappedIntervals: DataFunction[Double, Dim2D] = surface2D.mutate(_.mapIntervals:
      case i => Interval(i.start.rightAdjacent, i.end.rightAdjacent))

    mappedIntervals.getDataAt(in2D(1.0, 1.0)).map(_.interval) shouldBe Some(q1Mapped) // q1
    mappedIntervals.getDataAt(in2D(-1.0, 1.0)).map(_.interval) shouldBe Some(q2Mapped) // q2
    mappedIntervals.getDataAt(in2D(-1.0, -1.0)).map(_.interval) shouldBe Some(q3And4Mapped) // q3
    mappedIntervals.getDataAt(in2D(1.0, -1.0)).map(_.interval) shouldBe Some(q3And4Mapped) // q4

    val collected = surface2D.mutate(_.collect: // extend Q1 negative Gaussian to the whole domain
      case ValidData(f, i) if i.contains(in2D(1.0, 1.0)) => Interval.unbounded[Dim2D] -> f.andThen(-_))

    collected.getDataAt(in2D(1.0, 1.0)).map(_.interval) shouldBe Some(Interval.unbounded[Dim2D]) // whole domain
    collected.getOption.isDefined shouldBe true
    val negativeGaussian: Dim2D => Double = collected.get
    for
      x <- -3 to 3
      y <- -3 to 3
    do
      val point = in2D(x.toDouble / 10.0, y.toDouble / 10.0) // -0.3 to 0.3 by 0.1
      collected(point) shouldBeAbout negativeGaussian(point)

    val collectedValues = surface2D.mutate(_.collectValues: // drop the flat plateau and negate results
      case f if f(in2D(-10.0, 10.0)) < 1.0 => f.andThen(-_))

    collectedValues.isDefinedAt(in2D(-1.5, 2.0)) shouldBe false // Q2: Flat plateau is dropped
    collectedValues(in2D(0.0, 0.0)) shouldBeAbout -1.0 // Center point where all regions converge
    collectedValues(in2D(1.0, 1.0)) shouldBeAbout -math.exp(-2.0) // Q1: Gaussian evaluation at (1.0, 1.0)
    collectedValues(in2D(0.5, -0.5)) shouldBeAbout -0.5 // Q3/Q4: Clamped dome inside unit circle
    collectedValues(in2D(1.0, -1.0)) shouldBeAbout 0.0 // Q3/Q4: Clamped dome outside unit circle

    val collectedIntervals = surface2D.mutate(_.collectIntervals: // extend Q1 Gaussian to the whole domain
      case i if i.contains(in2D(1.0, 1.0)) => Interval.unbounded[Dim2D])
    collectedIntervals.getOption.isDefined shouldBe true

    val gaussian: Dim2D => Double = collectedIntervals.get
    for
      x <- -3 to 3
      y <- -3 to 3
    do
      val point = in2D(x.toDouble / 10.0, y.toDouble / 10.0) // -0.3 to 0.3 by 0.1
      collectedIntervals(point) shouldBeAbout gaussian(point)
      collectedIntervals(point) shouldBeAbout -negativeGaussian(point)

    val clampOutside = interval(-1.0, 1.0) x interval(-1.0, 1.0)
    val flatmapped = collectedIntervals.mutate(_.flatMap: d => // clamp value outside a bounded interval
      val clampedData: Iterable[ValidFunction[Double, Dim2D]] = d.interval
        .separateUsing(clampOutside)
        .map: i =>
          if i.isBounded then i -> d.value else i -> (_ => 0.0)
      DataFunction(clampedData))

    flatmapped(in2D(-1.5, -1.5)) shouldBeAbout 0.0
    flatmapped(in2D(-1.0, -1.0)) shouldBeAbout gaussian(in2D(-1.0, -1.0))
    flatmapped(in2D(-0.5, -0.5)) shouldBeAbout gaussian(in2D(-0.5, -0.5))
    flatmapped(in2D(0.0, 0.0)) shouldBeAbout gaussian(in2D(0.0, 0.0))
    flatmapped(in2D(0.5, 0.5)) shouldBeAbout gaussian(in2D(0.5, 0.5))
    flatmapped(in2D(1.0, 1.0)) shouldBeAbout gaussian(in2D(1.0, 1.0))
    flatmapped(in2D(1.5, 1.5)) shouldBeAbout 0.0

  test("Mutable: Continuity, diff actions, and queries"):
    val reciprocalFunction: DomainFunction[Double, Dim1D] = _ match
      case Point.In1D(x) => 1.0 / x
      case _             => 0.0 // as x -> ∞ and -∞, 1/x -> 0
    val unsafeReciprocal = DataFunction.ofValue(reciprocalFunction) // defined everwhere
    unsafeReciprocal.isDefinedAt(0.0) shouldBe true
    unsafeReciprocal.isDefinedAt(Top) shouldBe true

    val safeReciprocal = unsafeReciprocal.mutate(_.remove(intervalAt(0.0))) // undefined at x = 0
    safeReciprocal.isDefinedAt(0.0) shouldBe false
    safeReciprocal ≡≡ DataFunction.of(
      intervalToBefore(0.0) -> reciprocalFunction,
      intervalFromAfter(0.0) -> reciprocalFunction
    )

    val diffActions = safeReciprocal.diffActionsFrom(unsafeReciprocal).toList
    diffActions.map(_.toCodeLikeString).foreach(println)
    diffActions shouldBe List(
      DiffAction.Update(intervalToBefore(0.0) -> reciprocalFunction), // (∞, -∞) => (∞, 0.0)
      DiffAction.Create(intervalFromAfter(0.0) -> reciprocalFunction) // adds (0.0, -∞)
    )
    unsafeReciprocal.mutate(_.applyDiffActions(diffActions)) ≡≡ safeReciprocal
    unsafeReciprocal.mutate(_.syncWith(safeReciprocal)) ≡≡ safeReciprocal

    val allIntervals = safeReciprocal.allIntervals.toList
    allIntervals shouldBe List(intervalToBefore(0.0).tupled, intervalFromAfter(0.0).tupled)
    safeReciprocal.domain shouldBe IntervalShape(allIntervals)
    safeReciprocal.intervals(reciprocalFunction).toList shouldBe allIntervals
    safeReciprocal.values.toList shouldBe List(reciprocalFunction)

    safeReciprocal.isDefinedAt(0.0) shouldBe false
    safeReciprocal(2.0) shouldBeAbout 0.5
    safeReciprocal(-2.0) shouldBeAbout -0.5

    val shiftedReciprocal = safeReciprocal.mutate(_.mapIntervals: i =>
      Interval(i.start.leftAdjacent, i.end)) // shifts the positive domain start to include 0.0

    shiftedReciprocal.domain shouldBe IntervalShape.ξ[Dim1D] // (∞, 0) + [0, ∞) = ξ
    shiftedReciprocal shouldBe unsafeReciprocal
    shiftedReciprocal.isDefinedAt(0.0) shouldBe true // uh-oh
    shiftedReciprocal(0.0) shouldBe Double.PositiveInfinity
    shiftedReciprocal(-0.0) shouldBe Double.NegativeInfinity

    val uncompressed = DataFunction.of(
      intervalToBefore(0.0) -> reciprocalFunction,
      intervalFrom(0.0) -> reciprocalFunction
    )
    uncompressed ≡≡ shiftedReciprocal
    uncompressed shouldNot be(shiftedReciprocal)
    uncompressed.mutate(_.compress(reciprocalFunction)) shouldBe shiftedReciprocal
    uncompressed.mutate(_.compressAll()) shouldBe shiftedReciprocal
    uncompressed.mutate(_.recompressAll()) shouldBe shiftedReciprocal

  test("Mutable: Dimension transforms and other geometry"):
    // Slice along X at y = -0.5 (tracing across the Q3/Q4 dome)
    val bottomSliceY: DataFunction.In1D[Double, Double] = surface2D.getByDimension(dimensionIndex = 1, domain = -0.5)

    // Expected curve: f(x) = max(0.0, 1 - 0.25 - x^2)
    bottomSliceY(0.0) shouldBeAbout 0.75
    bottomSliceY(0.5) shouldBeAbout 0.50

    // Boundary check at x = sqrt(0.75) ~ 0.866025
    bottomSliceY(in1D(math.sqrt(0.75))) shouldBeAbout 0.0

    // Outside the circle clamp
    bottomSliceY(1.0) shouldBeAbout 0.0

    // Slice along Y at x = -0.5 (tracing across the Q3 dome and Q2 plateau)
    val leftSliceX: DataFunction.In1D[Double, Double] = surface2D.getByHeadDimension(domain = -0.5)

    leftSliceX(-0.5) shouldBeAbout 0.50 // Expected curve: f(y) = max(0.0, 1 - 0.25 - y^2)
    leftSliceX(0.5) shouldBeAbout 1.0 // Expected curve: f(y) = 1.0

    // Boundary check at y = -sqrt(0.75) ~ -0.866025
    leftSliceX(in1D(-math.sqrt(0.75))) shouldBeAbout 0.0

    // Outside the circle clamp
    leftSliceX(-1.0) shouldBeAbout 0.0

    val maxProfileAlongX = surface2D.collapseDimension[Dim1D](
      dimensionIndex = 1, // Collapse Y (dimension 1)
      mapFunctions = oldFn2D => (d1D => oldFn2D(d1D x 0.0)), // sampling at y = 0.0
      mergeValues = math.max // find the max peak along the X-axis
    )

    val maxProfileAlongY = surface2D.collapseDimension[Dim1D](
      dimensionIndex = 0, // Collapse X (dimension 0)
      mapFunctions = oldFn2D => (d1D => oldFn2D(d1D.withHead(0.0))), //  sampling at x = 0.0
      mergeValues = math.max // find the max peak along the Y-axis
    )

    // For x <= 0: Plateau Q2 dominates at 1.0
    maxProfileAlongX(-2.0) shouldBeAbout 1.0
    maxProfileAlongX(-0.5) shouldBeAbout 1.0
    maxProfileAlongY(0.0) shouldBeAbout 1.0

    // For y >= 0: Plateau Q2 dominates at 1.0 across the whole top half
    maxProfileAlongY(0.0) shouldBeAbout 1.0
    maxProfileAlongY(1.0) shouldBeAbout 1.0
    maxProfileAlongY(10.0) shouldBeAbout 1.0

    // For 0 <= x <= 1:
    // Q4 Dome height at x=0.5 is (1 - 0.25) = 0.75
    // Q1 Gaussian height at x=0.5 is exp(-0.25) ~ 0.7788 (larger)
    maxProfileAlongX(0.5) shouldBeAbout math.exp(-0.25)

    // For x > 1 (outside unit circle):
    // Q4 Dome at y=0 is (1 - 0.25) = 0.75
    // Dome is clamped to 0.0, Gaussian is exp(-4.0) ~ 0.018315
    maxProfileAlongX(2.0) shouldBeAbout math.exp(-4.0)

    // For y < 0 (Dome region):
    // At y = -0.5: max(0.0, 1.0 - (-0.5)^2) = 0.75
    maxProfileAlongY(-0.5) shouldBeAbout 0.75

    // At y = -1.0 (Dome edge):
    maxProfileAlongY(-1.0) shouldBeAbout 0.0

    // For y < -1.0 (Outside dome in y < 0 space):
    maxProfileAlongY(-2.0) shouldBeAbout 0.0

    val clippedProfileAlongX = maxProfileAlongX.mutate(_ ∩ interval(-1.0, 2.0))
    val clippedProfileAlongY = maxProfileAlongY.mutate(_ intersection interval(-2.0, 1.0))

    clippedProfileAlongX.intersection(clippedProfileAlongY) shouldBe IntervalShape.of(interval(-1.0, 1.0))
    (clippedProfileAlongX intersects interval(-1.0, 1.0)) shouldBe true
    clippedProfileAlongX.getIntersecting(interval(-1.0, 1.0)).toList shouldBe clippedProfileAlongX.getAll.toList
    (clippedProfileAlongX isSubsetOf maxProfileAlongX) shouldBe true
    (clippedProfileAlongX ⊆ maxProfileAlongX) shouldBe true

    val zipped = clippedProfileAlongX.zip(clippedProfileAlongY)
    zipped.boundingInterval shouldBe Some(interval(-1.0, 1.0).tupled)
    zipped(0.0) shouldBeAbout (1.0, 1.0)
    zipped(-0.5) shouldBeAbout (1.0, 0.75)
    zipped.isDefinedAt(1.5) shouldBe false
    zipped.isDefinedAt(-1.5) shouldBe false

    val zippedAll = clippedProfileAlongX.zipAll(clippedProfileAlongY, _ => -42.0, _ => 42.0)
    zippedAll.boundingInterval shouldBe Some(interval(-2.0, 2.0).tupled)
    zippedAll(0.0) shouldBeAbout (1.0, 1.0)
    zippedAll(-0.5) shouldBeAbout (1.0, 0.75)
    zippedAll(1.5) shouldBeAbout (math.exp(-2.25), 42) // default Y
    zippedAll(-1.5) shouldBeAbout (-42, 0.0) // default X
    zipped.isDefinedAt(2.5) shouldBe false
    zipped.isDefinedAt(-2.5) shouldBe false

    def average(x: Double, y: Double) = (x + y) / 2.0
    val merged = clippedProfileAlongX.mutate(_.merge(clippedProfileAlongY, average))
    merged(0.0) shouldBeAbout 1.0 // both contributions are 1.0
    merged(-0.5) shouldBeAbout average(1.0, 0.75) // average of different contributions
    merged(1.5) shouldBeAbout math.exp(-2.25) // no Y contribution
    merged(-1.5) shouldBeAbout 0.0 // no X contribution
    merged.isDefinedAt(2.5) shouldBe false
    merged.isDefinedAt(-2.5) shouldBe false

    val mergedMany = clippedProfileAlongX.mutate(_.mergeMany(clippedProfileAlongY.getAll, average))
    mergedMany(0.0) shouldBeAbout 1.0 // both contributions are 1.0
    mergedMany(-0.5) shouldBeAbout average(1.0, 0.75) // average of different contributions
    mergedMany(1.5) shouldBeAbout math.exp(-2.25) // no Y contribution
    mergedMany(-1.5) shouldBeAbout 0.0 // no X contribution
    mergedMany.isDefinedAt(2.5) shouldBe false
    mergedMany.isDefinedAt(-2.5) shouldBe false

    val xor = clippedProfileAlongX.mutate(_ △ clippedProfileAlongY)
    xor.isDefinedAt(0.0) shouldBe false
    xor.isDefinedAt(-0.5) shouldBe false
    xor(1.5) shouldBeAbout math.exp(-2.25) // no Y contribution
    xor(-1.5) shouldBeAbout 0.0 // no X contribution
    xor.isDefinedAt(2.5) shouldBe false
    xor.isDefinedAt(-2.5) shouldBe false

    val diff = clippedProfileAlongX.mutate(_ \ clippedProfileAlongY)
    diff.isDefinedAt(0.0) shouldBe false
    diff.isDefinedAt(-0.5) shouldBe false
    diff(1.5) shouldBeAbout math.exp(-2.25) // no Y at this point
    diff.isDefinedAt(-1.5) shouldBe false
    diff.isDefinedAt(2.5) shouldBe false
    diff.isDefinedAt(-2.5) shouldBe false

    val extruded: DataFunction[Double, Dim2D] = clippedProfileAlongX.extrudeDimension(0, interval(-1.0, 1.0))
    extruded.getAt(in2D(0.0, -2.0)) shouldBeAbout None // outside source clip
    extruded.getAt(in2D(0.0, -0.5)) shouldBeAbout Some(1.0)
    extruded.getAt(in2D(0.0, 0.5)) shouldBeAbout Some(math.exp(-0.25))
    extruded.getAt(in2D(0.0, 2.0)) shouldBeAbout Some(math.exp(-4.0))
    extruded.getAt(in2D(10.0, 2.0)) shouldBeAbout None // outside extrude extent

  test("Mutable: Thin tests for thin wrappers"):
    val reciprocalFunction: DomainFunction[Double, Dim1D] = _ match
      case Point.In1D(x) => 1.0 / x
      case _             => 0.0 // as x -> ∞ and -∞, 1/x -> 0
    val safeReciprocal = DataFunction.ofValue(reciprocalFunction).mutate(_ - intervalAt(0.0))

    val positive = safeReciprocal.mutate(_.filter(_.interval.contains(1.0)))
    positive.isDefinedAt(1.0) shouldBe true
    positive.isDefinedAt(-1.0) shouldBe false

    val clampPositive: ValidFunction[Double, Dim1D] = intervalFrom(10.0) -> (_ => 0.0)
    val clampNegative: ValidFunction[Double, Dim1D] = intervalTo(-10.0) -> (_ => 0.0)
    val positiveClamped = safeReciprocal.mutate(_ + clampPositive)
    positiveClamped(10.0) shouldBe 0.0
    safeReciprocal.mutate(_.set(clampPositive)) ≡≡ positiveClamped
    val bothClamped = safeReciprocal.mutate(_ ++ Seq(clampPositive, clampNegative))
    bothClamped(-10.0) shouldBe 0.0
    safeReciprocal.mutate(_.setMany(Seq(clampPositive, clampNegative))) ≡≡ bothClamped

    val posInfinityConstant: DomainFunction[Double, Dim1D] = _ => Double.PositiveInfinity
    val negInfinityConstant: DomainFunction[Double, Dim1D] = _ => Double.NegativeInfinity
    val zeroConstant: DomainFunction[Double, Dim1D] = _ => 0.0

    val unsafeFilled = safeReciprocal.mutate(_.fill(unbounded -> posInfinityConstant))
    unsafeFilled(0.0) shouldBe Double.PositiveInfinity
    unsafeFilled(1.0) shouldBe 1.0

    val unsafe = safeReciprocal.copy
    if (!unsafe.setIfNoConflict(intervalAt(0.0) -> posInfinityConstant)) then fail("expected to set once")
    else if (unsafe.setIfNoConflict(intervalAt(0.0) -> posInfinityConstant)) then fail("didn't expect to set twice")
    else
      unsafe(0.0) shouldBe Double.PositiveInfinity
      val unsafeNeg = unsafe.mutate(_.update(intervalAt(0.0) -> negInfinityConstant))
      unsafeNeg(0.0) shouldBe Double.NegativeInfinity
      val unsafeZero = unsafe.mutate(
        _.replace(
          intervalAt(0.0) -> posInfinityConstant,
          intervalAt(0.0) -> zeroConstant
        )
      )
      unsafeZero(0.0) shouldBe 0.0
      unsafe.mutate(_.replaceByKey(0.0, intervalAt(0.0) -> zeroConstant)).getAt(0.0) shouldBe Some(0.0)
      unsafe.mutate(_.removeByKey(0.0)).getAt(0.0) shouldBe None
      val removeMany = unsafe.mutate(_ -- Seq(intervalToBefore(-10.0), intervalFromAfter(10.0)))
      removeMany.domain shouldBe IntervalShape.of(interval(-10.0, 10.0))
      unsafe.mutate(_.removeMany(Seq(intervalToBefore(-10.0), intervalFromAfter(10.0)))) ≡≡ removeMany
      unsafe.mutate(_.removeValue(posInfinityConstant)) ≡≡ safeReciprocal
