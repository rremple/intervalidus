package intervalidus

import intervalidus.Domain.{HasDisplacementType, HasScalarType}
import intervalidus.Domain1D.{Bottom, OpenPoint, Point, Top}

import scala.language.adhocExtensions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Type class with operations on a domain with multiple discrete and/or continuous dimensions that are all affine. See
  * [[DomainLike]] for more details.
  *
  * @tparam D
  *   the affine domain type -- a non-empty tuple of one-dimensional domains, where each can have its own affine domain
  *   value type.
  */
class DomainAffineLike[D <: NonEmptyTuple: DomainLikeTupleOps](using
  val applyToAffineDomain: DomainAffineLikeTupleOps[D]
) extends DomainLike[D]

/**
  * Common definitions and extensions for single and multidimensional structures with affine domains.
  */
object DomainAffineLike:

  given [D <: NonEmptyTuple: DomainAffineLikeTupleOps: DomainLikeTupleOps]: DomainAffineLike[D] = DomainAffineLike[D]

  extension [T](lhs: Domain1D[T])(using op: DomainAffineValueLike[T])
    /**
      * Finds the displacement from this domain to another. Overflows, such as displacements to or from Top or Bottom,
      * or overflowing underlying value operations, are returned as None.
      */
    infix def displacementTo(rhs: Domain1D[T]): Option[op.Displacement] =
      (lhs, rhs) match
        case (Domain1D.Bounded(from), Domain1D.Bounded(to)) => op.displacement(from, to)
        case _                                              => None

    /**
      * Scale this domain relative to some center. Overflows of the underlying value operations are translated to
      * Bottom/Top. See [[https://en.wikipedia.org/wiki/Scaling_(geometry)]] and
      * [[https://en.wikipedia.org/wiki/Homothetic_center]].
      */
    infix def scaledAbout(center: Domain1D[T], scaledBy: op.Scalar): Domain1D[T] =
      val centerValue: T = center match
        case Domain1D.Bounded(value) => value
        case bottomOrTop             => throw Exception(s"can't scale using an unbounded center: $bottomOrTop")

      val scalePositive = scaledBy > op.zeroScalar
      def scaleAs(value: T, domain: T => Domain1D[T]): Domain1D[T] = op.displacement(centerValue, value) match
        case None => // displacement from center overflows
          val displacementPositive = value > centerValue
          if displacementPositive == scalePositive then Top else Bottom
        case Some(displacement) =>
          op.scale(displacement, scaledBy) match
            case None => // scaling displacement overflows
              val displacementPositive = displacement > op.zeroDisplacement
              if displacementPositive == scalePositive then Top else Bottom
            case Some(scaledDisplacement) =>
              op.displace(centerValue, scaledDisplacement) match
                case Some(scaledValue) => domain(scaledValue)
                case None              => // displacing by scaled displacement overflows
                  if scaledDisplacement > op.zeroDisplacement then Top else Bottom

      lhs match
        case Point(value)     => scaleAs(value, Point(_))
        case OpenPoint(value) => scaleAs(value, OpenPoint(_))
        case Bottom           => if scaledBy >= op.zeroScalar then Bottom else Top
        case Top              => if scaledBy >= op.zeroScalar then Top else Bottom

    /**
      * Reflects this domain about a pivot. Overflows of the underlying value operations are translated to Bottom/Top.
      */
    infix def reflectedAbout(pivot: Domain1D[T]): Domain1D[T] =
      lhs.scaledAbout(pivot, op.reflectionScalar)

    /**
      * Displaces this domain. Overflows of the underlying value operations are translated to Bottom/Top.
      */
    infix def displacedBy(offset: op.Displacement): Domain1D[T] =
      def displaceAs(value: T, domain: T => Domain1D[T]): Domain1D[T] =
        op.displace(value, offset) match
          case Some(inBounds) => domain(inBounds)
          case None           => if offset > op.zeroDisplacement then Top else Bottom
      lhs match
        case Point(value)     => displaceAs(value, Point(_))
        case OpenPoint(value) => displaceAs(value, OpenPoint(_))
        case bottomOrTop      => bottomOrTop

  extension [T](lhs: Interval1D[T])(using op: DomainAffineValueLike[T])

    /**
      * The measure of an interval.
      *
      * @note
      *   Discrete intervals count the inclusive elements ("fence posts"), whereas continuous intervals measure the pure
      *   coordinate distance between boundaries ("space between posts"). So the measure of [1..3] in a discrete domain
      *   is 3 (i.e., 3 + 1 - 1), where the measure of intervals [1, 3], (1, 3], [1, 3), or (1, 3) in a continuous
      *   domain are all 2 (i.e., 3 - 1).
      */
    def measure: Option[op.Displacement] =
      /*
       * Why is it `end.rightAdjacent` instead of just `end`?
       *   - DISCRETE DOMAINS: An interval like [-1..1] represents 3 discrete cells (-1, 0, 1). Measuring distance by
       *     subtracting the `start` (-1) from the `end` (1) gives a measure of 2, which is off by one. By measuring to
       *     `end.rightAdjacent` = 2, the difference is the correct distance: 3. Rule of thumb: in discrete domains, we
       *     have to count the fence posts rather than the gaps between them.
       *   - CONTINUOUS DOMAINS: An interval like [-1.0, 1.0] has a measure of 2.0. This is correctly measured by
       *     subtracting the `start` from the `end`, i.e., 1.0 - (-1.0) = 2.0. The `rightAdjacent` of a closed end
       *     point ...1.0] is the open end point ...1.0), and vice versa. Crucially, the coordinate value DOES NOT
       *     CHANGE, it's still 1.0, so the measure of the interval does not change either. The calculated distance
       *     remains exactly 2.0 after `rightAdjacent` is applied. Rule of thumb: in continuous domains, we have to
       *     count the gaps between fence posts rather than the posts themselves.
       */
      lhs.start displacementTo lhs.end.rightAdjacent

    /**
      * Returns this interval scaled about some center scaled by some factor. The scaling factor must be a the scalar
      * type of T.
      *
      * Ignoring errors sometimes introduced by overflows and rounding, there are three invariants for this
      * transformation given any two intervals a and b, some scale s (non-zero, positive or negative), and some center c
      * (contained or not contained in a and/or b):
      *   - inversion: a ≡ scale(scale(a, s, c), 1/s, c) ≡ reflect(reflect(a, c), c)
      *   - adjacency: iff a is adjacent to b, scale(a, s, c) is adjacent to scale(b, s, c). If s is positive, they are
      *     adjacent in the same way (both left or both right), where if s is negative, adjacency flips (left becomes
      *     right or right becomes left )
      *   - measure: measure(scale(a, s, c)) = measure(a) * |s|
      *
      * The scaling of intervals in continuous domains is straightforward, but care is taken in the algorithm (i.e., use
      * of adjacency) for intervals in discrete domains to ensure these invariants.
      */
    def scaledAbout(center: Domain1D[T], scaledBy: op.Scalar): Option[Interval1D[T]] =
      val scaleIsPositive = scaledBy > op.zeroScalar
      val (start, end) = op match
        case _: ContinuousAffineValue[T] =>
          val (from, to) = if scaleIsPositive then (lhs.start, lhs.end) else (lhs.end, lhs.start)
          (from.scaledAbout(center, scaledBy), to.scaledAbout(center, scaledBy))
        case _: DiscreteAffineValue[T] =>
          val (from, to) =
            if scaleIsPositive then (lhs.start, lhs.end.rightAdjacent) else (lhs.end.rightAdjacent, lhs.start)
          (from.scaledAbout(center, scaledBy), to.scaledAbout(center, scaledBy).leftAdjacent)

      if Interval1D.validBounds(start, end) then Some(Interval1D(start, end)) else None

    /**
      * Reflects this interval about a pivot. If the result is not a valid interval, None is returned.
      *
      * Ignoring errors sometimes introduced by overflows and rounding, there are three invariants for this
      * transformation given any intervals a and b and some center c (contained or not contained in a and/or b):
      *   - adjacency: iff a is adjacent to b, reflect(a, c) is adjacent to reflect(b, c), and the adjacency type flips
      *     (left and right becomes right and left)
      *   - measure: measure(reflect(a, c)) = measure(a)
      *   - inversion: a = reflect(reflect(a, c), c)
      *
      * @note
      *   The reflecting of intervals in a continuous domain is straightforward, but the algorithm for reflecting in a
      *   discrete domain that maintains these invariants is slightly counterintuitive. Specifically the "center" is not
      *   a zero-width point, but a unit block. The line of symmetry for reflecting can be thought of as the left edge
      *   of this "block". E.g., reflecting the interval [0..1] about the center 3 results in the interval [4..5],
      *   because the left edge of 3 dictates that because the cell to the left its left edge (at 2) is empty, then the
      *   cell to the right of its left edge (at 3) is also empty.
      *
      * {{{
      *   Grid:         0   1   2   |   3   4   5   6
      *   Original:    [=====]  .   |   .  [=====]
      *   Pivot Axis:               |
      *                         ^       ^
      *                    1-block     1-block
      *                      gap         gap
      * }}}
      */
    infix def reflectedAbout(pivot: Domain1D[T]): Option[Interval1D[T]] = lhs.scaledAbout(pivot, op.reflectionScalar)

    /**
      * Displaces this interval. If the result is not a valid interval, None is returned.
      */
    infix def displacedBy(offset: op.Displacement): Option[Interval1D[T]] =
      val start = lhs.start displacedBy offset
      val end = lhs.end displacedBy offset
      if Interval1D.validBounds(start, end) then Some(Interval1D(start, end)) else None

    /**
      * Pads this interval at both the start and end. If the displacement is positive, the interval widens, and if the
      * displacement is negative, the interval shrinks. If the result is not a valid interval (e.g., pad to shrink more
      * than the interval width), None is returned.
      */
    infix def paddedBy(offset: op.Displacement): Option[Interval1D[T]] =
      val start = lhs.start displacedBy offset.negated
      val end = lhs.end displacedBy offset
      if Interval1D.validBounds(start, end) then Some(Interval1D(start, end)) else None

    /**
      * In mathematical morphology, a dilation operation uses a structuring element for probing and expanding a shape.
      * When applied to a one-dimensional interval, it positively pads the interval, potentially asymmetrically based on
      * the provided center of the structuring element. Depending on the choice of the center, the interval may also be
      * translated. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Dilation_(morphology)]]
      *
      * @param element
      *   a structuring element for the dilation.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   if the interval can be dilated, a new interval dilated by the structuring element, otherwise None.
      */
    def dilatedBy(element: Interval1D[T], elementCenter: Domain1D[T]): Option[Interval1D[T]] =
      val start = elementCenter displacementTo element.start match
        case Some(displacement) => lhs.start displacedBy displacement
        case None               => Bottom
      val end = elementCenter displacementTo element.end.rightAdjacent match
        case Some(displacement) => lhs.end displacedBy displacement
        case None               => Top
      if Interval1D.validBounds(start, end) then Some(Interval1D(start, end)) else None

    /**
      * In mathematical morphology, an erosion operation uses a structuring element for probing and contracting a shape.
      * When applied to a one-dimensional interval, it negatively pads the interval, potentially asymmetrically based on
      * the provided center of the structuring element. Depending on the choice of the center, the interval may also be
      * translated. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Erosion_(morphology)]]
      *
      * @param element
      *   a structuring element for the erosion.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   if the interval can be eroded, a new interval eroded by the structuring element, otherwise None.
      */
    def erodedBy(element: Interval1D[T], elementCenter: Domain1D[T]): Option[Interval1D[T]] =
      val start = element.start displacementTo elementCenter match
        case Some(displacement) => lhs.start displacedBy displacement
        case None               => Top
      val end = element.end.rightAdjacent displacementTo elementCenter match
        case Some(displacement) => lhs.end displacedBy displacement
        case None               => Bottom
      if Interval1D.validBounds(start, end) then Some(Interval1D(start, end)) else None

  extension [D <: NonEmptyTuple](lhs: Interval[D])(using op: DomainAffineLike[D])
    /**
      * Returns this interval reflected about some pivot.
      */
    def reflectedAbout(pivot: D): Option[Interval[D]] =
      op.applyToAffineDomain.reflectedAboutFromInterval(lhs, pivot)

    /**
      * Returns this interval displaced by some offset. The offset must be a tuple of the displacement types of D.
      */
    def displacedBy[S <: NonEmptyTuple](offset: S)(using D HasDisplacementType S): Option[Interval[D]] =
      op.applyToAffineDomain.displacedByFromInterval(lhs, offset)

    /**
      * Returns this interval padded by some offset. The interval end is displaced by the offset and the interval start
      * is displaced by the negation of the offset. The offset must be a tuple of the displacement types of D.
      */
    def paddedBy[S <: NonEmptyTuple](offset: S)(using D HasDisplacementType S): Option[Interval[D]] =
      op.applyToAffineDomain.paddedByFromInterval(lhs, offset)

    /**
      * In mathematical morphology, a dilation operation uses a structuring element for probing and expanding a shape.
      * When applied to a multidimensional interval, it positively pads the interval in all dimensions, potentially
      * asymmetrically based on the provided center of the structuring element. Depending on the choice of the center,
      * the interval may also be translated. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Dilation_(morphology)]]
      *
      * @param element
      *   a structuring element for the dilation.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   if the interval can be dilated, a new interval dilated by the structuring element, otherwise None.
      */
    def dilatedBy(element: Interval[D], elementCenter: D): Option[Interval[D]] =
      op.applyToAffineDomain.dilatedByFromInterval(lhs, element, elementCenter)

    /**
      * In mathematical morphology, an erosion operation uses a structuring element for probing and contracting a shape.
      * When applied to a multidimensional interval, it negatively pads the interval in all dimensions, potentially
      * asymmetrically based on the provided center of the structuring element. Depending on the choice of the center,
      * the interval may also be translated. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Erosion_(morphology)]]
      *
      * @param element
      *   a structuring element for the erosion.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   if the interval can be eroded, a new interval eroded by the structuring element, otherwise None.
      */
    def erodedBy(element: Interval[D], elementCenter: D): Option[Interval[D]] =
      op.applyToAffineDomain.erodedByFromInterval(lhs, element, elementCenter)

    /**
      * Returns the measure of this interval as a tuple of the displacement types of D if the measures are defined in
      * all dimensions. Returns None otherwise.
      */
    def measure[S <: NonEmptyTuple](using D HasDisplacementType S): Option[S] =
      op.applyToAffineDomain.measureFromInterval(lhs)

    /**
      * Maps the measure of this interval to some result type if the measures are defined in all dimensions. Returns
      * None otherwise.
      */
    def mapMeasure[S <: NonEmptyTuple, R](f: S => R)(using D HasDisplacementType S): Option[R] =
      op.applyToAffineDomain.measureFromInterval(lhs).map(f)

    /**
      * Returns this interval scaled about some center scaled by some factor. The scaling factor must be a tuple of the
      * scalar types of D.
      */
    def scaledAbout[S <: NonEmptyTuple](center: D, scaledBy: S)(using D HasScalarType S): Option[Interval[D]] =
      op.applyToAffineDomain.scaledAboutFromInterval(lhs, center, scaledBy)

  extension [D <: NonEmptyTuple](lhs: IntervalShape[D])(using op: DomainAffineLike[D])
    /**
      * Returns an interval shape consisting of this shape's interval components reflected about some pivot. For any
      * components where the reflection is not a valid interval (e.g., starts at Top), those components are excluded
      * from the result.
      */
    def reflectedAbout(pivot: D): IntervalShape[D] =
      val transform = ((_: Interval[D]).reflectedAbout(pivot)).unlift
      lhs.collect(transform)(using altConfig = lhs.config)

    /**
      * Returns an interval shape consisting of this shape's interval components displaced by some offset. For any
      * components where the displacement is not a valid interval (e.g., starts at Top), those components are excluded
      * from the result. The offset must be a tuple of the displacement types of D.
      */
    def displacedBy[S <: NonEmptyTuple](offset: S)(using D HasDisplacementType S): IntervalShape[D] =
      val transform = ((_: Interval[D]).displacedBy(offset)).unlift
      lhs.collect(transform)(using altConfig = lhs.config)

    /**
      * Returns an interval shape consisting of this shape's interval components scaled abound some center by some
      * factor. For any components where the scaling is not a valid interval (e.g., starts at Top), those components are
      * excluded from the result. The scaling factor must be a tuple of the scalar types of D.
      */
    def scaledAbout[S <: NonEmptyTuple](center: D, scaledBy: S)(using D HasScalarType S): IntervalShape[D] =
      val transform = ((_: Interval[D]).scaledAbout(center, scaledBy)).unlift
      IntervalShape.∅(using config = lhs.config) ++ lhs.allIntervals.collect(transform)

    /**
      * The shape forming a shell around the boundary of all valid data. The shell's thickness and direction are
      * determined per dimension by the given displacement vector. The resulting shell represents the symmetric
      * difference of the original shape and the same shape padded with the provided thickness.
      *
      * When all thickness components are positive (the common case), the resulting shape will be adjacent to valid data
      * everywhere, but not intersecting it anywhere.
      *
      * @note
      *   If data are valid on the boundaries of the domain in any dimension, the resulting shell may not be contiguous.
      *
      * @param thickness
      *   A tuple of displacements representing the shell's thickness in each dimension. Positive values expand the
      *   boundaries outward, while negative values contract them inward. Dimensions can mix positive and negative
      *   displacements.
      *
      * @return
      *   A shape that forms a shell around the boundary of all valid data.
      */
    def boundingShape[S <: NonEmptyTuple](thickness: S)(using D HasDisplacementType S): IntervalShape[D] =
      val paddedIntervals = lhs.allIntervals.flatMap(_.paddedBy(thickness))
      (IntervalShape.∅(using config = lhs.config) ++ paddedIntervals) △ lhs

    /**
      * In mathematical morphology, a dilation operation uses a structuring element for probing and expanding a shape.
      * When applied to an interval shape, it positively pads all the interval components of the shape, potentially
      * asymmetrically based on the provided center of the structuring element, and collects the result as a new shape.
      * Depending on the choice of the center, the shape may also be translated. See
      * [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Dilation_(morphology)]]
      *
      * @param element
      *   a structuring element for the dilation.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   a new shape with all components dilated by the structuring element -- any components that cannot be dialated
      *   are dropped in the result.
      */
    def dilatedBy(element: Interval[D], elementCenter: D): IntervalShape[D] =
      val dilatedIntervals = lhs.allIntervals.flatMap(_.dilatedBy(element, elementCenter))
      IntervalShape.∅(using config = lhs.config) ++ dilatedIntervals

    /**
      * In mathematical morphology, an erosion operation uses a structuring element for probing and contracting a shape.
      * When applied to an interval shape, it negatively pads all the interval components of the shape, potentially
      * asymmetrically based on the provided center of the structuring element, and collects the result as a new shape.
      * Depending on the choice of the center, the shape may also be translated. See
      * [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Erosion_(morphology)]]
      *
      * @note
      *   because eroding a shape is just the erosion applied to each of its interval components in the shape, the
      *   results in more than one dimension can be surprising. That is because a shape with intervals partially
      *   adjacent to one another can wind up with gaps between them when eroded.
      *
      * @param element
      *   a structuring element for the erosion.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   a new shape with all components eroded by the structuring element -- any components that cannot be eroded are
      *   dropped in the result.
      */
    def erodedBy(element: Interval[D], elementCenter: D): IntervalShape[D] =
      val transform = ((_: Interval[D]).erodedBy(element, elementCenter)).unlift
      lhs.collect(transform)(using altConfig = lhs.config)

    /**
      * In mathematical morphology, an opening operation is the dilation of the erosion, essentially removing features
      * smaller than the structuring element. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Opening_(morphology)]]
      *
      * @param element
      *   a structuring element for the erosion and subsequent dilation.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   a new shape with all components eroded by, and subsequently dialated by, the structuring element -- any
      *   components that cannot be eroded or dialated are dropped in the result.
      */
    def openingBy(element: Interval[D], elementCenter: D): IntervalShape[D] =
      erodedBy(element, elementCenter).dilatedBy(element, elementCenter)

    /**
      * In mathematical morphology, a closing operation is the erosion of the dilation, essentially removing gaps
      * smaller than the structuring element. See [[https://en.wikipedia.org/wiki/Mathematical_morphology]] and
      * [[https://en.wikipedia.org/wiki/Closing_(morphology)]]
      *
      * @note
      *   because the second step is eroding, the results in more than one dimension can be surprising (gaps open up
      *   where interval components had been partially adjacent). See note on [[erodedBy]] for more details.
      *
      * @param element
      *   a structuring element for the dilation and subsequent erosion.
      * @param elementCenter
      *   the center of the structuring element.
      * @return
      *   a new shape with all components dialated by, and subsequently eroded by, the structuring element -- any
      *   components that cannot be dialated or eroded are dropped in the result.
      */
    def closingBy(element: Interval[D], elementCenter: D): IntervalShape[D] =
      dilatedBy(element, elementCenter).erodedBy(element, elementCenter)

    /**
      * Returns an accumulated measure of this interval shape by measuring this shape's interval components (tuples of
      * the displacement types of D), mapping each measurement to the result type, and reducing those results to a
      * single result by combining them. Any components for which the measure is not defined in all dimensions are
      * excluded from the result. Returns None if no components are measurable.
      */
    def mapReduceMeasure[S <: NonEmptyTuple, R](
      f: S => R,
      combine: (R, R) => R
    )(using D HasDisplacementType S): Option[R] =
      val measure = ((_: Interval[D]).mapMeasure(f)).unlift
      lhs.allIntervals.collect(measure).reduceOption(combine)
