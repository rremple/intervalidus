package intervalidus

import intervalidus.collection.{Coordinate, CoordinateFixed}

import scala.Tuple.{Append, Concat, Drop, Elem, Take}
import scala.compiletime.ops.int.S

/**
  * Type class with operations on a domain with multiple discrete and/or continuous dimensions.
  *
  * An n-dimensional domain is represented by a tuple of `Domain1D[T`<sup>i</sup>`]` values (where i varies from 1 to
  * n), and each `T`<sup>i</sup> is a (potentially different) domain value type that is `DomainValueLike`.
  *
  * An n-dimensional domain is used in defining the boundaries of an n-dimensional interval. Generally, you will not
  * need to use these methods directly -- they are here primarily to support methods on `Interval` and `ValidData`.
  *
  * @tparam D
  *   the domain type -- a non-empty tuple of one-dimensional domains, where each can have its own domain value type.
  *
  * @define minMaxNote
  *   Because each dimension is evaluated independently, there is no guarantee the result will equal either of the
  *   arguments.
  */
trait DomainLike[D <: NonEmptyTuple]:
  /*
   * Type-level capabilities
   */

  /**
    * @return
    *   Multidimensional top value.
    */
  def top: D

  /**
    * @return
    *   Multidimensional bottom value.
    */
  def bottom: D

  /**
    * @return
    *   The number of dimensions represented in this domain.
    */
  def arity: Int

  /*
   * Valid data-like capabilities
   */

  /**
    * Used by the `toString` on `Data`
    *
    * @param validData
    *   valid data to represent
    * @tparam V
    *   the valid data value type
    * @return
    *   a tuple of strings:
    *   1. first dimension start string
    *   1. first dimension end string
    *   1. value + remaining dimension string (the content of the grid)
    */
  def validDataPreprocessForGrid[V](validData: ValidData[V, D]): (String, String, String)

  /**
    * Leverages the ordering of each constituent domain value to compare one domain with another.
    *
    * @param x
    *   lhs domain
    * @param y
    *   rhs domain
    * @return
    *   0 if all dimensions of x and y are equal; a value less than 0 if, in the first dimension where x and y are not
    *   equal, the x component in that dimension is less than the corresponding y component; and a value greater than 0
    *   otherwise.
    */
  def compareDomains(x: D, y: D): Int

  /*
   * Interval-like capabilities
   */

  /**
    * Used by the `toString` on `Data`
    *
    * @param intervals
    *   intervals to represent
    * @return
    *   a collection of tuples of strings:
    *   1. first dimension start string
    *   1. first dimension end string
    *   1. interval grid-formatted string
    */
  def intervalPreprocessForGrid(intervals: Iterable[Interval[D]]): Iterable[(String, String, String)]

  /**
    * Determines if the before interval is to the "left" of (i.e., horizontally to the left, or vertically below, or
    * depth-wise behind, etc., depending on dimension) the after interval.
    * @param beforeInterval
    *   interval to test for "beforeness"
    * @param afterInterval
    *   interval to test for "afterness"
    * @return
    *   true if the before interval is "left" adjacent to the after interval
    */
  infix def intervalIsLeftAdjacentTo(beforeInterval: Interval[D], afterInterval: Interval[D]): Boolean

  /**
    * @return
    *   a tuple of remainders in each dimension.
    */
  def intervalExcluding(thisInterval: Interval[D], thatInterval: Interval[D]): NonEmptyTuple

  /**
    * @return
    *   a collection of disjoint intervals covering thisInterval separated by the boundaries of thatInterval.
    */
  def intervalSeparateUsing(thisInterval: Interval[D], thatInterval: Interval[D]): Iterable[Interval[D]]

  /**
    * @return
    *   gap between thisInterval and thatInterval, if one exists.
    */
  def intervalGapWith(thisInterval: Interval[D], thatInterval: Interval[D]): Option[Interval[D]]

  /**
    * @return
    *   all intervals, including all overlaps and gaps between intervals, as intervals. Inputs may be overlapping. The
    *   result is disjoint and covers the span of the input intervals.
    */
  def intervalUniqueIntervals(intervals: Iterable[Interval[D]]): Iterable[Interval[D]]

  /**
    * Keys of all possible right-adjacent intervals. (Supports interval compression.)
    * @return
    *   a list of keys for the only possible intervals that could be adjacent to this interval.
    */
  def intervalRightAdjacentKeys(interval: Interval[D]): List[D]

  /**
    * @return
    *   a code-like string for this interval, with or without parens.
    */
  def intervalToCodeLikeString(interval: Interval[D], withParens: Boolean): String

  /**
    * @return
    *   a mathematical representation of this interval, with or without braces.
    */
  def intervalToString(interval: Interval[D], withBraces: Boolean): String

  /**
    * Checks if these start and end domains would form a valid interval
    */
  def validIntervalBounds(start: D, end: D): Boolean

  /*
   * Domain-like capabilities
   */

  extension (domain: D)
    // to equate type-level integer successor with the incremented value of an integer
    private def s(arg: Int): S[arg.type] = (arg + 1).asInstanceOf[S[arg.type]]

    /**
      * Extract a lower-dimensional domain by dropping a dimension.
      *
      * @param dimensionIndex
      *   the dimension to drop (e.g., the head dimension is index 0)
      * @tparam R
      *   the result domain. There is a type safety check that ensures the domain type for the result dimension is a
      *   concatenation of elements before and after the dropped dimension.
      * @return
      *   a new lower-dimensional domain
      */
    def dropDimension[R <: NonEmptyTuple: DomainLike](dimensionIndex: Int)(using
      Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
    ): R = domain.take(dimensionIndex) ++ domain.drop(s(dimensionIndex))

    /**
      * Create a higher-dimensional domain by inserting a dimension.
      *
      * @param dimensionIndex
      *   the dimension where the domain is inserted (e.g., inserting a new head dimension is index 0). Existing
      *   dimensions are pushed to the right.
      * @param domain1D
      *   the domain to be inserted
      * @tparam H
      *   the domain value type of the domain inserted
      * @tparam R
      *   the result domain. There is a type safety check that ensures the domain type for the result dimension is a
      *   concatenation of elements before the insert, the inserted domain, and the elements after the insert.
      * @return
      *   a new higher-dimensional domain
      */
    def insertDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
      dimensionIndex: Int,
      domain1D: Domain1D[H]
    )(using
      Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, dimensionIndex.type]] =:= R
    ): R = domain.take(dimensionIndex) ++ (domain1D *: domain.drop(dimensionIndex))

    /**
      * Update the domain at a particular dimension.
      *
      * @param dimensionIndex
      *   the dimension in which the domain is updated (e.g., the head dimension is index 0)
      * @param updated
      *   the domain updated
      * @tparam H
      *   the domain value type of the domain updated. There are type safety checks that ensure
      *   - the domain at the specified dimension index is of the specified domain type, and
      *   - the domain type for the result dimension is a concatenation of elements before the update, the updated
      *     domain, and the elements after the update.
      * @return
      *   a new domain of the same dimension with the update applied
      */
    def updateDimension[H: DomainValueLike](
      dimensionIndex: Int,
      updated: Domain1D[H]
    )(using
      Elem[D, dimensionIndex.type] =:= Domain1D[H],
      Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D
    ): D = domain.take(dimensionIndex) ++ (updated *: domain.drop(s(dimensionIndex)))

    infix def equiv(that: D): Boolean

    /**
      * Every possible point bounded by the domain and end.
      * @note
      *   if the domain is continuous in any dimension, no points are returned.
      */
    def pointsTo(end: D): Iterable[D]

    /**
      * Tests if the domain is infinite (i.e., `Top` or `Bottom`) in all dimensions.
      */
    def isUnbounded: Boolean

    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String

    /**
      * Approximate this domain as a (potentially unfixed) coordinate in double space based on the domain ordered hash.
      *
      * @return
      *   a new coordinate for boxes managed in box search trees
      */
    def asCoordinate: Coordinate

    /**
      * Approximate this domain as a fixed coordinate in double space based on the domain ordered hash.
      *
      * @return
      *   a new coordinate for box tree boundary capacities
      */
    def asCoordinateFixed: CoordinateFixed

    /**
      * Domain adjacent to this in all dimensions from the "right", where `Bottom` and `Top` are considered
      * self-adjacent. For discrete domains, this is the successor in all dimensions, where the right adjacent of
      * `maxValue` is `Top` -- see [[https://en.wikipedia.org/wiki/Successor_function]]. For continuous domains, this
      * maps open points in all dimensions to closed ones, and closed points to open ones (right and left complements
      * are the same).
      *
      * @return
      *   right complement of this
      */
    def rightAdjacent: D

    /**
      * Domain adjacent to this in all dimensions from the "left", where `Bottom` and `Top` are considered
      * self-adjacent. For discrete domains, this is the predecessor in all dimensions, where the left adjacent of
      * `minValue` is `Bottom` -- see [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]]. For
      * continuous domains, this maps open points in all dimensions to closed ones, and closed points to open ones
      * (right and left complements are the same).
      *
      * @return
      *   left complement of this
      */
    def leftAdjacent: D

    /**
      * In each dimension, any open points are converted to closed points at the same value. Unbounded dimensions are
      * left unchanged.
      */
    def closeIfOpen: D

    // enhanced

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name.
      */
    def asString: String

    /**
      * Is this start after that start, using default ordering to compare starts?
      */
    infix def afterStart(that: D): Boolean

    /**
      * Is this start after or equal to that start in all dimensions, using default ordering to compare starts?
      */
    infix def afterOrAtStart(that: D): Boolean

    /**
      * Is this end before that end in all dimensions, using non-default ordering to compare ends?
      */
    infix def beforeEnd(that: D): Boolean

    /**
      * Is this end before or equal to that end in all dimensions, using non-default ordering to compare ends?
      */
    infix def beforeOrAtEnd(that: D): Boolean

    /**
      * Find the max domain components in each dimension of two starts, treating open > closed at the same point by
      * using the default ordering.
      * @note
      *   $minMaxNote
      */
    infix def maxStart(that: D): D

    /**
      * Find the max domain components in each dimension of two ends, treating open < closed at the same point by using
      * the non-default ordering.
      * @note
      *   $minMaxNote
      */
    infix def maxEnd(that: D): D

    /**
      * Find the min domain components in each dimension of two starts, treating open > closed at the same point by
      * using the default ordering.
      * @note
      *   $minMaxNote
      */
    infix def minStart(that: D): D

    /**
      * Find the min domain components in each dimension of two ends, treating open < closed at the same point by using
      * the non-default ordering.
      * @note
      *   $minMaxNote
      */
    infix def minEnd(that: D): D

    /**
      * Test if this domain consists of only closed or unbounded points in all dimensions.
      */
    def isClosedOrUnbounded: Boolean

    // concrete

    /**
      * Appends that domain to this one.
      */
    infix def x[X: DomainValueLike](that: Domain1D[X]): Append[D, Domain1D[X]] = domain :* that

    /**
      * Prepends that domain to this one.
      */
    infix def withHead[X: DomainValueLike](that: Domain1D[X]): Domain1D[X] *: D = that *: domain

    /**
      * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
      *
      * @param interval
      *   interval to test.
      * @return
      *   true if this belongs to the specified interval, false otherwise.
      */
    infix def belongsTo(interval: Interval[D]): Boolean = interval contains domain

    // equivalent symbolic method names

    /**
      * Same as [[belongsTo]]
      *
      * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
      *
      * @param interval
      *   interval to test.
      * @return
      *   true if this belongs to the specified interval, false otherwise.
      */
    def ∈(interval: Interval[D]): Boolean = domain belongsTo interval

object DomainLike:
  given domainOrdering[D <: NonEmptyTuple](using domainLike: DomainLike[D]): Ordering[D] with
    override def compare(x: D, y: D): Int = domainLike.compareDomains(x, y)

  given [D <: NonEmptyTuple](using applyToDomain: DomainLikeTupleOps[D]): DomainLike[D] with

    /*
     * Type-level capabilities
     */

    override inline def top: D = applyToDomain.unbounded(Domain1D.Top)

    override inline def bottom: D = applyToDomain.unbounded(Domain1D.Bottom)

    override inline def arity: Int = applyToDomain.arity

    /*
     * Valid data-like capabilities
     */

    override inline def validDataPreprocessForGrid[V](validData: ValidData[V, D]): (String, String, String) =
      applyToDomain.preprocessForGridFromValidData(validData)

    /*
     * Interval-like capabilities
     */
    override inline def compareDomains(x: D, y: D): Int =
      applyToDomain.compareDomains(x, y)

    override inline def intervalPreprocessForGrid(
      intervals: Iterable[Interval[D]]
    ): Iterable[(String, String, String)] =
      applyToDomain.preprocessForGridFromIntervals(intervals)

    override inline infix def intervalIsLeftAdjacentTo(
      beforeInterval: Interval[D],
      afterInterval: Interval[D]
    ): Boolean =
      val (equivalency, adjacency, total) = applyToDomain.equivalencyAndAdjacencyFromIntervals(
        beforeInterval,
        afterInterval
      )
      adjacency == 1 && equivalency == total - 1

    override inline def intervalExcluding(thisInterval: Interval[D], thatInterval: Interval[D]): NonEmptyTuple =
      applyToDomain.excludingFromIntervals(
        thisInterval,
        thatInterval
      )

    override inline def intervalSeparateUsing(
      thisInterval: Interval[D],
      thatInterval: Interval[D]
    ): Iterable[Interval[D]] = applyToDomain
      .separateUsingFromIntervals(
        thisInterval,
        thatInterval
      )

    override inline def intervalGapWith(
      thisInterval: Interval[D],
      thatInterval: Interval[D]
    ): Option[Interval[D]] =
      applyToDomain.gapWithFromIntervals(
        thisInterval,
        thatInterval
      )

    override inline def intervalUniqueIntervals(
      intervals: Iterable[Interval[D]]
    ): Iterable[Interval[D]] =
      applyToDomain.uniqueIntervalsFromInterval(intervals)

    // not inline to avoid warning: "New anonymous class definition will be duplicated at each inline site"
    private val withOneSwap: PartialFunction[(D, Int), D] =
      case (domain, swaps) if swaps == 1 => domain

    override inline def intervalRightAdjacentKeys(interval: Interval[D]): List[D] =
      applyToDomain.rightAdjacentKeysFromInterval(interval).collect(withOneSwap)

    override inline def intervalToCodeLikeString(interval: Interval[D], withParens: Boolean): String =
      applyToDomain.toCodeLikeStringsFromInterval(interval) match
        case single :: Nil          => single
        case multiple if withParens => multiple.mkString("(", " x ", ")")
        case multiple               => multiple.mkString(" x ")

    override inline def intervalToString(interval: Interval[D], withBraces: Boolean): String =
      applyToDomain.toStringsFromInterval(interval) match
        case single :: Nil          => single
        case multiple if withBraces => multiple.mkString("{", ", ", "}")
        case multiple               => multiple.mkString(", ")

    override inline def validIntervalBounds(startDomainTuple: D, endDomainTuple: D): Boolean =
      applyToDomain.validIntervalBoundsFromDomains(startDomainTuple, endDomainTuple)

    extension (domainTuple: D)

      /*
       * Basic domain-like capabilities
       */
      override inline infix def equiv(thatDomainTuple: D): Boolean =
        applyToDomain.equivFromDomains(domainTuple, thatDomainTuple)
      override inline def pointsTo(endDomainTuple: D): Iterable[D] =
        applyToDomain.pointsFromDomains(domainTuple, endDomainTuple)
      override inline def isUnbounded: Boolean =
        applyToDomain.isUnboundedFromDomain(domainTuple)
      override inline def toCodeLikeString: String =
        applyToDomain.toCodeLikeStringsFromDomain(domainTuple).mkString(" x ")
      override inline def asCoordinate: Coordinate =
        Coordinate(applyToDomain.unfixedOrderedHashesFromDomain(domainTuple).toVector)
      override inline def asCoordinateFixed: CoordinateFixed =
        CoordinateFixed(applyToDomain.orderedHashesFromDomain(domainTuple).toVector)
      override inline def rightAdjacent: D =
        applyToDomain.rightAdjacentFromDomain(domainTuple)
      override inline def leftAdjacent: D =
        applyToDomain.leftAdjacentFromDomain(domainTuple)
      override inline def closeIfOpen: D =
        applyToDomain.closeIfOpenFromDomain(domainTuple)

      /*
       * Enhanced domain-like capabilities
       */
      override inline def asString: String = applyToDomain.toStringsFromDomain(domainTuple) match
        case one :: Nil => one
        case notOne     => notOne.mkString("{", ", ", "}")

      infix override inline def afterStart(thatDomainTuple: D): Boolean =
        applyToDomain.afterStartFromDomains(domainTuple, thatDomainTuple)

      infix override inline def afterOrAtStart(thatDomainTuple: D): Boolean =
        applyToDomain.afterOrAtStartFromDomains(domainTuple, thatDomainTuple)

      infix override inline def beforeEnd(thatDomainTuple: D): Boolean =
        applyToDomain.beforeEndFromDomains(domainTuple, thatDomainTuple)

      infix override inline def beforeOrAtEnd(thatDomainTuple: D): Boolean =
        applyToDomain.beforeOrAtEndFromDomains(domainTuple, thatDomainTuple)

      infix override inline def maxStart(tuple2: D): D =
        applyToDomain.maxStartFromDomains(domainTuple, tuple2)

      infix override inline def minEnd(tuple2: D): D =
        applyToDomain.minEndFromDomains(domainTuple, tuple2)

      infix override inline def minStart(tuple2: D): D =
        applyToDomain.minStartFromDomains(domainTuple, tuple2)

      infix override inline def maxEnd(tuple2: D): D =
        applyToDomain.maxEndFromDomains(domainTuple, tuple2)

      override inline def isClosedOrUnbounded: Boolean =
        applyToDomain.isClosedOrUnboundedFromDomain(domainTuple)
