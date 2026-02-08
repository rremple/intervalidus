package intervalidus

import intervalidus.collection.{Coordinate, CoordinateFixed}

/**
  * Type class with operations on a domain with multiple discrete and/or continuous dimensions.
  *
  * An n-dimensional domain is represented by a tuple of `Domain1D[T`<sup>i</sup>`]` values (where 'i' varies from 1 to
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
class DomainLike[D <: NonEmptyTuple](using applyToDomain: DomainLikeTupleOps[D]):

  /*
   * Domain-like capabilities
   */

  extension (domain: D)
    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String = applyToDomain.toCodeLikeStringsFromDomain(domain).mkString(" x ")

    /**
      * Tests if the domain is infinite (i.e., `Top` or `Bottom`) in all dimensions.
      */
    def isUnbounded: Boolean = applyToDomain.isUnboundedFromDomain(domain)

    /**
      * Test if this domain consists of only closed or unbounded points in all dimensions.
      */
    def isClosedOrUnbounded: Boolean = applyToDomain.isClosedOrUnboundedFromDomain(domain)

    /**
      * In each dimension, any open points are converted to closed points at the same value. Unbounded dimensions are
      * left unchanged.
      */
    def closeIfOpen: D = applyToDomain.closeIfOpenFromDomain(domain)

    /**
      * Every possible point bounded by this domain and the end domain.
      * @note
      *   if this domain is continuous in any dimension, no points are returned.
      */
    def pointsTo(end: D): Iterable[D] = applyToDomain.pointsFromDomains(domain, end)

    /**
      * Domain adjacent to this domain in all dimensions from the "left", where `bottom` and `top` are considered
      * self-adjacent. For discrete domain dimensions, this is the predecessor, where the left adjacent of `minValue` is
      * `Bottom` -- see [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]]. For continuous
      * domain dimensions, this maps open points to closed ones, and closed points to open ones (right and left adjacent
      * domains are the same).
      *
      * @return
      *   left complement of this
      */
    def leftAdjacent: D = applyToDomain.leftAdjacentFromDomain(domain)

    /**
      * Domain adjacent to this in all dimensions from the "right", where `bottom` and `top` are considered
      * self-adjacent. For discrete domain dimensions, this is the successor, where the right adjacent of `maxValue` is
      * `Top` -- see [[https://en.wikipedia.org/wiki/Successor_function]]. For continuous domain dimensions, this maps
      * open points to closed ones, and closed points to open ones (right and left adjacent domains are the same).
      *
      * @return
      *   right complement of this
      */
    def rightAdjacent: D = applyToDomain.rightAdjacentFromDomain(domain)

    /**
      * Is this start after that start, using default ordering to compare starts?
      */
    infix def afterStart(that: D): Boolean = applyToDomain.afterStartFromDomains(domain, that)

    /**
      * Is this start after or equal to that start in all dimensions, using default ordering to compare starts?
      */
    infix def afterOrAtStart(that: D): Boolean = applyToDomain.afterOrAtStartFromDomains(domain, that)

    /**
      * Is this end before that end in all dimensions, using non-default ordering to compare ends?
      */
    infix def beforeEnd(that: D): Boolean = applyToDomain.beforeEndFromDomains(domain, that)

    /**
      * Is this end before or equal to that end in all dimensions, using non-default ordering to compare ends?
      */
    infix def beforeOrAtEnd(that: D): Boolean = applyToDomain.beforeOrAtEndFromDomains(domain, that)

    /**
      * Find the max domain components in each dimension of two starts, treating open > closed at the same point by
      * using the default ordering.
      * @note
      *   $minMaxNote
      */
    infix def maxStart(that: D): D = applyToDomain.maxStartFromDomains(domain, that)

    /**
      * Find the max domain components in each dimension of two ends, treating open < closed at the same point by using
      * the non-default ordering.
      * @note
      *   $minMaxNote
      */
    infix def maxEnd(that: D): D = applyToDomain.maxEndFromDomains(domain, that)

    /**
      * Find the min domain components in each dimension of two starts, treating open > closed at the same point by
      * using the default ordering.
      * @note
      *   $minMaxNote
      */
    infix def minStart(that: D): D = applyToDomain.minStartFromDomains(domain, that)

    /**
      * Find the min domain components in each dimension of two ends, treating open < closed at the same point by using
      * the non-default ordering.
      * @note
      *   $minMaxNote
      */
    infix def minEnd(that: D): D = applyToDomain.minEndFromDomains(domain, that)

    /**
      * Appends that domain to this one.
      */
    infix def x[X: DomainValueLike](that: Domain1D[X]): Domain.Appended[D, X] = domain :* that

    /**
      * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
      *
      * @param interval
      *   interval to test.
      * @return
      *   true if this belongs to the specified interval, false otherwise.
      */
    infix def belongsTo(interval: Interval[D]): Boolean = interval contains domain

    /*
     * Specific to multidimensional domains.
     */

    /**
      * Approximate this domain as a fixed coordinate in double space based on the domain ordered hash.
      *
      * @return
      *   a new coordinate for box tree boundary capacities
      */
    def asCoordinateFixed: CoordinateFixed = CoordinateFixed(applyToDomain.fixedOrderedHashesFromDomain(domain))

    /**
      * Approximate this domain as a (potentially unfixed) coordinate in double space based on the domain ordered hash.
      *
      * @return
      *   a new coordinate for boxes managed in box search trees
      */
    def asCoordinateUnfixed: Coordinate = Coordinate(applyToDomain.unfixedOrderedHashesFromDomain(domain))

    /**
      * Can't override `toString` through an extension method, so we give it a slightly different name.
      */
    def asString: String = applyToDomain.toStringsFromDomain(domain) match
      case one :: Nil => one
      case notOne     => notOne.mkString("{", ", ", "}")

    /**
      * Prepends that domain to this one.
      */
    infix def withHead[X: DomainValueLike](that: Domain1D[X]): Domain1D[X] *: D = that *: domain

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
    def dropDimension[R <: NonEmptyTuple: DomainLike](dimensionIndex: Domain.DimensionIndex)(using
      Domain.HasIndex[D, dimensionIndex.type],
      Domain.IsDroppedInResult[D, dimensionIndex.type, R]
    ): R = domain.take(dimensionIndex) ++ domain.drop(dimensionIndex).drop(1)

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
      dimensionIndex: Domain.DimensionIndex,
      domain1D: Domain1D[H]
    )(using
      Domain.HasIndex[R, dimensionIndex.type],
      Domain.IsInsertedInResult[D, dimensionIndex.type, H, R]
    ): R = domain.take(dimensionIndex) ++ (domain1D *: domain.drop(dimensionIndex))

    /**
      * Swap the one-dimensional domains at the indexed dimensions.
      *
      * @param dimensionIndex1
      *   the lesser index of the one-dimensional domain to swap
      * @param dimensionIndex2
      *   the greater index of the one-dimensional domain to swap
      * @tparam R
      *   the multidimensional domain result after the one-dimensional domains are swapped
      */
    def swapDimensions[R <: NonEmptyTuple: DomainLike](
      dimensionIndex1: Domain.DimensionIndex,
      dimensionIndex2: Domain.DimensionIndex
    )(using
      Domain.HasSwappableIndexes[D, dimensionIndex1.type, dimensionIndex2.type],
      Domain.IsSwappedInResult[D, dimensionIndex1.type, dimensionIndex2.type, R]
    ): R = domain.take(dimensionIndex1) ++ (
      (domain(dimensionIndex2) *: domain.take(dimensionIndex2).drop(dimensionIndex1).drop(1)) ++
        (domain(dimensionIndex1) *: domain.drop(dimensionIndex2).drop(1))
    )

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
      dimensionIndex: Domain.DimensionIndex,
      updated: Domain1D[H]
    )(using
      Domain.HasIndex[D, dimensionIndex.type],
      Domain.IsAtIndex[D, dimensionIndex.type, H],
      Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H]
    ): D = domain.take(dimensionIndex) ++ (updated *: domain.drop(dimensionIndex).drop(1))

    /*
     * Equivalent symbolic method names
     */

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

  /*
   * Type-level domain capabilities
   */

  /**
    * @return
    *   Multidimensional top value.
    */
  def top: D = applyToDomain.unbounded(Domain1D.Top)

  /**
    * @return
    *   Multidimensional bottom value.
    */
  def bottom: D = applyToDomain.unbounded(Domain1D.Bottom)

  /**
    * @return
    *   The number of dimensions represented in this domain.
    */
  def arity: Int = applyToDomain.arity

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
  def compareDomains(x: D, y: D): Int = applyToDomain.compareDomains(x, y)

  /*
   * Interval-like capabilities (used internally)
   */

  /**
    * Internal method. Determines if the before interval is to the "left" of (i.e., horizontally to the left, or
    * vertically below, or depth-wise behind, etc., depending on dimension) the after interval.
    * @param beforeInterval
    *   interval to test for "beforeness"
    * @param afterInterval
    *   interval to test for "afterness"
    * @return
    *   true if the before interval is "left" adjacent to the after interval
    */
  infix def intervalIsLeftAdjacentTo(beforeInterval: Interval[D], afterInterval: Interval[D]): Boolean =
    val (equivalency, adjacency, total) = applyToDomain.equivalencyAndAdjacencyFromIntervals(
      beforeInterval,
      afterInterval
    )
    adjacency == 1 && equivalency == total - 1

  /**
    * Internal method. @return a tuple of remainders in each dimension.
    */
  def intervalExcluding(thisInterval: Interval[D], thatInterval: Interval[D]): Interval.Remainder[D] =
    applyToDomain.excludingFromIntervals(thisInterval, thatInterval)

  /**
    * Internal method. @return a collection of disjoint intervals covering thisInterval separated by the boundaries of
    * thatInterval.
    */
  def intervalSeparateUsing(thisInterval: Interval[D], thatInterval: Interval[D]): Iterable[Interval[D]] =
    applyToDomain.separateUsingFromIntervals(thisInterval, thatInterval)

  /**
    * Internal method. @return gap between thisInterval and thatInterval, if one exists.
    */
  def intervalGapWith(thisInterval: Interval[D], thatInterval: Interval[D]): Option[Interval[D]] =
    applyToDomain.gapWithFromIntervals(thisInterval, thatInterval)

  /**
    * Internal method. @return all intervals, including all overlaps and gaps between intervals, as intervals. Inputs
    * may be overlapping. The result is disjoint and covers the span of the input intervals.
    */
  def intervalUniqueIntervals(intervals: Iterable[Interval[D]]): Iterable[Interval[D]] =
    applyToDomain.uniqueIntervalsFromInterval(intervals)

  /**
    * Internal method. Keys of all possible right-adjacent intervals. (Supports interval compression.)
    * @return
    *   a list of keys for the only possible intervals that could be adjacent to this interval.
    */
  def intervalRightAdjacentKeys(interval: Interval[D]): List[D] =
    applyToDomain
      .rightAdjacentKeysFromInterval(interval)
      .collect:
        case (domain, swaps) if swaps == 1 => domain

  /**
    * Internal method. @return a code-like string for this interval, with or without parens.
    */
  def intervalToCodeLikeString(interval: Interval[D], withParens: Boolean): String =
    applyToDomain.toCodeLikeStringsFromInterval(interval) match
      case single :: Nil          => single
      case multiple if withParens => multiple.mkString("(", " x ", ")")
      case multiple               => multiple.mkString(" x ")

  /**
    * Internal method. @return a mathematical representation of this interval.
    */
  def intervalToString(interval: Interval[D]): String =
    applyToDomain.toStringsFromInterval(interval) match
      case single :: Nil => single
      case multiple      => multiple.mkString("{", ", ", "}")

  /**
    * Internal method. Checks if these start and end domains would form a valid interval
    */
  def validIntervalBounds(start: D, end: D): Boolean =
    applyToDomain.validIntervalBoundsFromDomains(start, end)

  /**
    * Internal method. Used by the `toString` on `Data`
    *
    * @param intervals
    *   intervals to represent
    * @return
    *   a collection of tuples of strings:
    *   1. first dimension start string
    *   1. first dimension end string
    *   1. interval grid-formatted string
    */
  def intervalPreprocessForGrid(intervals: Iterable[Interval[D]]): Iterable[(String, String, String)] =
    applyToDomain.preprocessForGridFromIntervals(intervals)

  /*
   * Valid data-like capabilities (used internally)
   */

  /**
    * Internal method. Used by the `toString` on `Data`
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
  def validDataPreprocessForGrid[V](validData: ValidData[V, D]): (String, String, String) =
    applyToDomain.preprocessForGridFromValidData(validData)

object DomainLike:
  given domainOrdering[D <: NonEmptyTuple](using domainLike: DomainLike[D]): Ordering[D] with
    override def compare(x: D, y: D): Int = domainLike.compareDomains(x, y)

  given [D <: NonEmptyTuple: DomainLikeTupleOps]: DomainLike[D] = DomainLike[D]
