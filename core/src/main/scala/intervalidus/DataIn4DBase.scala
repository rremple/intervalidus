package intervalidus

import intervalidus.Interval1D.{between, unbounded}
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}
import intervalidus.collection.{Box, Coordinate}

import scala.collection.mutable

/**
  * Constructs data in four-dimensional intervals.
  */
trait DataIn4DBaseObject extends DataIn4DConstructorParams:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DataIn4DBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    data: ValidData4D[V, R1, R2, R3, R4]
  )(using experimental: Experimental): DataIn4DBase[V, R1, R2, R3, R4]

  /**
    * Shorthand constructor for a single initial value that is valid in all full interval domains.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn4DBase]] structure with a single valid value.
    */
  def of[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    value: V
  )(using experimental: Experimental): DataIn4DBase[V, R1, R2, R3, R4]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of valid data to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of domain value used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of domain value used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of domain value used in the depth interval assigned to each value.
    * @tparam R4
    *   the type of domain value used in the fourth interval assigned to each value.
    * @return
    *   [[DataIn4DBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](
    initialData: Iterable[ValidData4D[V, R1, R2, R3, R4]]
  )(using
    Experimental
  ): DataIn4DBase[V, R1, R2, R3, R4]

trait DataIn4DConstructorParams:
  protected def constructorParams[V, R1, R2, R3, R4](
    initialData: Iterable[ValidData4D[V, R1, R2, R3, R4]]
  )(using
    experimental: Experimental,
    domainValue1: DomainValueLike[R1],
    domainValue2: DomainValueLike[R2],
    domainValue3: DomainValueLike[R3],
    domainValue4: DomainValueLike[R4]
  ): (
    mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]],
    mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]],
    MultiMapSorted[V, ValidData4D[V, R1, R2, R3, R4]],
    BoxTree[ValidData4D[V, R1, R2, R3, R4]]
  ) =
    val dataByStartAsc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]] =
      mutable.TreeMap.from(initialData.map(_.withKey))

    val dataByStartDesc: mutable.TreeMap[Domain4D[R1, R2, R3, R4], ValidData4D[V, R1, R2, R3, R4]] =
      experimental.control("noSearchTree")(
        experimentalResult =
          mutable.TreeMap.from(dataByStartAsc.iterator)(summon[Ordering[Domain4D[R1, R2, R3, R4]]].reverse),
        nonExperimentalResult = mutable.TreeMap()(summon[Ordering[Domain4D[R1, R2, R3, R4]]].reverse) // not used
      )

    val dataByValue: MultiMapSorted[V, ValidData4D[V, R1, R2, R3, R4]] =
      collection.mutable.MultiMapSorted.from(initialData.map(v => v.value -> v))

    val minPoint = Coordinate(
      domainValue1.minValue.orderedHashValue,
      domainValue2.minValue.orderedHashValue,
      domainValue3.minValue.orderedHashValue,
      domainValue4.minValue.orderedHashValue
    )
    val maxPoint = Coordinate(
      domainValue1.maxValue.orderedHashValue,
      domainValue2.maxValue.orderedHashValue,
      domainValue3.maxValue.orderedHashValue,
      domainValue4.maxValue.orderedHashValue
    )
    val boundary = Box(minPoint, maxPoint)

    val dataInSearchTree: BoxTree[ValidData4D[V, R1, R2, R3, R4]] =
      experimental.control("noSearchTree")(
        experimentalResult = BoxTree[ValidData4D[V, R1, R2, R3, R4]](boundary), // not used
        nonExperimentalResult =
          BoxTree.from[ValidData4D[V, R1, R2, R3, R4]](boundary, initialData.map(_.asBoxedPayload))
      )

    (dataByStartAsc, dataByStartDesc, dataByValue, dataInSearchTree)

/**
  * Like [[DataIn1DBase]], [[DataIn2DBase]], and [[DataIn3DBase]], data here have different values in different
  * intervals. But here data values vary in four dimensions. For example, one may want to represent when data are valid
  * in three dimensions of space and one dimension of time simultaneously (i.e., in spacetime).
  *
  * We can capture the dependency between various values and related four-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in four dimensions.
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of domain value used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of domain value used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of domain value used in the depth interval assigned to each value.
  * @tparam R4
  *   the type of domain value used in the fourth interval assigned to each value.
  */
trait DataIn4DBase[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike, R4: DomainValueLike](using
  experimental: Experimental
) extends intervalidus.DimensionalBase[
    V,
    Domain4D[R1, R2, R3, R4],
    Interval4D[R1, R2, R3, R4],
    ValidData4D[V, R1, R2, R3, R4],
    DiffAction4D[V, R1, R2, R3, R4],
    DataIn4DBase[V, R1, R2, R3, R4]
  ]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval4D.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  override protected def newValidData(value: V, interval: Interval4D[R1, R2, R3, R4]): ValidData4D[V, R1, R2, R3, R4] =
    interval -> value

  private def subIntervalsWith[B, R: DomainValueLike](
    that: DataIn4DBase[B, R1, R2, R3, R4],
    f: Interval4D[R1, R2, R3, R4] => Interval1D[R]
  ): Iterable[Interval1D[R]] = Interval1D.uniqueIntervals(
    this.getAll.map(d => f(d.interval)).toSet ++ that.getAll.map(d => f(d.interval))
  )

  protected def zipData[B](that: DataIn4DBase[B, R1, R2, R3, R4]): Iterable[ValidData4D[(V, B), R1, R2, R3, R4]] =
    for
      horizontalSubInterval <- subIntervalsWith(that, _.horizontal)
      verticalSubInterval <- subIntervalsWith(that, _.vertical)
      depthSubInterval <- subIntervalsWith(that, _.depth)
      fourthSubInterval <- subIntervalsWith(that, _.fourth)
      subInterval = horizontalSubInterval x verticalSubInterval x depthSubInterval x fourthSubInterval
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

  protected def zipAllData[B](
    that: DataIn4DBase[B, R1, R2, R3, R4],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData4D[(V, B), R1, R2, R3, R4]] =
    subIntervalsWith(that, _.horizontal).flatMap: horizontalSubInterval =>
      subIntervalsWith(that, _.vertical).flatMap: verticalSubInterval =>
        subIntervalsWith(that, _.depth).flatMap: depthSubInterval =>
          subIntervalsWith(that, _.fourth).flatMap: fourthSubInterval =>
            val subInterval = horizontalSubInterval x verticalSubInterval x depthSubInterval x fourthSubInterval
            val vOption = getIntersecting(subInterval).headOption.map(_.value)
            val bOption = that.getIntersecting(subInterval).headOption.map(_.value)
            val valuePair = (vOption, bOption) match
              case (None, None)       => None
              case (Some(v), Some(b)) => Some((v, b))
              case (Some(v), None)    => Some((v, thatElem))
              case (None, Some(b))    => Some((thisElem, b))
            valuePair.map(subInterval -> _)

  protected def getByHorizontalIndexData(
    horizontalIndex: Domain1D[R1]
  ): Iterable[ValidData3D[V, R2, R3, R4]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        Interval1D.intervalAt(horizontalIndex) x unbounded[R2] x unbounded[R3] x unbounded[R4]
      )
    )
    candidates.collect:
      case ValidData4D(value, interval) if horizontalIndex ∈ interval.horizontal =>
        (interval.vertical x interval.depth x interval.fourth) -> value

  protected def getByVerticalIndexData(
    verticalIndex: Domain1D[R2]
  ): Iterable[ValidData3D[V, R1, R3, R4]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        unbounded[R1] x Interval1D.intervalAt(verticalIndex) x unbounded[R3] x unbounded[R4]
      )
    )
    candidates.collect:
      case ValidData4D(value, interval) if verticalIndex ∈ interval.vertical =>
        (interval.horizontal x interval.depth x interval.fourth) -> value

  protected def getByDepthIndexData(
    depthIndex: Domain1D[R3]
  ): Iterable[ValidData3D[V, R1, R2, R4]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        unbounded[R1] x unbounded[R2] x Interval1D.intervalAt(depthIndex) x unbounded[R4]
      )
    )
    candidates.collect:
      case ValidData4D(value, interval) if depthIndex ∈ interval.depth =>
        (interval.horizontal x interval.vertical x interval.fourth) -> value

  protected def getByFourthIndexData(
    fourthIndex: Domain1D[R4]
  ): Iterable[ValidData3D[V, R1, R2, R3]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        unbounded[R1] x unbounded[R2] x unbounded[R3] x Interval1D.intervalAt(fourthIndex)
      )
    )
    candidates.collect:
      case ValidData4D(value, interval) if fourthIndex ∈ interval.fourth =>
        (interval.horizontal x interval.vertical x interval.depth) -> value

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intersections) in a pair. The other structure can have a different value type but must have the same interval
    * type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zip[B](that: DataIn4DBase[B, R1, R2, R3, R4]): DataIn4DBase[(V, B), R1, R2, R3, R4]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intersections) in a pair. If one of the two collections has a valid value in an interval where the other one
    * doesn't, placeholder elements are used in the result. The other structure can have a different value type but must
    * have the same interval type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisElem
    *   placeholder element used in intervals where data are valid in that but not this.
    * @param thatElem
    *   placeholder element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](that: DataIn4DBase[B, R1, R2, R3, R4], thisElem: V, thatElem: B): DataIn4DBase[(V, B), R1, R2, R3, R4]

  /**
    * Project as three-dimensional data based on a horizontal domain element
    *
    * @param horizontalIndex
    *   the horizontal domain element
    * @return
    *   a three-dimensional projection
    */
  def getByHorizontalIndex(horizontalIndex: Domain1D[R1]): DataIn3DBase[V, R2, R3, R4]

  /**
    * Project as three-dimensional data based on a vertical domain element
    *
    * @param verticalIndex
    *   the vertical domain element
    * @return
    *   a three-dimensional projection
    */
  def getByVerticalIndex(verticalIndex: Domain1D[R2]): DataIn3DBase[V, R1, R3, R4]

  /**
    * Project as three-dimensional data based on a depth domain element
    *
    * @param depthIndex
    *   the depth domain element
    * @return
    *   a three-dimensional projection
    */
  def getByDepthIndex(depthIndex: Domain1D[R3]): DataIn3DBase[V, R1, R2, R4]

  /**
    * Project as three-dimensional data based on a fourth domain element
    *
    * @param fourthIndex
    *   the fourth domain element
    * @return
    *   a three-dimensional projection
    */
  def getByFourthIndex(fourthIndex: Domain1D[R4]): DataIn3DBase[V, R1, R2, R3]

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value} ${v.interval.vertical} x ${v.interval.depth} x ${v.interval.fourth}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )

  // ---------- Implement methods from DimensionalBase ----------

  override def domain: Iterable[Interval4D[R1, R2, R3, R4]] =
    Interval4D.compress(Interval4D.uniqueIntervals(getAll.map(_.interval)).filter(intersects))

  override def domainComplement: Iterable[Interval4D[R1, R2, R3, R4]] =
    Interval4D.complement(domain)

  override def diffActionsFrom(old: DataIn4DBase[V, R1, R2, R3, R4]): Iterable[DiffAction4D[V, R1, R2, R3, R4]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction4D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction4D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction4D.Delete(oldData.interval.start))
        case _                                                    => None

  override protected def compressInPlace(value: V): Unit = Interval4D.compressGeneric(
    initialState = (), // no state -- updates applied in place
    result = identity, // no result -- updates applied in place
    dataIterable = _ => dataByValue.get(value),
    interval = _.interval,
    valueMatch = _.value == _.value,
    lookup = (_, start) => dataByStartAsc.get(start),
    compressAdjacent = (r, s, intervalByStart) =>
      removeValidData(s)
      updateValidData(r.interval ∪ s.interval -> value)
  )

  override protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- Interval4D.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // always returns either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    dataByValue.keySet.foreach(compressInPlace)

  /**
    * @inheritdoc
    *
    * More specifically, this gets even more complicated because there are four dimensions.
    *
    * @note
    *   The math of "combinations with repetition" tells us that there would be 6!/(3! x 3!) = 20 unique cases. Unlike
    *   in 2D and 3D where we have "legacy" logic that name and address each case individually, we don't even attempt to
    *   address them individually here -- just giving them intuitive names would be hard enough. (Nothing in four
    *   dimensions is ever intuitive!) Thankfully, the generic logic has been shown to be as fast as the legacy logic in
    *   2D and even faster than the legacy logic in 3D, so we only use that in 4D.
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  override protected def updateOrRemove(
    targetInterval: Interval4D[R1, R2, R3, R4],
    updateValue: V => Option[V]
  ): Unit = updateOrRemoveGeneric(targetInterval, updateValue): (overlap, intersection) =>
    for
      horizontal <- overlap.horizontal.separateUsing(intersection.horizontal)
      vertical <- overlap.vertical.separateUsing(intersection.vertical)
      depth <- overlap.depth.separateUsing(intersection.depth)
      fourth <- overlap.fourth.separateUsing(intersection.fourth)
      subinterval = horizontal x vertical x depth x fourth
      if subinterval != intersection
    yield subinterval

  override protected def fillInPlace[B <: V](interval: Interval4D[R1, R2, R3, R4], value: B): Unit = synchronized:
    val intersectingIntervals = getIntersecting(interval).map(_.interval)
    Interval4D
      .uniqueIntervals(intersectingIntervals.toSeq :+ interval)
      .foreach: i =>
        if interval.intersects(i) && !this.intersects(i) then addValidData(i -> value)
    compressInPlace(value)
