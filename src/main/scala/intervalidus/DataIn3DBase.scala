package intervalidus

import intervalidus.DiscreteInterval1D.{interval, unbounded}
import intervalidus.collection.mutable.{BoxOctree, MultiMapSorted}
import intervalidus.collection.{Box3D, BoxedPayload, Coordinate3D}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Constructs data in three-dimensional intervals.
  */
trait DataIn3DBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval domain.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @tparam R3
    *   the type of discrete value used in the depth discrete interval assigned to each value.
    * @param data
    *   value in interval to start with.
    * @return
    *   [[DataIn3DBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    data: ValidData3D[V, R1, R2, R3]
  )(using experimental: Experimental): DataIn3DBase[V, R1, R2, R3]

  /**
    * Shorthand constructor for a single initial value that is valid in all full interval domains.
    *
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete value used in the horizontal discrete interval assigned to each value.
    * @tparam R2
    *   the type of discrete value used in the vertical discrete interval assigned to each value.
    * @tparam R3
    *   the type of discrete value used in the depth discrete interval assigned to each value.
    * @param value
    *   value to start with.
    * @return
    *   [[DataIn3DBase]] structure with a single valid value.
    */
  def of[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    value: V
  )(using experimental: Experimental): DataIn3DBase[V, R1, R2, R3]

  /**
    * Constructor for a multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   (optional) a collection of valid data to start with -- intervals must be disjoint.
    * @tparam V
    *   the type of the value managed as data.
    * @tparam R1
    *   the type of discrete domain used in the horizontal interval assigned to each value.
    * @tparam R2
    *   the type of discrete domain used in the vertical interval assigned to each value.
    * @tparam R3
    *   the type of discrete value used in the depth discrete interval assigned to each value.
    * @return
    *   [[DataIn3DBase]] structure with zero or more valid values.
    */
  def apply[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]]
  )(using
    Experimental
  ): DataIn3DBase[V, R1, R2, R3]

  protected def constructorParams[V, R1, R2, R3](
    initialData: Iterable[ValidData3D[V, R1, R2, R3]]
  )(using
    experimental: Experimental,
    discreteValue1: DiscreteValue[R1],
    discreteValue2: DiscreteValue[R2],
    discreteValue3: DiscreteValue[R3]
  ): (
    mutable.TreeMap[DiscreteDomain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]],
    mutable.TreeMap[DiscreteDomain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]],
    MultiMapSorted[V, ValidData3D[V, R1, R2, R3]],
    BoxOctree[ValidData3D[V, R1, R2, R3]]
  ) =
    val dataByStartAsc: mutable.TreeMap[DiscreteDomain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]] =
      mutable.TreeMap.from(initialData.map(v => v.key -> v))

    val dataByStartDesc: mutable.TreeMap[DiscreteDomain3D[R1, R2, R3], ValidData3D[V, R1, R2, R3]] =
      experimental.control("noSearchTree")(
        experimentalResult =
          mutable.TreeMap.from(dataByStartAsc.iterator)(summon[Ordering[DiscreteDomain3D[R1, R2, R3]]].reverse),
        nonExperimentalResult = mutable.TreeMap()(summon[Ordering[DiscreteDomain3D[R1, R2, R3]]].reverse) // not used
      )

    val dataByValue: MultiMapSorted[V, ValidData3D[V, R1, R2, R3]] =
      collection.mutable.MultiMapSorted.from(initialData.map(v => v.value -> v))

    val minPoint = Coordinate3D(
      discreteValue1.minValue.orderedHashValue,
      discreteValue2.minValue.orderedHashValue,
      discreteValue3.minValue.orderedHashValue
    )
    val maxPoint = Coordinate3D(
      discreteValue1.maxValue.orderedHashValue,
      discreteValue2.maxValue.orderedHashValue,
      discreteValue3.maxValue.orderedHashValue
    )
    val boundary = Box3D(minPoint, maxPoint)

    val dataInSearchTree: BoxOctree[ValidData3D[V, R1, R2, R3]] =
      experimental.control("noSearchTree")(
        experimentalResult = BoxOctree[ValidData3D[V, R1, R2, R3]](boundary), // not used
        nonExperimentalResult = BoxOctree.from[ValidData3D[V, R1, R2, R3]](boundary, initialData.map(_.asBoxedPayload))
      )

    (dataByStartAsc, dataByStartDesc, dataByValue, dataInSearchTree)

/**
  * Like [[DataIn1DBase]], data here have different values in different discrete intervals. But here data values vary in
  * three dimensions. For example, one may want to represent when the data are valid in two dimensions of time plus over
  * certain versions simultaneously.
  *
  * We can capture the dependency between various values and related three-dimensional intervals cohesively in this
  * structure rather than in separate data structures using distributed (and potentially inconsistent) logic. This is
  * especially important for managing mutation, which can be a bit complex in three dimensions. Note that visualizing
  * three-dimensional data can be a bit daunting as well, so the toString method outputs a little Gantt chart and there
  * is a simple 2D Visualize tool provided where you can visualize 2D slices of the 3D structure (in the test package...
  * though maybe this should be its own separate subproject).
  *
  * @tparam V
  *   the type of the value managed as data.
  * @tparam R1
  *   the type of discrete domain used in the horizontal interval assigned to each value.
  * @tparam R2
  *   the type of discrete domain used in the vertical interval assigned to each value.
  * @tparam R3
  *   the type of discrete domain used in the depth interval assigned to each value.
  */
trait DataIn3DBase[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using experimental: Experimental)
  extends DimensionalBase[
    V,
    DiscreteDomain3D[R1, R2, R3],
    DiscreteInterval3D[R1, R2, R3],
    ValidData3D[V, R1, R2, R3],
    DataIn3DBase[V, R1, R2, R3]
  ]:

  def dataInSearchTree: BoxOctree[ValidData3D[V, R1, R2, R3]]

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(DiscreteInterval3D.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  override protected def newValidData(value: V, interval: DiscreteInterval3D[R1, R2, R3]): ValidData3D[V, R1, R2, R3] =
    interval -> value

  private def subIntervalsWith[B, R: DiscreteValue](
    that: DataIn3DBase[B, R1, R2, R3],
    f: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[R]
  ): Iterable[DiscreteInterval1D[R]] = DiscreteInterval1D.uniqueIntervals(
    this.getAll.map(d => f(d.interval)).toSet ++ that.getAll.map(d => f(d.interval))
  )

  protected def zipData[B](that: DataIn3DBase[B, R1, R2, R3]): Iterable[ValidData3D[(V, B), R1, R2, R3]] =
    for
      horizontalSubInterval <- subIntervalsWith(that, _.horizontal)
      verticalSubInterval <- subIntervalsWith(that, _.vertical)
      depthSubInterval <- subIntervalsWith(that, _.depth)
      subInterval = horizontalSubInterval x verticalSubInterval x depthSubInterval
      v <- getIntersecting(subInterval).headOption.map(_.value)
      b <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (v, b)

  protected def zipAllData[B](
    that: DataIn3DBase[B, R1, R2, R3],
    thisElem: V,
    thatElem: B
  ): Iterable[ValidData3D[(V, B), R1, R2, R3]] =
    subIntervalsWith(that, _.horizontal).flatMap: horizontalSubInterval =>
      subIntervalsWith(that, _.vertical).flatMap: verticalSubInterval =>
        subIntervalsWith(that, _.depth).flatMap: depthSubInterval =>
          val subInterval = horizontalSubInterval x verticalSubInterval x depthSubInterval
          val vOption = getIntersecting(subInterval).headOption.map(_.value)
          val bOption = that.getIntersecting(subInterval).headOption.map(_.value)
          val valuePair = (vOption, bOption) match
            case (None, None)       => None
            case (Some(v), Some(b)) => Some((v, b))
            case (Some(v), None)    => Some((v, thatElem))
            case (None, Some(b))    => Some((thisElem, b))
          valuePair.map(subInterval -> _)

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
  def zip[B](that: DataIn3DBase[B, R1, R2, R3]): DataIn3DBase[(V, B), R1, R2, R3]

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
  def zipAll[B](that: DataIn3DBase[B, R1, R2, R3], thisElem: V, thatElem: B): DataIn3DBase[(V, B), R1, R2, R3]

  /**
    * Flips this domain by swapping the vertical and depth components with one another and keeping the same horizontal
    * component.
    */
  def flipAboutHorizontal: DataIn3DBase[V, R1, R3, R2]

  /**
    * Flips this domain by swapping the depth and horizontal components with one another and keeping the same vertical
    * component.
    */
  def flipAboutVertical: DataIn3DBase[V, R3, R2, R1]

  /**
    * Flips this domain by swapping the vertical and horizontal components with one another and keeping the same depth
    * component.
    */
  def flipAboutDepth: DataIn3DBase[V, R2, R1, R3]

  /**
    * Project as 2-dimensional data based on a horizontal domain element
    *
    * @param horizontalIndex
    *   the horizontal domain element
    * @return
    *   a 2-dimensional projection
    */
  def getByHorizontalIndex(horizontalIndex: DiscreteDomain1D[R1]): DataIn2DBase[V, R2, R3]

  /**
    * Project as 2-dimensional data based on a vertical domain element
    *
    * @param verticalIndex
    *   the vertical domain element
    * @return
    *   a 2-dimensional projection
    */
  def getByVerticalIndex(verticalIndex: DiscreteDomain1D[R2]): DataIn2DBase[V, R1, R3]

  /**
    * Project as 2-dimensional data based on a depth domain element
    *
    * @param depthIndex
    *   the depth domain element
    * @return
    *   a 2-dimensional projection
    */
  def getByDepthIndex(depthIndex: DiscreteDomain1D[R3]): DataIn2DBase[V, R1, R2]

  // ---------- Implement methods not defined in DimensionalBase ----------

  // from Object - use Visualize (in the test package) if you want something fancier
  override def toString: String = toStringGrid(
    dataToString = v => s"${v.value} ${v.interval.vertical} x ${v.interval.depth}",
    dataToInterval = _.interval.horizontal,
    dataToSortBy = _.interval.vertical.end
  )

  /**
    * Internal method, to compress in place. Assumes caller does synchronization (if needed). Assumes underlying data
    * are disjoint, so no need to address intersections.
    *
    * @param value
    *   value to evaluate
    * @return
    *   this structure once compressed (not a copy)
    */
  override protected def compressInPlace(value: V): Unit =
    /*
     * Each mutation gives rise to other compression possibilities. And applying a compression action
     * can invalidate the remainder of the actions (e.g., three-in-a-row). Unlike in one dimension, there
     * is no safe order to fold over to avoid these issues. So, instead, we evaluate every entry with every
     * other entry, get the first compression action, apply it, and recurse until there aren't anymore actions to apply.
     */
    @tailrec
    def compressRecursively(): Unit =
      val firstActions = dataByValue
        .get(value)
        .map: r =>
          def rightKey = r.key.copy(horizontalIndex = r.interval.horizontal.end.successor)
          def upperKey = r.key.copy(verticalIndex = r.interval.vertical.end.successor)
          def frontKey = r.key.copy(depthIndex = r.interval.depth.end.successor)
          def rightAdjacent = dataByStartAsc
            .get(rightKey)
            .filter(s => s.value == value && (s.interval isRightAdjacentTo r.interval))
          def upperAdjacent = dataByStartAsc
            .get(upperKey)
            .filter(s => s.value == value && (s.interval isUpperAdjacentTo r.interval))
          def frontAdjacent = dataByStartAsc
            .get(frontKey)
            .filter(s => s.value == value && (s.interval isFrontAdjacentTo r.interval))
          rightAdjacent // preferred
            .orElse(upperAdjacent) // next preferred
            .orElse(frontAdjacent)
            .map: s =>
              Seq(
                () => removeValidData(s),
                () => updateValidData(r.interval ∪ s.interval -> value)
              )
        .collectFirst:
          case Some(actions) => actions

      firstActions match
        case None => ()
        case Some(actions) =>
          actions.foreach(_.apply())
          compressRecursively()

    compressRecursively()

  /**
    * Unlike in 1D, there is no unique compression in 2D and 3D. This method decompresses data so there is a unique
    * arrangement of "atomic" intervals. Then it recompresses the data, which results in a unique physical
    * representation. It may be useful when comparing two structures to see if they are logically equivalent even if,
    * physically, they differ in how they are compressed.
    */
  protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- DiscreteInterval3D.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // will always return either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    dataByValue.keySet.foreach(compressInPlace)

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn3DBase[V, R1, R2, R3]): Iterable[DiffAction3D[V, R1, R2, R3]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction3D.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction3D.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction3D.Delete(oldData.key))
        case _                                                    => None

  protected def getByHorizontalIndexData(
    horizontalIndex: DiscreteDomain1D[R1]
  ): Iterable[ValidData2D[V, R2, R3]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        DiscreteInterval1D.intervalAt(horizontalIndex) x unbounded[R2] x unbounded[R3]
      )
    )
    candidates.collect:
      case ValidData3D(value, interval) if horizontalIndex ∈ interval.horizontal =>
        (interval.vertical x interval.depth) -> value

  protected def getByVerticalIndexData(
    verticalIndex: DiscreteDomain1D[R2]
  ): Iterable[ValidData2D[V, R1, R3]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        unbounded[R1] x DiscreteInterval1D.intervalAt(verticalIndex) x unbounded[R3]
      )
    )
    candidates.collect:
      case ValidData3D(value, interval) if verticalIndex ∈ interval.vertical =>
        (interval.horizontal x interval.depth) -> value

  protected def getByDepthIndexData(
    depthIndex: DiscreteDomain1D[R3]
  ): Iterable[ValidData2D[V, R1, R2]] =
    val candidates = experimental.control("noSearchTree")(
      experimentalResult = getAll,
      nonExperimentalResult = dataInSearchTreeGet(
        unbounded[R1] x unbounded[R2] x DiscreteInterval1D.intervalAt(depthIndex)
      )
    )
    candidates.collect:
      case ValidData3D(value, interval) if depthIndex ∈ interval.depth =>
        (interval.horizontal x interval.vertical) -> value

  // ---------- Implement methods from DimensionalBase ----------

  override protected def dataInSearchTreeAdd(data: ValidData3D[V, R1, R2, R3]): Unit =
    dataInSearchTree.addOne(data.asBoxedPayload)

  override protected def dataInSearchTreeRemove(data: ValidData3D[V, R1, R2, R3]): Unit =
    dataInSearchTree.remove(data.asBoxedPayload)

  override protected def dataInSearchTreeClear(): Unit =
    dataInSearchTree.clear()

  override protected def dataInSearchTreeAddAll(data: Iterable[ValidData3D[V, R1, R2, R3]]): Unit =
    dataInSearchTree.addAll(data.map(_.asBoxedPayload))

  override protected def dataInSearchTreeGet(
    interval: DiscreteInterval3D[R1, R2, R3]
  ): Iterable[ValidData3D[V, R1, R2, R3]] =
    BoxedPayload.deduplicate(dataInSearchTree.get(interval.asBox)).map(_.payload)

  override protected def dataInSearchTreeGetByDomain(
    domainIndex: DiscreteDomain3D[R1, R2, R3]
  ): Option[ValidData3D[V, R1, R2, R3]] =
    dataInSearchTree
      .get(DiscreteInterval3D.intervalAt(domainIndex).asBox)
      .collectFirst:
        case d if d.payload.interval.contains(domainIndex) => d.payload

  override protected def dataInSearchTreeIntersects(interval: DiscreteInterval3D[R1, R2, R3]): Boolean =
    dataInSearchTree.get(interval.asBox).exists(_.payload.interval intersects interval)

  /**
    * @inheritdoc
    *
    * More specifically, this gets even more complicated because there are three dimensions. Exclusions in one dimension
    * can have three remainders: none (simple), single (partial), and split. But three-dimensional exclusions have these
    * same three remainders in each dimension, so there are a total of 3 x 3 x 9 = 27 remainder cases. But there are
    * symmetric partners for many of them, so actually there are only 5!/(2! x 3!) = 10 unique cases:
    *   1. simple = none + none + none (1 case)
    *   1. corner = single + single + single (1 case)
    *   1. core = split + split + split (1 case)
    *   1. face = single + none + none (3 symmetric cases)
    *   1. edge = single + single + none (3 symmetric cases)
    *   1. slice = split + none + none (3 symmetric cases)
    *   1. hole = split + split + none (3 symmetric cases)
    *   1. notch = split + single + single (3 symmetric cases)
    *   1. divot = split + split + single (3 symmetric cases)
    *   1. bite = split + single + none (6 symmetric cases)
    * @param targetInterval
    *   the interval where any valid values are removed/updated.
    * @param newValueOption
    *   when defined, the value to be stored as part of an update
    */
  protected override def updateOrRemove(
    targetInterval: DiscreteInterval3D[R1, R2, R3],
    newValueOption: Option[V]
  ): Unit = experimental.control("bruteForceUpdate")(
    nonExperimentalResult = updateOrRemoveOptimized(targetInterval, newValueOption),
    experimentalResult = updateOrRemoveBruteForce(targetInterval, newValueOption)
  )

  /*
   * The normal, way faster (2.8 - 4.5 times faster), but way more complex implementation.
   * Considers each case separately.
   */
  private def updateOrRemoveOptimized(
    targetInterval: DiscreteInterval3D[R1, R2, R3],
    newValueOption: Option[V]
  ): Unit = synchronized:
    import DiscreteInterval1D.Remainder

    // Utility to find the complement of split remaining
    def between[T: DiscreteValue](
      beforeRemaining: DiscreteInterval1D[T],
      afterRemaining: DiscreteInterval1D[T]
    ): DiscreteInterval1D[T] = interval(beforeRemaining.end.successor, afterRemaining.start.predecessor)

    // Utility to find the complement of single remaining (intervals must either start or end together)
    def excludeRemaining[T: DiscreteValue](
      full: DiscreteInterval1D[T],
      remaining: DiscreteInterval1D[T]
    ): DiscreteInterval1D[T] =
      if remaining hasSameStartAs full
      then full.startingAfter(remaining.end)
      else full.endingBefore(remaining.start)

    val intersecting = getIntersecting(targetInterval)
    // These values will be targets for compression later
    val potentiallyAffectedValues = intersecting.map(_.value).toSet ++ newValueOption

    intersecting.foreach: overlap =>

      /*
       * These methods are generic by dimension to accommodate symmetric operations without code duplication.
       * In general, "before" can be left, below, or back, and "after" can be right, above, or front.
       */

      /**
        * A single overlap in one dimension with total overlaps in the others - adjust in the one. (This is the 2D
        * "edge" case extended into the third dimension to take out the whole face.)
        */
      def face[T: DiscreteValue](
        remaining1D: DiscreteInterval1D[T],
        extract1D: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T],
        modify3D: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        if remaining1D hasSameStartAs extract1D(overlap.interval)
        then // same start: shorten
          updateValidData(modify3D(overlap.interval, remaining1D) -> overlap.value)
        else // different start: remove and add remaining after, new before
          removeValidData(overlap)
          addValidData(modify3D(overlap.interval, remaining1D) -> overlap.value)

        newValueOption.foreach: newValue =>
          addValidData(
            modify3D(overlap.interval, excludeRemaining(extract1D(overlap.interval), remaining1D)) -> newValue
          )

      /**
        * A partial overlap in two dimensions and total overlap in the other - remove and add 2 remaining (compress
        * later). (This is the 2D "corner" case extended into the third dimension to take out the whole edge.)
        */
      def edge[T1: DiscreteValue, T2: DiscreteValue](
        remainingDimOne: DiscreteInterval1D[T1],
        remainingDimTwo: DiscreteInterval1D[T2],
        extractDimOne: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T1],
        extractDimTwo: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T2],
        modifyDimOne: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T1]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimTwo: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T2]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        extension (s: DiscreteInterval3D[R1, R2, R3])
          def dimOne = extractDimOne(s)
          def dimTwo = extractDimTwo(s)
          def withDimOne(update: DiscreteInterval1D[T1]) = modifyDimOne(s, update)
          def withDimTwo(update: DiscreteInterval1D[T2]) = modifyDimTwo(s, update)

        // can be before or after.
        val dimTwoCornerBit = excludeRemaining(overlap.interval.dimTwo, remainingDimTwo)
        removeValidData(overlap)
        addValidData(overlap.interval.withDimTwo(remainingDimTwo) -> overlap.value)
        addValidData(overlap.interval.withDimOne(remainingDimOne).withDimTwo(dimTwoCornerBit) -> overlap.value)
        newValueOption.foreach: newValue =>
          // can be before or after.
          val dimOneCornerBit = excludeRemaining(overlap.interval.dimOne, remainingDimOne)
          addValidData(overlap.interval.withDimOne(dimOneCornerBit).withDimTwo(dimTwoCornerBit) -> newValue)

      /**
        * A split in one dimension with total overlaps in the others - shorten before, add remaining after. (This is the
        * 2D "slice" case extended into the third dimension.)
        */
      def slice[T: DiscreteValue](
        remainingBefore1D: DiscreteInterval1D[T],
        remainingAfter1D: DiscreteInterval1D[T],
        modify3D: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        updateValidData(modify3D(overlap.interval, remainingBefore1D) -> overlap.value)
        addValidData(modify3D(overlap.interval, remainingAfter1D) -> overlap.value)
        newValueOption.foreach: newValue =>
          val sliceBit = between(remainingBefore1D, remainingAfter1D)
          addValidData(modify3D(overlap.interval, sliceBit) -> newValue)

      /**
        * A split in three dimensions with a total overlap in the other - shorten before, add remaining. (This is the 2D
        * "hole" case extended into the third dimension.)
        */
      def hole[T1: DiscreteValue, T2: DiscreteValue](
        beforeRemainingDimOne: DiscreteInterval1D[T1],
        afterRemainingDimOne: DiscreteInterval1D[T1],
        beforeRemainingDimTwo: DiscreteInterval1D[T2],
        afterRemainingDimTwo: DiscreteInterval1D[T2],
        modifyDimOne: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T1]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimTwo: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T2]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        extension (s: DiscreteInterval3D[R1, R2, R3])
          def withDimOne(update: DiscreteInterval1D[T1]) = modifyDimOne(s, update)
          def withDimTwo(update: DiscreteInterval1D[T2]) = modifyDimTwo(s, update)

        val holeBitDim2 = between(beforeRemainingDimTwo, afterRemainingDimTwo)
        // shorten before the hole in dim 2
        updateValidData(overlap.interval.withDimTwo(beforeRemainingDimTwo) -> overlap.value)
        // add before the hole in dim 1
        addValidData(overlap.interval.withDimOne(beforeRemainingDimOne).withDimTwo(holeBitDim2) -> overlap.value)
        // add after the hole in dim 2
        addValidData(overlap.interval.withDimOne(afterRemainingDimOne).withDimTwo(holeBitDim2) -> overlap.value)
        // after the hole in dim 2
        addValidData(overlap.interval.withDimTwo(afterRemainingDimTwo) -> overlap.value)
        newValueOption.foreach: newValue =>
          val holeBitDim1 = between(beforeRemainingDimOne, afterRemainingDimOne)
          // the hole itself is updated
          addValidData(overlap.interval.withDimOne(holeBitDim1).withDimTwo(holeBitDim2) -> newValue)

      /**
        * A partial overlap in two dimensions and a split in the other - remove and add four chunks to replace it. (This
        * is a unique 3D case, like a "bite", but out of an edge instead of a face.)
        */
      def notch[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
        remainingDimOne: DiscreteInterval1D[T1],
        beforeRemainingDimTwo: DiscreteInterval1D[T2],
        afterRemainingDimTwo: DiscreteInterval1D[T2],
        remainingDimThree: DiscreteInterval1D[T3],
        extractDimOne: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T1],
        extractDimThree: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T3],
        modifyDimOne: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T1]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimTwo: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T2]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimThree: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T3]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        extension (s: DiscreteInterval3D[R1, R2, R3])
          def dimOne = extractDimOne(s)
          def dimThree = extractDimThree(s)
          def withDimOne(update: DiscreteInterval1D[T1]) = modifyDimOne(s, update)
          def withDimTwo(update: DiscreteInterval1D[T2]) = modifyDimTwo(s, update)
          def withDimThree(update: DiscreteInterval1D[T3]) = modifyDimThree(s, update)

        val excludedDimOne = excludeRemaining(overlap.interval.dimOne, remainingDimOne)
        val excludedDimTwo = between(beforeRemainingDimTwo, afterRemainingDimTwo)
        val excludedDimThree = excludeRemaining(overlap.interval.dimThree, remainingDimThree)

        val dimThreeExcluded = overlap.interval.withDimThree(excludedDimThree)
        val dimOneAndThreeExcluded = dimThreeExcluded.withDimOne(excludedDimOne)

        removeValidData(overlap)
        // large chunk across the whole of dim 1 and 2 where dim 3 remains
        addValidData(overlap.interval.withDimThree(remainingDimThree) -> overlap.value)
        // medium chunk across the whole of dim 2 where dim 1 remains but dim 3 is excluded
        addValidData(dimThreeExcluded.withDimOne(remainingDimOne) -> overlap.value)
        // smaller chunks before and after dim 2 exclusion where dim 1 and 3 are both excluded
        addValidData(dimOneAndThreeExcluded.withDimTwo(beforeRemainingDimTwo) -> overlap.value)
        addValidData(dimOneAndThreeExcluded.withDimTwo(afterRemainingDimTwo) -> overlap.value)

        newValueOption.foreach: newValue =>
          val excluded = overlap.interval
            .withDimOne(excludedDimOne)
            .withDimTwo(excludedDimTwo)
            .withDimThree(excludedDimThree)
          addValidData(excluded -> newValue)

      /**
        * A partial overlap in one dimension and splits in the other two - remove and add five chunks to replace it.
        * (This is a unique 3D case, like a "hole" but not extending in the full third dimension.)
        */
      def divot[T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](
        beforeRemainingDimOne: DiscreteInterval1D[T1],
        afterRemainingDimOne: DiscreteInterval1D[T1],
        beforeRemainingDimTwo: DiscreteInterval1D[T2],
        afterRemainingDimTwo: DiscreteInterval1D[T2],
        remainingDimThree: DiscreteInterval1D[T3],
        extractDimThree: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T3],
        modifyDimOne: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T1]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimTwo: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T2]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimThree: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T3]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        extension (s: DiscreteInterval3D[R1, R2, R3])
          def dimThree = extractDimThree(s)
          def withDimOne(update: DiscreteInterval1D[T1]) = modifyDimOne(s, update)
          def withDimTwo(update: DiscreteInterval1D[T2]) = modifyDimTwo(s, update)
          def withDimThree(update: DiscreteInterval1D[T3]) = modifyDimThree(s, update)

        val excludedDimOne = between(beforeRemainingDimOne, afterRemainingDimOne)
        val excludedDimTwo = between(beforeRemainingDimTwo, afterRemainingDimTwo)
        val excludedDimThree = excludeRemaining(overlap.interval.dimThree, remainingDimThree)

        val dimThreeExcluded = overlap.interval.withDimThree(excludedDimThree)
        val dimOneAndThreeExcluded = dimThreeExcluded.withDimOne(excludedDimOne)

        removeValidData(overlap)
        // large chunk across the whole of dim 1 and 2 where dim 3 remains
        addValidData(overlap.interval.withDimThree(remainingDimThree) -> overlap.value)
        // medium chunks across the whole of dim 2 where dim 1 remains but dim 3 is excluded
        addValidData(dimThreeExcluded.withDimOne(beforeRemainingDimOne) -> overlap.value)
        addValidData(dimThreeExcluded.withDimOne(afterRemainingDimOne) -> overlap.value)
        // smaller chunks before and after dim 2 exclusion where dim 1 and 3 are both excluded
        addValidData(dimOneAndThreeExcluded.withDimTwo(beforeRemainingDimTwo) -> overlap.value)
        addValidData(dimOneAndThreeExcluded.withDimTwo(afterRemainingDimTwo) -> overlap.value)

        newValueOption.foreach: newValue =>
          val excluded = overlap.interval
            .withDimOne(excludedDimOne)
            .withDimTwo(excludedDimTwo)
            .withDimThree(excludedDimThree)
          addValidData(excluded -> newValue)

      /**
        * A partial overlap in one dimension, a split in another, and a total overlap in the last - shorten before and
        * add 2 remaining. (This is the 2D "bite" case extended into the third dimension, so a bite out of a face.)
        */
      def bite[T1: DiscreteValue, T2: DiscreteValue](
        remainingDimOne: DiscreteInterval1D[T1],
        beforeRemainingDimTwo: DiscreteInterval1D[T2],
        afterRemainingDimTwo: DiscreteInterval1D[T2],
        extractDimOne: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T1],
        modifyDimOne: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T1]) => DiscreteInterval3D[R1, R2, R3],
        modifyDimTwo: (DiscreteInterval3D[R1, R2, R3], DiscreteInterval1D[T2]) => DiscreteInterval3D[R1, R2, R3]
      ): Unit =
        extension (s: DiscreteInterval3D[R1, R2, R3])
          def dimOne = extractDimOne(s)
          def withDimOne(update: DiscreteInterval1D[T1]) = modifyDimOne(s, update)
          def withDimTwo(update: DiscreteInterval1D[T2]) = modifyDimTwo(s, update)

        val biteBitDimTwo = between(beforeRemainingDimTwo, afterRemainingDimTwo)
        // shorten to before bite
        updateValidData(overlap.interval.withDimTwo(beforeRemainingDimTwo) -> overlap.value)
        // through the bite
        addValidData(overlap.interval.withDimOne(remainingDimOne).withDimTwo(biteBitDimTwo) -> overlap.value)
        // after the bite
        addValidData(overlap.interval.withDimTwo(afterRemainingDimTwo) -> overlap.value)
        newValueOption.foreach: newValue =>
          // can be before or after.
          val biteBitDimOne = excludeRemaining(overlap.interval.dimOne, remainingDimOne)
          // the bite is updated
          addValidData(overlap.interval.withDimOne(biteBitDimOne).withDimTwo(biteBitDimTwo) -> newValue)

      overlap.interval \ targetInterval match

        /*
         * One kind of simple, corner, and core
         */

        /**
          * Simple: total overlap in dims 1, 2, and 3 (no remainders) - just update/remove the whole thing
          */
        case (Remainder.None, Remainder.None, Remainder.None) =>
          newValueOption match
            case Some(newValue) => updateValidData(overlap.interval -> newValue)
            case None           => removeValidData(overlap)

        /**
          * Corner: partial overlaps in dims 1, 2, and 3. Although not optimal, much easier to operate on octants
          * separately, with one being removed. Remove the full overlap if the start is the octant being excluded,
          * otherwise we can just shorten something. Then add the octants remaining
          */
        case (
              Remainder.Single(horizontalRemaining),
              Remainder.Single(verticalRemaining),
              Remainder.Single(depthRemaining)
            ) =>
          // The bit to remove/update can be any of the eight corners.
          val excludedBit = excludeRemaining(overlap.interval.horizontal, horizontalRemaining) x
            excludeRemaining(overlap.interval.vertical, verticalRemaining) x
            excludeRemaining(overlap.interval.depth, depthRemaining)

          if excludedBit hasSameStartAs overlap.interval then removeValidData(overlap)
          for
            horizontal <- Seq(horizontalRemaining, excludedBit.horizontal)
            vertical <- Seq(verticalRemaining, excludedBit.vertical)
            depth <- Seq(depthRemaining, excludedBit.depth)
          do
            val octant = horizontal x vertical x depth
            if octant != excludedBit
            then
              if octant hasSameStartAs overlap.interval
              then updateValidData(octant -> overlap.value)
              else addValidData(octant -> overlap.value)

          newValueOption.foreach: newValue =>
            addValidData(excludedBit -> newValue)

        /**
          * Core: split in dims 1, 2, and 3. The region excluded is completely contained in the overlap region. Shorten
          * the bottom
          */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          // The bit to remove/update
          val excluded = between(leftRemaining, rightRemaining) x
            between(belowRemaining, aboveRemaining) x
            between(backRemaining, frontRemaining)

          // shorten the bottom and add the top
          updateValidData(overlap.interval.withVertical(belowRemaining) -> overlap.value)
          addValidData(overlap.interval.withVertical(aboveRemaining) -> overlap.value)
          // add the slivers to the left and right of the exclusion
          val centerSlice = overlap.interval.withVertical(excluded.vertical)
          addValidData(centerSlice.withHorizontal(leftRemaining) -> overlap.value)
          addValidData(centerSlice.withHorizontal(rightRemaining) -> overlap.value)
          // add the bits in back and front of the exclusion
          val centerSliver = centerSlice.withHorizontal(excluded.horizontal)
          addValidData(centerSliver.withDepth(backRemaining) -> overlap.value)
          addValidData(centerSliver.withDepth(frontRemaining) -> overlap.value)
          // update if required
          newValueOption.foreach: newValue =>
            addValidData(excluded -> newValue)

        /*
         * Three kinds of faces
         */

        /** partial overlap in dim 1 with total overlap in dim 2 and 3 */
        case (Remainder.Single(remainingHorizontal), Remainder.None, Remainder.None) =>
          face(remainingHorizontal, _.horizontal, _.withHorizontal(_))

        /** partial overlap in dim 2 with total overlap in dim 1 and 3 */
        case (Remainder.None, Remainder.Single(remainingVertical), Remainder.None) =>
          face(remainingVertical, _.vertical, _.withVertical(_))

        /** partial overlap in dim 3 with total overlap in dim 1 and 2 */
        case (Remainder.None, Remainder.None, Remainder.Single(remainingDepth)) =>
          face(remainingDepth, _.depth, _.withDepth(_))

        /*
         * Three kinds of edges
         */

        /** partial overlap in both dim 1 and 2 and total overlap in dim 3 */
        case (Remainder.Single(remainingHorizontal), Remainder.Single(remainingVertical), Remainder.None) =>
          edge(remainingHorizontal, remainingVertical, _.horizontal, _.vertical, _.withHorizontal(_), _.withVertical(_))

        /** partial overlap in both dim 1 and 3 and total overlap in dim 2 */
        case (Remainder.Single(remainingHorizontal), Remainder.None, Remainder.Single(remainingDepth)) =>
          edge(remainingHorizontal, remainingDepth, _.horizontal, _.depth, _.withHorizontal(_), _.withDepth(_))

        /** partial overlap in both dim 2 and 3 and total overlap in dim 1 */
        case (Remainder.None, Remainder.Single(remainingVertical), Remainder.Single(remainingDepth)) =>
          edge(remainingVertical, remainingDepth, _.vertical, _.depth, _.withVertical(_), _.withDepth(_))

        /*
         * Three kinds of slices
         */

        /** split in dim 1 with total overlap in dim 2 and 3 */
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.None, Remainder.None) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          slice(leftRemaining, rightRemaining, _.withHorizontal(_))

        /** split in dim 2 with total overlap in dim 1 and 3 */
        case (Remainder.None, Remainder.Split(belowRemaining, aboveRemaining), Remainder.None) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          slice(belowRemaining, aboveRemaining, _.withVertical(_))

        /** split in dim 3 with total overlap in dim 1 and 2 */
        case (Remainder.None, Remainder.None, Remainder.Split(backRemaining, frontRemaining)) =>
          // assert(backRemaining hasSameStartAs overlap.interval.depth) // same overlap key
          slice(backRemaining, frontRemaining, _.withDepth(_))

        /*
         * Three kinds of holes
         */

        /** split in dim 1 and 2 and total overlap in dim 3 */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.None
            ) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          hole(leftRemaining, rightRemaining, belowRemaining, aboveRemaining, _.withHorizontal(_), _.withVertical(_))

        /** split in dim 1 and 3 and total overlap in dim 2 */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.None,
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          // assert(backRemaining hasSameStartAs overlap.interval.depth) // same overlap key
          hole(leftRemaining, rightRemaining, backRemaining, frontRemaining, _.withHorizontal(_), _.withDepth(_))

        /** split in dim 2 and 3 and total overlap in dim 1 */
        case (
              Remainder.None,
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          // assert(backRemaining hasSameStartAs overlap.interval.depth) // same overlap key
          hole(belowRemaining, aboveRemaining, backRemaining, frontRemaining, _.withVertical(_), _.withDepth(_))

        /*
         * Three kinds of notches (a 3D bite)
         */

        /** partial overlap in dim 1 and 2 with a split in dim 3 */
        case (
              Remainder.Single(horizontalRemaining),
              Remainder.Single(verticalRemaining),
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          notch(
            horizontalRemaining,
            backRemaining,
            frontRemaining,
            verticalRemaining,
            _.horizontal,
            _.vertical,
            _.withHorizontal(_),
            _.withDepth(_),
            _.withVertical(_)
          )

        /** partial overlap in dim 1 and 3 with a split in dim 2 */
        case (
              Remainder.Single(horizontalRemaining),
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.Single(depthRemaining)
            ) =>
          notch(
            horizontalRemaining,
            belowRemaining,
            aboveRemaining,
            depthRemaining,
            _.horizontal,
            _.depth,
            _.withHorizontal(_),
            _.withVertical(_),
            _.withDepth(_)
          )

        /** partial overlap in dim 2 and 3 with a split in dim 1 */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.Single(verticalRemaining),
              Remainder.Single(depthRemaining)
            ) =>
          notch(
            verticalRemaining,
            leftRemaining,
            rightRemaining,
            depthRemaining,
            _.vertical,
            _.depth,
            _.withVertical(_),
            _.withHorizontal(_),
            _.withDepth(_)
          )

        /*
         * Three kinds of divots - a hole that doesn't go all the way through
         */

        /** partial overlap in dim 1 with a split in dim 2 and 3 */
        case (
              Remainder.Single(horizontalRemaining),
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          divot(
            belowRemaining,
            aboveRemaining,
            backRemaining,
            frontRemaining,
            horizontalRemaining,
            _.horizontal,
            _.withVertical(_),
            _.withDepth(_),
            _.withHorizontal(_)
          )

        /** partial overlap in dim 2 with a split in dim 1 and 3 */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.Single(verticalRemaining),
              Remainder.Split(backRemaining, frontRemaining)
            ) =>
          divot(
            leftRemaining,
            rightRemaining,
            backRemaining,
            frontRemaining,
            verticalRemaining,
            _.vertical,
            _.withHorizontal(_),
            _.withDepth(_),
            _.withVertical(_)
          )

        /** partial overlap in dim 3 with a split in dim 1 and 2 */
        case (
              Remainder.Split(leftRemaining, rightRemaining),
              Remainder.Split(belowRemaining, aboveRemaining),
              Remainder.Single(depthRemaining)
            ) =>
          divot(
            leftRemaining,
            rightRemaining,
            belowRemaining,
            aboveRemaining,
            depthRemaining,
            _.depth,
            _.withHorizontal(_),
            _.withVertical(_),
            _.withDepth(_)
          )

        /*
         * Six kinds of bites
         */

        /** partial overlap in dim 1, split in dim 2, and total overlap in dim 3 */
        case (Remainder.Single(remainingHorizontal), Remainder.Split(belowRemaining, aboveRemaining), Remainder.None) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          bite(
            remainingHorizontal,
            belowRemaining,
            aboveRemaining,
            _.horizontal,
            _.withHorizontal(_),
            _.withVertical(_)
          )

        /** partial overlap in dim 1, split in dim 3, and total overlap in dim 2 */
        case (Remainder.Single(remainingHorizontal), Remainder.None, Remainder.Split(backRemaining, frontRemaining)) =>
          // assert(backRemaining hasSameStartAs overlap.interval.depth) // same overlap key
          bite(remainingHorizontal, backRemaining, frontRemaining, _.horizontal, _.withHorizontal(_), _.withDepth(_))

        /** partial overlap in dim 2, split in dim 1, and total overlap in dim 3 */
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.Single(remainingVertical), Remainder.None) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          bite(remainingVertical, leftRemaining, rightRemaining, _.vertical, _.withVertical(_), _.withHorizontal(_))

        /** partial overlap in dim 2, split in dim 3, and total overlap in dim 1 */
        case (Remainder.None, Remainder.Single(remainingVertical), Remainder.Split(backRemaining, frontRemaining)) =>
          // assert(backRemaining hasSameStartAs overlap.interval.depth) // same overlap key
          bite(remainingVertical, backRemaining, frontRemaining, _.vertical, _.withVertical(_), _.withDepth(_))

        /** partial overlap in dim 3, split in dim 1, and total overlap in dim 2 */
        case (Remainder.Split(leftRemaining, rightRemaining), Remainder.None, Remainder.Single(remainingDepth)) =>
          // assert(leftRemaining hasSameStartAs overlap.interval.horizontal) // same overlap key
          bite(remainingDepth, leftRemaining, rightRemaining, _.depth, _.withDepth(_), _.withHorizontal(_))

        /** partial overlap in dim 3, split in dim 2, and total overlap in dim 1 */
        case (Remainder.None, Remainder.Split(belowRemaining, aboveRemaining), Remainder.Single(remainingDepth)) =>
          // assert(belowRemaining hasSameStartAs overlap.interval.vertical) // same overlap key
          bite(remainingDepth, belowRemaining, aboveRemaining, _.depth, _.withDepth(_), _.withVertical(_))
    potentiallyAffectedValues.foreach(compressInPlace)

  /*
   * The simpler but slower and less efficient implementation.
   * Benchmarks show this makes the remove operation 2.8 to 4.5 times slower.
   */
  private def updateOrRemoveBruteForce(
    targetInterval: DiscreteInterval3D[R1, R2, R3],
    newValueOption: Option[V]
  ): Unit = synchronized:
    import DiscreteInterval1D.Remainder

    val intersecting = getIntersecting(targetInterval)
    val potentiallyAffectedValues = intersecting.map(_.value).toSet ++ newValueOption

    intersecting.foreach: overlap =>

      def excludeOverlapRemainder1D[T: DiscreteValue](
        extractFromOverlap: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T],
        remainder: Remainder[DiscreteInterval1D[T]]
      ): DiscreteInterval1D[T] =
        val full = extractFromOverlap(overlap.interval)
        remainder match
          case Remainder.None =>
            full
          case Remainder.Single(remaining) if remaining hasSameStartAs full =>
            full.startingAfter(remaining.end)
          case Remainder.Single(remaining) =>
            full.endingBefore(remaining.start)
          case Remainder.Split(leftRemaining, rightRemaining) =>
            interval(leftRemaining.end.successor, rightRemaining.start.predecessor)

      def allOverlapSubintevals1D[T: DiscreteValue](
        extractFromOverlap: DiscreteInterval3D[R1, R2, R3] => DiscreteInterval1D[T],
        remainder: Remainder[DiscreteInterval1D[T]]
      ): Seq[DiscreteInterval1D[T]] =
        val excluded = excludeOverlapRemainder1D(extractFromOverlap, remainder)
        remainder match
          case Remainder.None                                 => Seq(excluded)
          case Remainder.Single(remaining)                    => Seq(remaining, excluded).sortBy(_.start)
          case Remainder.Split(leftRemaining, rightRemaining) => Seq(leftRemaining, excluded, rightRemaining)

      val (horizontalRemainder, verticalRemainder, depthRemainder) = overlap.interval \ targetInterval
      val excludedSubinterval =
        excludeOverlapRemainder1D(_.horizontal, horizontalRemainder) x
          excludeOverlapRemainder1D(_.vertical, verticalRemainder) x
          excludeOverlapRemainder1D(_.depth, depthRemainder)
      if excludedSubinterval hasSameStartAs overlap.interval then removeValidData(overlap)
      for
        horizontal <- allOverlapSubintevals1D(_.horizontal, horizontalRemainder)
        vertical <- allOverlapSubintevals1D(_.vertical, verticalRemainder)
        depth <- allOverlapSubintevals1D(_.depth, depthRemainder)
      do
        val subinterval = horizontal x vertical x depth
        if subinterval != excludedSubinterval
        then
          if subinterval hasSameStartAs overlap.interval
          then updateValidData(subinterval -> overlap.value)
          else addValidData(subinterval -> overlap.value)

      newValueOption.foreach: newValue =>
        addValidData(excludedSubinterval -> newValue)

    potentiallyAffectedValues.foreach(compressInPlace)

  override def getIntersecting(interval: DiscreteInterval3D[R1, R2, R3]): Iterable[ValidData3D[V, R1, R2, R3]] =
    experimental.control("noSearchTree")(
      experimentalResult = dataByStartDesc // Using reverse-key order allows us O(1) in 1D and nearly O(1) in 2D
        .valuesIteratorFrom(interval.end) // starting at or before the interval end
        .filter(_.interval intersects interval)
        .toList,
      nonExperimentalResult = dataInSearchTreeGet(interval).filter(_.interval intersects interval).toList
    )

  override infix def intersects(interval: DiscreteInterval3D[R1, R2, R3]): Boolean =
    experimental.control("noSearchTree")(
      experimentalResult = getAll.exists(_.interval intersects interval),
      nonExperimentalResult = dataInSearchTreeIntersects(interval)
    )
