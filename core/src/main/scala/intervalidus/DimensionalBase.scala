package intervalidus

import intervalidus.Domain.NonEmptyTail
import intervalidus.DomainLike.given
import intervalidus.collection.{Boundary, Box, BoxedPayload, Capacity}
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.Tuple.{Concat, Drop, Elem, Head, Take, Tail}
import scala.collection.mutable
import scala.compiletime.ops.int.S

/**
  * Common definitions used in all dimensional data.
  */
object DimensionalBase:
  type In1D[V, R1] = DimensionalBase[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DimensionalBase[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DimensionalBase[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DimensionalBase[V, Domain.In4D[R1, R2, R3, R4]]

/**
  * @define objectDesc
  *   Constructs data in multidimensional intervals.
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  */
trait DimensionalBaseObject:
  /**
    * Shorthand constructor for a single initial value that is valid in a particular interval.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param data
    *   value valid within an interval.
    * @return
    *   [[DimensionalBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): DimensionalBase[V, D]

  /**
    * Shorthand constructor for a single initial value that is valid in the full interval domain.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @param value
    *   value that is valid in the full domain (`Interval.unbounded[D]`).
    * @return
    *   [[DimensionalBase]] structure with a single valid value.
    */
  def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DimensionalBase[V, D]

  /**
    * Constructor for multiple (or no) initial values that are valid in the various intervals.
    *
    * @param initialData
    *   a collection of values valid within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   [[DimensionalBase]] structure with zero or more valid values.
    */
  def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using Experimental): DimensionalBase[V, D]

  /**
    * Get a Builder based on an intermediate buffer of valid data.
    *
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    */
  def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], DimensionalBase[V, D]]

class DimensionalDataBuilder[V, D <: NonEmptyTuple: DomainLike, Self <: DimensionalBase[V, D]](
  build: List[ValidData[V, D]] => Self
)(using
  Experimental
) extends mutable.ReusableBuilder[ValidData[V, D], Self]:
  protected val validDataBuilder: mutable.Builder[ValidData[V, D], List[ValidData[V, D]]] = List.newBuilder
  override def clear(): Unit = validDataBuilder.clear()
  override def result(): Self = build(validDataBuilder.result())
  override def addOne(elem: ValidData[V, D]): this.type =
    validDataBuilder.addOne(elem)
    this

/**
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  */
trait DimensionalBaseConstructorParams:
  /**
    * Given a collection of valid data, returns data used to populate the `dataByStartAsc`, `dataByValue`, and
    * `dataInSearchTree` internal data structures.
    * @note
    *   A tight boundary around the origin with a capacity large enough to contain the initial data is used. Benchmarks
    *   have shown that a tightly-defined capacity has better insert performance even if it has to be resized at some
    *   point.
    *
    * @param initialData
    *   a collection of values valid within intervals -- intervals must be disjoint.
    * @tparam V
    *   $dataValueType
    * @tparam D
    *   $intervalDomainType
    * @return
    *   tuple of `TreeMap` data, `MultiMapSorted` data, and `BoxTree` data used when constructing something that is a
    *   `DimensionalBase` and has overridden `dataByStartAsc`, `dataByValue`, and `dataInSearchTree` in the constructor.
    */
  protected def constructorParams[V, D <: NonEmptyTuple](
    initialData: Iterable[ValidData[V, D]]
  )(using domainValue: DomainLike[D])(using Experimental): (
    mutable.TreeMap[D, ValidData[V, D]],
    MultiMapSorted[V, ValidData[V, D]],
    BoxTree[ValidData[V, D]]
  ) =
    val initialPayloads = initialData.map(_.asBoxedPayload)
    val initialCapacity = initialPayloads.foldLeft(Capacity.aroundOrigin(domainValue.arity)): (capacity, payload) =>
      capacity.growAround(payload.box)
    (
      mutable.TreeMap.from(initialData.map(_.withStartKey)),
      MultiMapSorted.from(initialData.map(_.withValueKey)),
      BoxTree.from(Boundary(initialCapacity), initialPayloads)
    )

/**
  * Base for all dimensional data, both mutable and immutable, of arbitrary dimensions.
  *
  * @define dataValueType
  *   the type of the value managed as data.
  * @define intervalDomainType
  *   the domain type -- a non-empty tuple that is DomainLike.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  *
  * @define immutableReturn
  *   a new, updated structure.
  * @define mutableAction
  *   Data are mutated in place.
  * @define mapDesc
  *   Applies a function to all valid data.
  * @define mapParamF
  *   the function to apply to each valid data element.
  * @define collectDesc
  *   Applies a partial function to all valid data on which it is defined.
  * @define collectParamPf
  *   the partial function to apply to each data element.
  * @define mapValuesDesc
  *   Applies a function to all valid data values.
  * @define mapValuesParamF
  *   the function to apply to the value part of each valid data element.
  * @define mapIntervalsDesc
  *   Applies a function to all valid data intervals.
  * @define mapIntervalsParamF
  *   the function to apply to the interval part of each valid data element.
  * @define flatMapParamF
  *   the function to apply to each valid data element which results in a new structure.
  * @define filterParamP
  *   the predicate used to test elements.
  * @define setDesc
  *   Set new valid data. Replaces any data previously valid in this interval.
  * @define setParamNewData
  *   the valid data to set.
  * @define setManyDesc
  *   Set a collection of new valid data. Replaces any data previously valid in this interval.
  * @define setManyNote
  *   if intervals overlap, later items will update earlier ones, so order can matter.
  * @define setManyParamNewData
  *   collection of valid data to set.
  * @define setIfNoConflictDesc
  *   Set new valid data, but only if there are no data previously valid in this interval.
  * @define setIfNoConflictParamNewData
  *   the valid data to set.
  * @define updateDesc
  *   Update everything valid in data's interval to have the data's value. No new intervals of validity are added as
  *   part of this operation. Data that overlaps are adjusted accordingly.
  * @define updateParamNewData
  *   the new value existing data in the interval should take on
  * @define replaceDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceParamOldData
  *   the old data to be replaced.
  * @define replaceParamNewData
  *   the new data replacing the old data
  * @define replaceByKeyDesc
  *   Remove the old data and replace it with the new data. The new data value and interval can be different. Data that
  *   overlaps with the new data interval are adjusted accordingly.
  * @define replaceByKeyParamKey
  *   key of the old data to be replaced (the interval start).
  * @define replaceByKeyParamNewData
  *   the new data replacing the old data
  * @define removeDesc
  *   Remove valid values on the interval. If there are values valid on portions of the interval, those values have
  *   their intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeParamInterval
  *   the interval where any valid values are removed.
  * @define removeManyDesc
  *   Remove data in all the intervals. If there are values valid on portions of any interval, those values have their
  *   intervals adjusted (e.g., shortened, shifted, split) accordingly.
  * @define removeManyParamIntervals
  *   the intervals where any valid values are removed.
  * @define removeValueDesc
  *   Remove the value in all the intervals where it is valid.
  * @define removeValueParamValue
  *   the value that is removed.
  * @define compressDesc
  *   Compress out adjacent intervals with the same value.
  * @define compressParamValue
  *   value for which valid data are compressed.
  * @define compressAllDesc
  *   Compress out adjacent intervals with the same value for all values.
  * @define recompressAllDesc1
  *   Unlike in 1D, there is no unique compression in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
  *   [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
  * @define recompressAllDesc2
  *   First, this method decompresses data to use a unique arrangement of "atomic" intervals. In the above example, that
  *   would be the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Next, it
  *   recompresses the data, which results in a unique physical representation. It may be useful when comparing two
  *   structures to see if they are logically equivalent even if, physically, they differ in how they are compressed.
  * @define applyDiffActionsDesc
  *   Applies a sequence of diff actions to this structure.
  * @define applyDiffActionsParamDiffActions
  *   actions to be applied.
  * @define syncWithDesc
  *   Synchronizes this with another structure by getting and applying the applicable diff actions.
  * @define syncWithParamThat
  *   the structure with which this is synchronized.
  * @define fillDesc
  *   Adds a value as valid in portions of the interval where there aren't already valid values.
  * @define fillParamData
  *   value to make valid in any validity gaps found in the interval
  * @define mergeDesc
  *   Merges this structure with data from that structure. In intervals where both structures have valid values, the two
  *   values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
  *   are added (a fill operation).
  * @define mergeParamThat
  *   structure to merge with this one
  * @define mergeParamMergeValues
  *   function that merges values where both this and that have valid values, where the default merge operation is to
  *   give this data values priority and drop that data values
  */
trait DimensionalBase[V, D <: NonEmptyTuple](using
  domainLike: DomainLike[D]
)(using Experimental)
  extends PartialFunction[D, V]:

  override def equals(obj: Any): Boolean = obj match
    case that: DimensionalBase[V, D] @unchecked =>
      size == that.size && getAll.zip(that.getAll).forall(_ == _)
    case _ => false

  // Utility methods for managing state, not part of API

  /**
    * Internal mutator to add valid data, where there is no existing overlapping data.
    *
    * @param data
    *   valid data to add.
    */
  protected def addValidData(data: ValidData[V, D]): Unit =
    // assert(!dataByStartAsc.isDefinedAt(data.interval.start))
    dataByStartAsc.addOne(data.withStartKey)
    dataByValue.addOne(data.withValueKey)
    dataInSearchTree.addOne(data.asBoxedPayload)

  /**
    * Internal mutator to update valid data, where a value with v.interval.start already exists.
    *
    * @param data
    *   valid data to update.
    */
  protected def updateValidData(data: ValidData[V, D]): Unit =
    // assert(dataByStartAsc.isDefinedAt(data.interval.start))
    val oldData = dataByStartAsc(data.interval.start)
    dataByValue.subtractOne(oldData.withValueKey)
    dataByValue.addOne(data.withValueKey)
    dataByStartAsc.update(data.interval.start, data)
    dataInSearchTree.remove(oldData.asBoxedPayload)
    dataInSearchTree.addOne(data.asBoxedPayload)

  /**
    * Internal mutator to remove valid data, where a known value already exists.
    *
    * @param oldData
    *   valid data to remove.
    */
  protected def removeValidData(oldData: ValidData[V, D]): Unit =
    dataByValue.subtractOne(oldData.withValueKey)
    dataByStartAsc.remove(oldData.interval.start)
    dataInSearchTree.remove(oldData.asBoxedPayload)

  /**
    * Internal mutator to remove valid data, where a value with a known key already exists.
    *
    * @param key
    *   key (interval start) for valid data to remove.
    */
  protected def removeValidDataByKey(key: D): Unit =
    removeValidData(dataByStartAsc(key))

  /**
    * Internal mutator to replace all valid data.
    * @param data
    *   new valid data replacing the old valid data
    */
  protected def replaceValidData(data: Iterable[ValidData[V, D]]): Unit =
    dataByStartAsc.clear()
    dataByStartAsc.addAll(data.map(_.withStartKey))
    dataByValue.clear()
    dataByValue.addAll(data.map(_.withValueKey))
    dataInSearchTree.clear()
    dataInSearchTree.addAll(data.map(_.asBoxedPayload))

  /**
    * Internal method, to update or remove in place.
    *
    * Remove or update valid values on the target interval. If there are values valid on portions of the interval, those
    * values have their intervals adjusted (e.g., shortened, shifted, split) accordingly. The logic of remove and update
    * are similar, and this method supports both.
    *
    * @note
    *   this gets even more complicated in higher dimensions. Exclusions in one dimension can have three remainders:
    *   none (simple), single (partial), and split. But multidimensional exclusions have these same three remainders in
    *   each dimension, so there are a total of 3<sup>n</sup> remainder cases. But there is symmetry because order
    *   doesn't matter, so actually there are only `(3+n-1)! / (3! x (n-1)!)` (we want the unique combinations of
    *   choosing from 3 remainder scenarios n times, where order doesn't matter).
    *
    * For example, with `n = 4`, this reduces to `6!/(3! x 3!) = 20` unique cases. But just giving these cases intuitive
    * names would be hard enough. (Nothing in four dimensions is ever intuitive!)
    *
    * Falling back to more familiar space, using `n = 3` this reduces to `5!/(2! x 3!) = 10` unique cases, which can be
    * enumerated as follows:
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
    *
    * @param targetInterval
    *   the interval where any valid values are updated or removed.
    * @param updateValue
    *   maps a current value to some updated value, or None if the value should be removed.
    */
  protected def updateOrRemove(targetInterval: Interval[D], updateValue: V => Option[V]): Unit = synchronized:
    val intersectingValues = getIntersecting(targetInterval).map: overlap =>
      (overlap.interval ∩ targetInterval)
        .foreach: intersection => // there will always be one
          // Creates an atomic deconstruction of the intervals covering the overlap without the intersection
          val atomicNonIntersections = overlap.interval.separateUsing(intersection).filter(_ != intersection)
          /*
           * Compression is important to minimize add/update calls (which are expensive). Initially, to avoid the
           * TreeMap build in `Interval.compress`, a linear folding algorithm was used here that only looked back two
           * elements. Although it worked, compression had more of an "above" than "right" bias leading to results
           * that, although properly compressed, differed from the results of `recompressAll`. By benchmarking
           * `remove` using these alternative approaches (folding vs. `Interval.compress`) it was shown that the
           * throughput was kind of a wash in two dimensions, but the throughput in three dimensions was more than 40%
           * better using `Interval.compress`. This is probably because, to effectively compress in n dimensions, the
           * folding method would need to look back at least n elements, and 3 > 2. The ineffective compression led
           * to more unnecessary update/add calls, and therefore lower throughput. This effect is likely even more
           * pronounced in higher dimensions. That's why this just uses standard `Interval.compress` now, and it is a
           * nice side effect that all the compressions are consistent without taking the performance hit of
           * `recompressAll`.
           */
          val nonIntersections = Interval.compress(atomicNonIntersections)

          // remove the intersecting region if it happens to have the same key as the overlap
          if intersection hasSameStartAs overlap.interval then removeValidData(overlap)

          // add/update non-intersecting regions
          nonIntersections.foreach: subinterval =>
            if subinterval hasSameStartAs overlap.interval
            then updateValidData(subinterval -> overlap.value)
            else addValidData(subinterval -> overlap.value)

          // if there is an updated value, add it back in
          updateValue(overlap.value).foreach: newValue =>
            addValidData(intersection -> newValue)

      // intersecting value result for compression later
      overlap.value

    // compress all potentially affected values
    val intersectingValueSet = intersectingValues.toSet
    val potentiallyAffectedValues = intersectingValueSet ++ intersectingValueSet.flatMap(updateValue)
    potentiallyAffectedValues.foreach(compressInPlace)

  /**
    * Internal method, to fill in place.
    *
    * Adds a value as valid in portions of the interval where there aren't already valid values.
    *
    * @param data
    *   specifies the interval in which the value should be filled
    */
  protected def fillInPlace(data: ValidData[V, D]): Unit = synchronized:
    val intersectingIntervals = getIntersecting(data.interval).map(_.interval)
    Interval
      .uniqueIntervals(intersectingIntervals.toSeq :+ data.interval)
      .foreach: i =>
        if data.interval.intersects(i) && !this.intersects(i) then addValidData(i -> data.value)
    compressInPlace(data.value)

  /**
    * Internal method, to merge in place.
    *
    * Merges this structure with data from that structure. In intervals where both structures have valid values, the two
    * values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
    * are added (a fill operation).
    *
    * @param that
    *   data to merge into this one
    * @param mergeValues
    *   function that merges values where both this and that have valid values
    */
  protected def mergeInPlace(
    that: Iterable[ValidData[V, D]],
    mergeValues: (V, V) => V
  ): Unit = that.foreach: thatData =>
    updateOrRemove(thatData.interval, thisDataValue => Some(mergeValues(thisDataValue, thatData.value)))
    fillInPlace(thatData)

  /**
    * Internal method, to compress in place.
    *
    * Assumes caller does synchronization (if needed). Assumes underlying data are disjoint, so no need to address
    * intersections.
    *
    * @param value
    *   value to be evaluated
    * @return
    *   this structure once compressed (not a copy)
    */
  protected def compressInPlace(value: V): Unit = Interval.compressGeneric(
    initialState = (), // no state -- updates applied in place
    result = identity, // no result -- updates applied in place
    dataIterable = _ => dataByValue.get(value),
    interval = _.interval,
    valueMatch = _.value == _.value,
    lookup = (_, start) => dataByStartAsc.get(start),
    compressAdjacent = (r, s, _) =>
      removeValidData(s)
      updateValidData(r.interval ∪ s.interval -> value)
  )

  /**
    * Internal method, to recompress in place.
    *
    * Unlike in 1D, there are no unique compressions in higher dimensions. For example, {[1..5], [1..2]} + {[1..2],
    * [3..4]} could also be represented physically as {[1..2], [1..4]} + {[3..5], [1..2]}.
    *
    * This method decompresses data to a unique arrangement of "atomic" intervals. In the above example, that would be
    * the following "atomic" intervals: {[1..2], [1..2]} + {[3..5], [1..2]} + {[1..2], [3..4]}. Then it recompresses the
    * data, which results in a unique physical representation. This may be useful when comparing two structures to see
    * if they are logically equivalent even if, physically, they differ in how they are compressed.
    */
  protected def recompressInPlace(): Unit = synchronized:
    // decompress
    val atomicData = for
      atomicInterval <- Interval.uniqueIntervals(getAll.map(_.interval))
      intersecting <- getIntersecting(atomicInterval) // always returns either one or zero results
    yield intersecting.copy(interval = atomicInterval)
    replaceValidData(atomicData)

    // recompress
    dataByValue.keySet.foreach(compressInPlace)

  /**
    * Applies a diff action to this structure.
    *
    * @param diffAction
    *   action to be applied.
    */
  protected def applyDiffActionInPlace(diffAction: DiffAction[V, D]): Unit = synchronized:
    diffAction match
      case DiffAction.Create(data: ValidData[V, D]) => addValidData(data)
      case DiffAction.Update(data: ValidData[V, D]) => updateValidData(data)
      case DiffAction.Delete(key)                   => removeValidDataByKey(key)
    // Not sure why, but returning explicit Unit here resolves runtime type check warning above
    ()

  /**
    * Internal method, to zip with the data of another dimensional structure with the same domain type. The result
    * domain will be the intersection of this domain and that domain.
    *
    * @param that
    *   dimensional structure to zip with -- must have the same domain type
    * @tparam B
    *   the value type of that
    * @return
    *   a collection of valid data representing this structure's data zipped with that structure's data.
    */
  protected def zipData[B](that: DimensionalBase[B, D]): Iterable[ValidData[(V, B), D]] =
    for
      subInterval <- Interval.uniqueIntervals(getAll.map(_.interval) ++ that.getAll.map(_.interval))
      thisValue <- getIntersecting(subInterval).headOption.map(_.value)
      thatValue <- that.getIntersecting(subInterval).headOption.map(_.value)
    yield subInterval -> (thisValue, thatValue)

  /**
    * Internal method, to zip with the data of another dimensional structure with the same domain type and apply
    * defaults where data are valid in one structure and not the other. The result domain will be the union of this
    * domain and that domain.
    *
    * @param that
    *   dimensional structure to zip with -- must have the same domain type
    * @param thisDefault
    *   default value used on the left-hand side when no data are valid in this structure when data are valid in that
    *   structure.
    * @param thatDefault
    *   default value used on the right-hand side when no data are valid in that structure when data are valid in this
    *   structure.
    * @tparam B
    *   the value type of that
    * @return
    *   a collection of valid data representing this structure's data zipped with that structure's data and defaults.
    */
  protected def zipAllData[B](
    that: DimensionalBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): Iterable[ValidData[(V, B), D]] =
    for
      subInterval <- Interval.uniqueIntervals(getAll.map(_.interval) ++ that.getAll.map(_.interval))
      thisValueOption = getIntersecting(subInterval).headOption.map(_.value)
      thatValueOption = that.getIntersecting(subInterval).headOption.map(_.value)
      valuePair <- (thisValueOption, thatValueOption) match
        case (None, None)                       => None
        case (Some(thisValue), Some(thatValue)) => Some((thisValue, thatValue))
        case (Some(thisValue), None)            => Some((thisValue, thatDefault))
        case (None, Some(thatValue))            => Some((thisDefault, thatValue))
    yield subInterval -> valuePair

  /**
    * Internal method to get all data as data in n-1 dimensions based on a lookup in the head dimension.
    *
    * (Equivalent to `getByDimensionData[H, NonEmptyTail[D]](0, domain)`, though the type checking is simpler.)
    *
    * @tparam H
    *   the domain value type of the 1D domain used for filtering. There are type safety checks that ensure
    *   - the head 1D domain has the specified domain value type
    *   - the current domain tail is a non-empty domain (i.e., the current domain type `D` has at least two dimensions)
    *   - the current domain type can be constructed by concatenating the 1D domain type specified and the current
    *     domain tail.
    * @param domain
    *   the head dimension domain element used for filtering
    * @return
    *   a collection of valid data representing the lower-dimensional (n-1) projection
    */
  protected def getByHeadDimensionData[H: DomainValueLike](domain: Domain1D[H])(using
    Head[D] =:= Domain1D[H],
    Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): Iterable[ValidData[V, NonEmptyTail[D]]] =
    val filteredData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        getAll.filter(_.interval.headInterval1D contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withHeadUpdate[H](_ => Interval1D.intervalAt(domain))
        getIntersecting(lookup)
    filteredData.map: data =>
      data.interval.tailInterval -> data.value

  /**
    * Internal method to get all data as data in n-1 dimensions based on a lookup in the specified dimension.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    * @param domain
    *   the domain element used for filtering
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    * @tparam R
    *   domain of intervals in the returned valid data. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a collection of valid data representing the lower-dimensional (n-1) projection
    */
  protected def getByDimensionData[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int,
    domain: Domain1D[H]
  )(using
    Elem[D, dimensionIndex.type] =:= Domain1D[H],
    Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D,
    Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
  ): Iterable[ValidData[V, R]] =
    val filteredData = domain match
      case Domain1D.Top | Domain1D.Bottom =>
        getAll.filter(_.interval(dimensionIndex) contains domain)
      case _ =>
        val lookup = Interval.unbounded[D].withDimensionUpdate[H](dimensionIndex, _ => Interval1D.intervalAt(domain))
        getIntersecting(lookup)
    filteredData.map: data =>
      data.interval.dropDimension(dimensionIndex) -> data.value

  // ---------- API methods implemented here ----------

  // from Object
  // print a uniform grid representing the data.
  override def toString: String = if getAll.isEmpty then "<nothing is valid>"
  else
    // tuples of first dimension start string, first dimension end string, first dimension string (header)
    val horizontalIntervalStrings = domainLike.intervalPreprocessForGrid(getAll.map(_.interval))
    // tuples of first dimension start string, first dimension end string, value + remaining dimension string
    val validDataStrings = getAll.map(_.preprocessForGrid)
    val maxDataSize = validDataStrings
      .map(_._3.length + 3)
      .maxOption
      .getOrElse(3)
    val maxHorizontalIntervalsSize = horizontalIntervalStrings
      .map(_._3.length)
      .maxOption
      .getOrElse(7)

    val cellSize = math.max(maxDataSize, maxHorizontalIntervalsSize)

    def pad(chars: Int, p: String = " "): String = p * chars

    val (horizontalStringBuilder, horizontalStartPositionBuilder, horizontalEndPositionBuilder) =
      horizontalIntervalStrings.zipWithIndex.foldLeft(
        (StringBuilder(), Map.newBuilder[String, Int], Map.newBuilder[String, Int])
      ):
        case ((stringBuilder, startPositionBuilder, endPositionBuilder), ((startString, endString, formatted), pos)) =>
          startPositionBuilder.addOne(startString, stringBuilder.size)
          stringBuilder.append(formatted)
          val padTo = cellSize * (pos + 1)
          if stringBuilder.size < padTo then stringBuilder.append(pad(padTo - stringBuilder.size))
          endPositionBuilder.addOne(endString, stringBuilder.size)
          (stringBuilder, startPositionBuilder, endPositionBuilder)

    val horizontalStartPosition = horizontalStartPositionBuilder.result()
    val horizontalEndPosition = horizontalEndPositionBuilder.result()
    horizontalStringBuilder.append("|\n")

    validDataStrings
      // .sortBy(dataToSortBy) // if needed, could be hard to implement...
      .foreach: (startString, endString, valueString) =>
        val leftPosition = horizontalStartPosition(startString)
        val rightPosition = horizontalEndPosition(endString)
        val valuePadding = rightPosition - leftPosition - valueString.length - 2
        horizontalStringBuilder.append(
          s"${pad(leftPosition)}| $valueString${pad(valuePadding)}|\n"
        )

    horizontalStringBuilder.result()

  // from PartialFunction
  override def isDefinedAt(key: D): Boolean = getAt(key).isDefined

  // from PartialFunction
  override def apply(domainIndex: D): V = getAt(domainIndex).getOrElse(
    throw Exception(s"Not defined at $domainIndex")
  )

  /**
    * Tests if there are no valid data in this structure.
    *
    * @return
    *   true if there are no valid data, false otherwise.
    */
  def isEmpty: Boolean = dataByStartAsc.isEmpty

  /**
    * The number of valid data entries.
    */
  def size: Int = dataByStartAsc.size

  /**
    * Returns the value if a single, unbounded valid value exists, otherwise throws an exception.
    *
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals).
    */
  def get: V = getAll.headOption match
    case Some(d: ValidData[V, D]) if d.interval.isUnbounded => d.value
    case Some(_)                                            => throw new NoSuchElementException("bounded get")
    case None                                               => throw new NoSuchElementException("empty get")

  /**
    * Returns Some value if a single, unbounded valid value exists, otherwise returns None.
    */
  def getOption: Option[V] = getAll.headOption.filter(_.interval.isUnbounded).map(_.value)

  /**
    * Returns all valid data in interval start order
    */
  def getAll: Iterable[ValidData[V, D]] = dataByStartAsc.values

  /**
    * Returns valid data at the specified domain element. That is, where the specified domain element is a member of
    * some valid data interval. If no such valid data exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value and corresponding interval if valid at the specified domain element, otherwise None.
    */
  def getDataAt(domainIndex: D): Option[ValidData[V, D]] =
    dataInSearchTree
      .get(Box.at(domainIndex.asCoordinate))
      .collectFirst:
        case d if domainIndex ∈ d.payload.interval => d.payload

  /**
    * Returns a value that is valid at the specified domain element. That is, where the specified domain element is a
    * member of some valid data interval. If no such valid value exists, returns None.
    *
    * @param domainIndex
    *   the domain element where data may be valid. The domain element can be a specific data point or the special
    *   notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(domainIndex: D): Option[V] = getDataAt(domainIndex).map(_.value)

  /**
    * Returns all data that are valid on some or all of the provided interval.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: Interval[D]): Iterable[ValidData[V, D]] =
    dataInSearchTree
      .getAndDeduplicate(interval.asBox)
      .collect:
        case BoxedPayload(_, payload, _) if payload.interval intersects interval => payload

  /**
    * Are there values that are valid on some or all of the provided interval?
    *
    * @param interval
    *   the interval to check.
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: Interval[D]): Boolean =
    dataInSearchTree.get(interval.asBox).exists(_.payload.interval intersects interval)

  /**
    * Returns all the intervals (compressed) in which there are valid values. See
    * [[https://en.wikipedia.org/wiki/Domain_of_a_function]].
    */
  def domain: Iterable[Interval[D]] = Interval.compress(
    Interval.uniqueIntervals(getAll.map(_.interval)).filter(intersects)
  )

  /**
    * Returns all the intervals (compressed) in which there are no valid values. That is, all intervals that are not in
    * the [[domain]]. See [[https://en.wikipedia.org/wiki/Domain_of_a_function]] and
    * [[https://en.wikipedia.org/wiki/Complement_(set_theory)]].
    */
  def domainComplement: Iterable[Interval[D]] = Interval.compress(
    Interval.uniqueIntervals(getAll.map(_.interval) ++ Iterable(Interval.unbounded[D])).filter(!intersects(_))
  )

  /**
    * Returns the distinct values that are valid in some interval.
    */
  def values: Iterable[V] = dataByValue.keySet

  /**
    * Returns the intervals in which this value is valid.
    *
    * @param value
    *   the value to look up
    */
  def intervals(value: V): Iterable[Interval[D]] = dataByValue.get(value).map(_.interval)

  /**
    * Applies a binary operator to a start value and all valid data, going left to right.
    *
    * @param z
    *   the start value.
    * @param op
    *   the binary operator.
    * @tparam B
    *   the result type of the binary operator.
    * @return
    *   the result of inserting op between consecutive valid data elements, going left to right with the start value z
    *   on the left. Returns z if there are no valid data elements.
    */
  def foldLeft[B](z: B)(op: (B, ValidData[V, D]) => B): B = getAll.foldLeft(z)(op)

  /**
    * Constructs a sequence of diff actions that, if applied to the old structure, would synchronize it with this one.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DimensionalBase[V, D]): Iterable[DiffAction[V, D]] =
    (dataByStartAsc.keys.toSet ++ old.dataByStartAsc.keys).toList.sorted.flatMap: key =>
      (old.dataByStartAsc.get(key), dataByStartAsc.get(key)) match
        case (Some(oldData), Some(newData)) if oldData != newData => Some(DiffAction.Update(newData))
        case (None, Some(newData))                                => Some(DiffAction.Create(newData))
        case (Some(oldData), None)                                => Some(DiffAction.Delete(oldData.interval.start))
        case _                                                    => None

  // ---------- To be implemented by inheritor ----------

  /**
    * Internal data structure where all the interval-bounded data are stored, always expected to be disjoint. TreeMap
    * maintains interval key order.
    */
  protected def dataByStartAsc: scala.collection.mutable.TreeMap[D, ValidData[V, D]]

  /**
    * An internal shadow data structure where all the interval-bounded data are also stored, but using the value itself
    * as the key (for faster compression, which is done by value). The ValidData[V, D] are stored in a sorted set, so
    * they are retrieved in key order, making compression operations repeatable.
    */
  protected def dataByValue: MultiMapSorted[V, ValidData[V, D]]

  /**
    * An internal shadow data structure where all the interval-bounded data are also stored, but in a "box search tree"
    * -- a hyperoctree (i.e., a B-tree, quadtree, octree, etc., depending on the dimension) that supports quick
    * retrieval by interval.
    */
  protected def dataInSearchTree: BoxTree[ValidData[V, D]]

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data.
    */
  def copy: DimensionalBase[V, D]

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
  def zip[B](that: DimensionalBase[B, D]): DimensionalBase[(V, B), D]

  /**
    * Returns a new structure formed from this structure and another structure by combining the corresponding elements
    * (all intervals in both this and that) in a pair. If one of the two collections has a valid value in an interval
    * where the other one doesn't, default elements are used in the result. The other structure can have a different
    * value type but must have the same interval type.
    *
    * @param that
    *   the structure which is going to be zipped.
    * @param thisDefault
    *   default element used in intervals where data are valid in that but not this.
    * @param thatDefault
    *   default element used in intervals where data are valid in this but not that.
    * @tparam B
    *   value type of that structure.
    * @return
    *   a new structure with this and that value type as a pair.
    */
  def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): DimensionalBase[(V, B), D]

  /**
    * Project as data in n-1 dimensions based on a lookup in the head dimension.
    *
    * (Equivalent to `getByDimension[H, NonEmptyTail[D]](0, domain)`, though the type checking is simpler)
    *
    * @tparam H
    *   the domain value type of the 1D domain used for filtering. There are type safety checks that ensure
    *   - the head 1D domain has the specified domain value type
    *   - the current domain tail is a non-empty domain (i.e., the current domain type `D` has at least two dimensions)
    *   - the current domain type can be constructed by concatenating the 1D domain type specified and the current
    *     domain tail.
    * @param domain
    *   the head dimension domain element
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Head[D] =:= Domain1D[H],
    Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): DimensionalBase[V, NonEmptyTail[D]]

  /**
    * Project as data in n-1 dimensions based on a lookup in the specified dimension.
    *
    * @param dimensionIndex
    *   dimension to filter on and drop. Must be a value with a singleton type known at compile time, e.g., a numeric
    *   literal. (The head dimension is dimension 0.)
    * @param domain
    *   the domain element used for filtering
    * @tparam H
    *   the domain value type of the domain used for filtering. There are type safety checks that ensure
    *   - the 1D domain at the specified dimension index has the specified domain value type
    *   - the current domain type can be constructed by concatenating the elements before the domain, the domain itself,
    *     and the elements after the domain.
    * @tparam R
    *   domain of intervals in the returned structure. There is a type safety check that ensures the domain type for
    *   this result type can be constructed by concatenating the elements before and after the dropped dimension.
    * @return
    *   a lower-dimensional (n-1) projection
    */
  def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int,
    domain: Domain1D[H]
  )(using
    Elem[D, dimensionIndex.type] =:= Domain1D[H],
    Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D,
    Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
  ): DimensionalBase[V, R]

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: intervalidus.mutable.MutableBase[V, D]

  /**
    * Returns this as an immutable structure.
    */
  def toImmutable: intervalidus.immutable.ImmutableBase[V, D, ?]
