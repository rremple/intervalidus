package intervalidus.mutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomain, VersionDomainValue, VersionSelection, Versioned}
import intervalidus.DiscreteValue.IntDiscreteValue
import intervalidus.Domain.NonEmptyTail

import scala.collection.mutable
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/** @inheritdoc */
object DataVersioned extends DimensionalVersionedBaseObject:
  type In1D[V, R1] = DataVersioned[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataVersioned[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataVersioned[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataVersioned[V, Domain.In4D[R1, R2, R3, R4]]

  override def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D],
    initialVersion: VersionDomainValue
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] = from(
    Iterable(data),
    initialVersion
  )

  override def of[V, D <: NonEmptyTuple: DomainLike](
    value: V,
    initialVersion: VersionDomainValue = 0
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] =
    of(Interval.unbounded[D] -> value, initialVersion)

  override def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]],
    initialVersion: VersionDomainValue = 0
  )(using Experimental, DomainLike[Versioned[D]]): DataVersioned[V, D] = DataVersioned[V, D](
    initialData.map(d => (d.interval withHead Interval1D.intervalFrom(initialVersion)) -> d.value),
    initialVersion
  )

  override def newBuilder[V, D <: NonEmptyTuple: DomainLike](
    initialVersion: VersionDomainValue = 0
  )(using
    Experimental,
    DomainLike[Versioned[D]]
  ): mutable.Builder[ValidData[V, D], DataVersioned[V, D]] =
    DimensionalDataVersionedBuilder[V, D, DataVersioned[V, D]](from(_, initialVersion))

/**
  * Immutable versioned dimensional data in any dimension.
  *
  * Interface is similar to [[Data]], but it operates on an underlying [[mutable.Data]] using an extra integer-valued
  * head dimension to version data. One use case would be versioned data that are valid in two dimensions of time, so
  * the underlying data actually vary in terms of version and two dimensions of time (three dimensions total). Most
  * methods require some generic version selection criteria rather than specific integer intervals, therefore this does
  * not extend [[DimensionalBase]].
  *
  * The "current" version is managed as state (a `var`). Versioning also separates notions of approved vs. unapproved
  * data (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too.
  * @note
  *   Updates starting with "current" also update unapproved changes (since intervalFrom goes to the Top).
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals in the public interface -- [[DomainLike]] non-empty tuples.
  */
class DataVersioned[V, D <: NonEmptyTuple: DomainLike](
  initialData: Iterable[ValidData[V, Versioned[D]]] = Iterable.empty[ValidData[V, Versioned[D]]],
  initialVersion: VersionDomainValue = 0,
  withCurrentVersion: Option[VersionDomain] = None
)(using
  Experimental,
  DomainLike[Versioned[D]]
) extends DimensionalVersionedBase[V, D](initialData, initialVersion, withCurrentVersion):

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override def copy: DataVersioned[V, D] = DataVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def zip[B](that: DimensionalVersionedBase[B, D]): DataVersioned[(V, B), D] =
    DataVersioned(
      underlying.zip(that.getVersionedData).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](
    that: DimensionalVersionedBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): DataVersioned[(V, B), D] =
    DataVersioned(
      underlying.zipAll(that.getVersionedData, thisDefault, thatDefault).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def getByHeadIndex[H: DomainValueLike](headIndex: Domain1D[H])(using
    Tuple.Head[D] =:= Domain1D[H],
    Tuple.Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tuple.Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]],
    DomainLike[Versioned[NonEmptyTail[D]]]
  ): DataVersioned[V, NonEmptyTail[D]] =
    DataVersioned(
      getByHeadIndexData(headIndex),
      initialVersion,
      Some(currentVersion)
    )

  override def toImmutable: intervalidus.immutable.DataVersioned[V, D] = intervalidus.immutable.DataVersioned(
    underlying.getAll,
    initialVersion,
    Some(currentVersion)
  )

  override def toMutable: DataVersioned[V, D] = this

  // ------ Implement methods similar to those from MutableVersionedBase, but with a version selection context ------

  /**
    * Set new valid data. Given a version selection context, any data previously valid in this interval are replaced by
    * this data.
    *
    * @param data
    *   the valid data to set.
    */
  def set(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.set(underlyingValidDataFromVersionBoundary(data))

  /**
    * Set a collection of new valid data. Given a version selection context, any data previously valid in the intervals
    * are replaced by these data.
    *
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    * @param data
    *   collection of valid data to set.
    */
  def setMany(data: Iterable[ValidData[V, D]])(using VersionSelection): Unit =
    underlying.setMany(data.map(underlyingValidDataFromVersionBoundary))

  /**
    * Set new valid data, but only if there are no previously valid values in its interval and given the version
    * selection context.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(newData: ValidData[V, D])(using VersionSelection): Boolean =
    underlying.setIfNoConflict(underlyingValidDataFromVersionBoundary(newData))

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. No new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param data
    *   the new value and interval existing data should take on.
    */
  def update(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.update(underlyingValidDataFromVersionBoundary(data))

  /**
    * Remove valid values on the interval and the given version selection context. If there are values valid on portions
    * of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  def remove(interval: Interval[D])(using VersionSelection): Unit =
    underlying.remove(underlyingIntervalFromVersionBoundary(interval))

  /**
    * Remove data in all the intervals given a version selection context. If there are values valid on portions of any
    * interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    */
  def removeMany(intervals: Iterable[Interval[D]])(using VersionSelection): Unit =
    underlying.removeMany(intervals.map(underlyingIntervalFromVersionBoundary))

  /**
    * Remove the value in all the intervals where it is valid in the given version selection context.
    *
    * @param value
    *   the value that is removed.
    */
  def removeValue(value: V)(using VersionSelection): Unit =
    intervals(value).foreach: interval =>
      underlying.remove(underlyingIntervalFromVersionBoundary(interval))

  /**
    * Given the version selection context, adds a value as valid in portions of the interval where there aren't already
    * valid values.
    *
    * @param data
    *   value to make valid in any validity gaps found in the interval
    */
  def fill(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.fill(underlyingValidDataFromVersionBoundary(data))

  // ------ Implement methods similar to those from MutableVersionedBase, without version selection context ------

  /**
    * Compress out adjacent intervals with the same value.
    *
    * @param value
    *   value for which valid data are compressed.
    */
  def compress(value: V): Unit = underlying.compress(value)

  /**
    * Compress out adjacent intervals with the same value for all values. (Shouldn't ever need to do this.)
    */
  def compressAll(): Unit = underlying.compressAll()

  /**
    * Compress out adjacent intervals with the same value for all values after decompressing everything, resulting in a
    * unique physical representation. (Shouldn't ever need to do this.)
    *
    * Does not use a version selection context -- operates on full underlying structure.
    */
  def recompressAll(): Unit = underlying.recompressAll()

  /**
    * Applies a sequence of diff actions to this structure.
    *
    * Does not use a version selection context -- operates on full underlying structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction[V, Versioned[D]]]): Unit =
    underlying.applyDiffActions(diffActions)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions.
    *
    * Does not use a version selection context -- operates on full underlying structure.
    *
    * @param that
    *   the structure with which this is synchronized.
    */
  def syncWith(that: DataVersioned[V, D]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * Does not use a version selection context -- the predicate is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData[V, Versioned[D]] => Boolean): Unit = underlying.filter(p)

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    */
  def map(f: ValidData[V, Versioned[D]] => ValidData[V, Versioned[D]]): Unit = underlying.map(f)

  /**
    * Applies a partial function to all valid data on which it is defined. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param pf
    *   the partial function to apply to each versioned data element.
    */
  def collect(
    pf: PartialFunction[ValidData[V, Versioned[D]], ValidData[V, Versioned[D]]]
  ): Unit = underlying.collect(pf)

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it maps all values
    * in all versions.
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit = underlying.mapValues(f)

  /**
    * Applies a function to all valid data intervals. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it maps all
    * intervals in all versions.
    *
    * @param f
    *   the function to apply to the versioned interval part of each valid data element.
    */
  def mapIntervals(f: Interval[Versioned[D]] => Interval[Versioned[D]]): Unit = underlying.mapIntervals(f)

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * Does not use a version selection context -- the function is applied to the underlying data, so it can operate on
    * the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    */
  def flatMap(f: ValidData[V, Versioned[D]] => DataVersioned[V, D]): Unit =
    underlying.flatMap(f(_).underlying)

  /**
    * Merges this structure with data from that structure. In intervals where both structures have valid values, the two
    * values are merged (e.g., keep this data). In intervals where this does not have valid data but that does, the data
    * are added (a fill operation).
    *
    * @param that
    *   versioned structure to merge into this one
    * @param mergeValues
    *   function that merges values where both this and that have valid values, where the default merge operation is to
    *   give this data values priority and drop that data values
    */
  def merge(
    that: DimensionalVersionedBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = underlying.merge(that.getVersionedData, mergeValues)

  // --- API methods unique to this "versioned" variant

  /**
    * Sets the current version. No version history is rewritten, which may cause some unexpected results (especially if
    * the version is set to something from the past). Use with caution.
    *
    * @param version
    *   the new current version
    */
  def setCurrentVersion(version: VersionDomain): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv Domain1D.Bottom then throw Exception("version too small")
    else currentVersion = version

  /**
    * Increments the current version.
    */
  def incrementCurrentVersion(): Unit = synchronized:
    if currentVersion.rightAdjacent equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else currentVersion = currentVersion.rightAdjacent

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    */
  def resetToVersion(version: VersionDomain): Unit =
    val keep = VersionSelection(version)
    filter(versionInterval(_) intersects keep.intervalTo)
    map(d =>
      if versionInterval(d).end >= keep.boundary
      then withVersionUpdate(d, _.toTop)
      else d
    )
    setCurrentVersion(version)
    compressAll()

  /**
    * Updates the existing structure to include only data based on the version selection context, but without any
    * version history.
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): Unit = synchronized:
    filter(versionInterval(_) contains versionSelection.boundary)
    map(d => withVersionUpdate(d, _ => Interval1D.intervalFrom(initialVersion)))
    compressAll()
    setCurrentVersion(initialVersion)

  /**
    * Special case where we change the version interval by approving everything that is unapproved.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   true if the unapproved version was found and approved, false otherwise
    */
  def approve(data: ValidData[V, D]): Boolean =
    val allUnapproved = underlying
      .getIntersecting(underlyingIntervalWithVersion(data.interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
    allUnapproved.headOption match
      case Some(d) if publicValidData(d) == data =>
        set(data)(using VersionSelection.Current)
        true
      case other =>
        false

  /**
    * Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
    *
    * @param interval
    *   interval in which all changes (updates and deletes) are approved
    */
  def approveAll(interval: Interval[D]): Unit =
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersion) // only unapproved
      .map(publicValidData)
      .foreach(approve)
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv unapprovedStartVersion.leftAdjacent) // only related to unapproved removes
      .flatMap(publicValidData(_).interval intersectionWith interval)
      .foreach(remove(_)(using VersionSelection.Current))

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * Set new valid data. Given a version selection context, any data previously valid in this interval are replaced by
    * this data.
    *
    * @param data
    *   the valid data to set.
    */
  infix def +(data: ValidData[V, D])(using VersionSelection): Unit = set(data)

  /**
    * Same as [[setMany]]
    *
    * Set a collection of new valid data. Given a version selection context, any data previously valid in the intervals
    * are replaced by these data.
    *
    * @note
    *   if intervals overlap, later items will update earlier ones, so order can matter.
    * @param data
    *   collection of valid data to set.
    */
  infix def ++(data: Iterable[ValidData[V, D]])(using VersionSelection): Unit = setMany(data)

  /**
    * Same as [[remove]]
    *
    * Remove valid values on the interval and the given version selection context. If there are values valid on portions
    * of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    */
  infix def -(interval: Interval[D])(using VersionSelection): Unit = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * Remove data in all the intervals given a version selection context. If there are values valid on portions of any
    * interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param intervals
    *   the interval where any valid values are removed.
    */
  infix def --(intervals: Iterable[Interval[D]])(using VersionSelection): Unit =
    removeMany(intervals)

// These may be problematic/misunderstood in the versioned space, so leaving them out for now.
//  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit
//  def replaceByKey(key: D, newData: ValidData[V, D]): Unit
