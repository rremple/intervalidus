package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalVersionedBase.*
import intervalidus.DiscreteValue.IntDiscreteValue

import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps

/** $objectDesc */
object DataVersioned extends DimensionalVersionedBaseObject[DataVersioned]:
  type In1D[V, R1] = DataVersioned[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataVersioned[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataVersioned[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataVersioned[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, Versioned[D]]],
    initialVersion: VersionDomainValue = 0,
    initialComment: String = "init"
  )(using config: CoreConfig[Versioned[D]])(using DomainLike[Versioned[D]], CurrentInstant): DataVersioned[V, D] =
    new DataVersioned[V, D](
      initialData,
      initialVersion,
      mutable.Map(initialVersion -> (summon[CurrentInstant].now(), initialComment))
    )

/**
  * Immutable versioned dimensional data in any dimension.
  *
  * Interface is similar to [[Data]], but it operates on an underlying [[intervalidus.mutable.Data]] using an extra
  * integer-valued head dimension to version data. $classDescUseCase Most methods require some generic version selection
  * criteria rather than specific integer intervals, therefore this does not extend [[Data]].
  *
  * $classDescFeatures
  *
  * @note
  *   $classNote
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataVersioned[V, D <: NonEmptyTuple: DomainLike](
  initialData: Iterable[ValidData[V, Versioned[D]]] = Iterable.empty[ValidData[V, Versioned[D]]],
  initialVersion: VersionDomainValue = 0,
  versionTimestamps: mutable.Map[VersionDomainValue, VersionMetadata] = mutable.Map.empty,
  withCurrentVersion: Option[VersionDomainValue] = None
)(using
  val config: CoreConfig[Versioned[D]]
)(using
  DomainLike[Versioned[D]],
  CurrentInstant
) extends DimensionalVersionedBase[V, D](initialData, initialVersion, versionTimestamps, withCurrentVersion):

  if versionTimestamps.isEmpty then addVersionTimestampInPlace("init")

  private def copyAndModify(f: DataVersioned[V, D] => Unit): DataVersioned[V, D] =
    val result = copy
    f(result)
    result

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override protected def resetTo(selection: VersionSelection): DataVersioned[V, D] = selection match
    case VersionSelection.Unapproved        => this
    case VersionSelection.Current           => resetToVersion(currentVersion)
    case VersionSelection.Specific(version) => resetToVersion(version)

  override def copy: DataVersioned[V, D] = new DataVersioned(
    underlying.getAll,
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )

  override def zip[B](that: DimensionalVersionedBase[B, D]): DataVersioned[(V, B), D] =
    new DataVersioned(
      underlying.zip(that.getVersionedData).getAll,
      initialVersion,
      mergeVersionTimestamps(that),
      Some(currentVersion)
    )

  override def zipAll[B](
    that: DimensionalVersionedBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): DataVersioned[(V, B), D] =
    new DataVersioned(
      underlying.zipAll(that.getVersionedData, thisDefault, thatDefault).getAll,
      initialVersion,
      mergeVersionTimestamps(that),
      Some(currentVersion)
    )

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]],
    DomainLike[Versioned[Domain.NonEmptyTail[D]]]
  )(using altConfig: CoreConfig[Versioned[Domain.NonEmptyTail[D]]]): DataVersioned[V, Domain.NonEmptyTail[D]] =
    new DataVersioned(
      getByHeadDimensionData(domain),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    ).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R],
    DomainLike[Versioned[R]]
  )(using altConfig: CoreConfig[Versioned[R]]): DataVersioned[V, R] =
    new DataVersioned(
      getByDimensionData[H, R](dimensionIndex, domain),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    ).compressAll()

  override def toImmutable: DataVersioned[V, D] = this

  override def toMutable: intervalidus.mutable.DataVersioned[V, D] = new intervalidus.mutable.DataVersioned(
    underlying.getAll,
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )

  // ------ Implement methods similar to those from ImmutableVersionedBase, but with a version selection context ------

  /**
    * $setDesc
    *
    * @param data
    *   $setParamData
    * @return
    *   $immutableReturn
    */
  def set(data: ValidData[V, D])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying + underlyingValidDataFromVersionBoundary(data))

  /**
    * $setManyDesc
    *
    * @note
    *   $setManyNote
    * @param data
    *   $setManyParamData
    * @return
    *   $immutableReturn
    */
  def setMany(data: IterableOnce[ValidData[V, D]])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying ++ data.iterator.map(underlyingValidDataFromVersionBoundary))

  /**
    * $setIfNoConflictDesc
    *
    * @param data
    *   $setIfNoConflictParamData
    * @return
    *   Some new structure data if there were no conflicts and new data was set, None otherwise.
    */
  def setIfNoConflict(data: ValidData[V, D])(using VersionSelection): Option[DataVersioned[V, D]] =
    val result = copy
    val updated = result.underlying.setIfNoConflict(underlyingValidDataFromVersionBoundary(data))
    if updated then Some(result) else None

  /**
    * $updateDesc
    *
    * @param data
    *   $updateParamData
    * @return
    *   $immutableReturn
    */
  def update(data: ValidData[V, D])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.update(underlyingValidDataFromVersionBoundary(data)))

  /**
    * $removeDesc
    *
    * @param interval
    *   $removeParamInterval
    * @return
    *   $immutableReturn
    */
  def remove(interval: Interval[D])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying - underlyingIntervalFromVersionBoundary(interval))

  /**
    * $removeManyDesc
    *
    * @param intervals
    *   $removeManyParamIntervals
    * @return
    *   $immutableReturn
    */
  def removeMany(intervals: IterableOnce[Interval[D]])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying -- intervals.iterator.map(underlyingIntervalFromVersionBoundary))

  /**
    * $removeValueDesc
    *
    * @param value
    *   $removeValueParamValue
    * @return
    *   $immutableReturn
    */
  def removeValue(value: V)(using VersionSelection): DataVersioned[V, D] = copyAndModify: result =>
    intervals(value).foreach: interval =>
      result.underlying - underlyingIntervalFromVersionBoundary(interval)

  /**
    * $fillDesc
    *
    * @param data
    *   $fillParamData
    * @return
    *   $immutableReturn
    */
  def fill(data: ValidData[V, D])(using VersionSelection): DataVersioned[V, D] =
    copyAndModify(_.underlying.fill(underlyingValidDataFromVersionBoundary(data)))

  // ------ Implement methods similar to those from ImmutableVersionedBase, without version selection context ------

  /**
    * $compressDesc $noVersionSelection
    *
    * @param value
    *   $compressParamValue
    * @return
    *   $immutableReturn
    */
  def compress(value: V): DataVersioned[V, D] = copyAndModify(_.underlying.compress(value))

  /**
    * $compressAllDesc $noVersionSelection
    *
    * @return
    *   $immutableReturn
    */
  def compressAll(): DataVersioned[V, D] = copyAndModify(_.underlying.compressAll())

  /**
    * $recompressAllDesc $noVersionSelection
    *
    * @param otherIntervals
    *   $recompressAllParamOtherIntervals
    * @return
    *   $immutableReturn
    */
  def recompressAll(otherIntervals: IterableOnce[Interval[Versioned[D]]] = Iterable.empty): DataVersioned[V, D] =
    copyAndModify(_.underlying.recompressAll(otherIntervals))

  /**
    * $applyDiffActionsDesc $noVersionSelection
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    * @return
    *   $immutableReturn
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[V, Versioned[D]]]): DataVersioned[V, D] =
    copyAndModify(_.underlying.applyDiffActions(diffActions))

  /**
    * $syncWithDesc $noVersionSelection
    *
    * @param that
    *   $syncWithParamThat
    * @return
    *   $immutableReturn
    */
  def syncWith(that: DataVersioned[V, D]): DataVersioned[V, D] =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * $filterDesc
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param p
    *   $filterParamP
    * @return
    *   a new structure with the same current version consisting of all elements that satisfy the provided predicate p.
    */
  def filter(p: ValidData[V, Versioned[D]] => Boolean): DataVersioned[V, D] = new DataVersioned(
    underlying.getAll.filter(p),
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )

  /**
    * $mapDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param f
    *   $mapParamF
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, Versioned[D]] => ValidData[B, Versioned[S]]
  )(using
    DomainLike[Versioned[S]]
  )(using altConfig: CoreConfig[Versioned[S]]): DataVersioned[B, S] =
    new DataVersioned(
      underlying.getAll.map(f),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    )(using config = altConfig)

  /**
    * $collectDesc Both the valid data value and interval types can be changed in the mapping.
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param pf
    *   $collectParamPf
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data versioned interval domain type of the returned structure.
    * @return
    *   a new structure resulting from applying the provided function to each versioned element of this structure on
    *   which it is defined.
    */
  def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, Versioned[D]], ValidData[B, Versioned[S]]]
  )(using
    DomainLike[Versioned[S]]
  )(using altConfig: CoreConfig[Versioned[S]]): DataVersioned[B, S] =
    new DataVersioned(
      underlying.getAll.collect(pf),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    )(using config = altConfig)

  /**
    * $mapValuesDesc Only the valid data value type can be changed in the mapping.
    *
    * $noVersionSelectionFunction maps all values in all versions.
    *
    * @param f
    *   $mapValuesParamF
    * @tparam B
    *   the valid data value type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure.
    */
  def mapValues[B](f: V => B): DataVersioned[B, D] = new DataVersioned(
    underlying.getAll.map(d => d.copy(value = f(d.value))),
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )

  /**
    * $mapIntervalsDesc The interval type can be changed in the mapping.
    *
    * $noVersionSelectionFunction maps all intervals in all versions.
    *
    * @param f
    *   $mapIntervalsParamF
    * @tparam S
    *   the valid data versioned interval domain type of the returned structure.
    *
    * @return
    *   a new structure resulting from applying the provided function f to each versioned interval.
    */
  def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[Versioned[D]] => Interval[Versioned[S]]
  )(using
    DomainLike[Versioned[S]]
  )(using altConfig: CoreConfig[Versioned[S]]): DataVersioned[V, S] = new DataVersioned(
    underlying.getAll.map(d => d.copy(interval = f(d.interval))),
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )(using config = altConfig).compressAll()

  /**
    * $flatMapDesc and builds a new structure by concatenating the elements of the resulting structures.
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param f
    *   $flatMapParamF
    * @tparam B
    *   the valid data value type of the returned structure.
    * @tparam S
    *   the valid data interval domain type of the returned structure.
    * @return
    *   a new structure with the same current version resulting from applying the provided function f to each element of
    *   this structure and concatenating the results.
    */
  def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, Versioned[D]] => DataVersioned[B, S]
  )(using
    DomainLike[Versioned[S]]
  )(using altConfig: CoreConfig[Versioned[S]]): DataVersioned[B, S] =
    new DataVersioned(
      underlying.getAll.flatMap(f(_).underlying.getAll),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    )(using config = altConfig)

  /**
    * $mergeDesc
    *
    * @param that
    *   $mergeParamThat
    * @param mergeValues
    *   $mergeParamMergeValues
    * @return
    *   a new structure with that merged in
    */
  def merge(
    that: DimensionalVersionedBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): DataVersioned[V, D] = copyAndModify: result =>
    result.underlying.merge(that.getVersionedData, mergeValues)
    result.mergeVersionTimestampsInPlace(that)

  // --- API methods unique to this "versioned" variant

  /**
    * $setCurrentVersionDesc
    * @throws Exception
    *   if the version is too large
    * @param version
    *   $setCurrentVersionParamVersion
    * @return
    *   a new structure with the current version set.
    */
  def setCurrentVersion(
    version: VersionDomainValue,
    comment: String = "version set"
  )(using CurrentInstant): DataVersioned[V, D] =
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else
      copyAndModify: result =>
        result.currentVersion = version
        result.addVersionTimestampInPlace(comment)

  /**
    * $incrementCurrentVersionDesc
    * @throws Exception
    *   if we run out of versions
    * @return
    *   a new structure with the current version incremented.
    */
  def incrementCurrentVersion(comment: String = "incremented")(using CurrentInstant): DataVersioned[V, D] =
    if currentVersion + 1 equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else
      copyAndModify: result =>
        result.currentVersion = currentVersion + 1
        result.addVersionTimestampInPlace(comment)

  /**
    * $resetToVersionDesc
    *
    * @param version
    *   $resetToVersionParamVersion
    * @return
    *   a new structure reset to the specified version, with the current version set to the same.
    */
  def resetToVersion(version: VersionDomainValue): DataVersioned[V, D] =
    val keep = VersionSelection(version)
    new DataVersioned(
      underlying.getAll
        .filter(versionInterval(_) intersects keep.intervalTo)
        .map(extendInterval(keep)),
      initialVersion,
      mutable.Map.from(versionTimestamps.view.filterKeys(_ <= version)),
      Some(version)
    ).compressAll()

  /**
    * $collapseVersionHistoryDesc
    *
    * @return
    *   a new structure with the version history collapsed.
    */
  def collapseVersionHistory(using VersionSelection): DataVersioned[V, D] =
    DataVersioned.from(getAll, initialVersion)

  /**
    * $approveDesc
    *
    * @param data
    *   $approveDesc
    * @return
    *   some new structure if the unapproved version was found and approved, None otherwise
    */
  def approve(data: ValidData[V, D]): Option[DataVersioned[V, D]] =
    val allUnapproved = underlying
      .getIntersecting(underlyingIntervalWithVersion(data.interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersionDomain) // only unapproved
    allUnapproved.headOption match
      case Some(d) if publicValidData(d) == data =>
        Some(set(data)(using VersionSelection.Current))
      case _ =>
        None

  /**
    * $approveAllDesc
    *
    * @param interval
    *   $approveAllParamInterval
    * @return
    *   a new structure with all unapproved changes approved in the interval.
    */
  def approveAll(interval: Interval[D]): DataVersioned[V, D] =
    val approved = underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersionDomain) // only unapproved
      .map(publicValidData)
      .foldLeft(this): (prev, d) =>
        prev.approve(d).getOrElse(prev)
    approved.underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv approvedEndVersionDomain) // only related to unapproved removes
      .flatMap(publicValidData(_).interval ∩ interval)
      .foldLeft(approved): (prev, i) =>
        prev.remove(i)(using VersionSelection.Current)

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * $setDesc
    *
    * @param data
    *   $setParamData
    * @return
    *   $immutableReturn
    */
  infix def +(data: ValidData[V, D])(using VersionSelection): DataVersioned[V, D] = set(data)

  /**
    * Same as [[setMany]]
    *
    * $setManyDesc
    *
    * @note
    *   $setManyNote
    * @param data
    *   $setManyParamData
    * @return
    *   $immutableReturn
    */
  infix def ++(data: IterableOnce[ValidData[V, D]])(using VersionSelection): DataVersioned[V, D] = setMany(data)

  /**
    * Same as [[remove]]
    *
    * $removeDesc
    *
    * @param interval
    *   $removeParamInterval
    * @return
    *   $immutableReturn
    */
  infix def -(interval: Interval[D])(using VersionSelection): DataVersioned[V, D] = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * $removeManyDesc
    *
    * @param intervals
    *   $removeManyParamIntervals
    * @return
    *   $immutableReturn
    */
  infix def --(intervals: IterableOnce[Interval[D]])(using VersionSelection): DataVersioned[V, D] =
    removeMany(intervals)

// These may be problematic/misunderstood in the versioned space, so leaving them out for now.
//  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): DataVersioned[V, D]
//  def replaceByKey(key: D, newData: ValidData[V, D]): DataVersioned[V, D]
