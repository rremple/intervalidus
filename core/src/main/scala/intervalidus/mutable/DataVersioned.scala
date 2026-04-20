package intervalidus.mutable

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
  * Mutable versioned dimensional data in any dimension.
  *
  * Interface is similar to [[Data]], but it operates on an underlying [[Data]] using an extra integer-valued head
  * dimension to version data. $classDescUseCase Most methods require some generic version selection criteria rather
  * than specific integer intervals, therefore this does not extend [[Data]].
  *
  * $classDescFeatures
  *
  * @note
  *   $classNote
  *
  * @param config
  *   $configParam
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

  // ---------- Implement methods from DimensionalVersionedBase ----------

  override protected def resetTo(selection: VersionSelection): DataVersioned[V, D] = selection match
    case VersionSelection.Unapproved                                               => this
    case notUnapproved @ (VersionSelection.Current | VersionSelection.Specific(_)) =>
      val resetCopy = copy
      notUnapproved match
        case VersionSelection.Current           => resetCopy.resetToVersion(currentVersion)
        case VersionSelection.Specific(version) => resetCopy.resetToVersion(version)
      resetCopy

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
    val result = new DataVersioned(
      getByHeadDimensionData(domain),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    )
    result.compressAll()
    result

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
    val result = new DataVersioned(
      getByDimensionData[H, R](dimensionIndex, domain),
      initialVersion,
      versionTimestamps.clone(),
      Some(currentVersion)
    )
    result.compressAll()
    result

  override def toImmutable: intervalidus.immutable.DataVersioned[V, D] = new intervalidus.immutable.DataVersioned(
    underlying.getAll,
    initialVersion,
    versionTimestamps.clone(),
    Some(currentVersion)
  )

  override def toMutable: DataVersioned[V, D] = this

  // ------ Implement methods similar to those from MutableVersionedBase, but with a version selection context ------

  /**
    * $setDesc $mutableAction
    *
    * @param data
    *   $setParamData
    */
  def set(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying + underlyingValidDataFromVersionBoundary(data)

  /**
    * $setManyDesc $mutableAction
    *
    * @note
    *   $setManyNote
    * @param data
    *   $setManyParamData
    */
  def setMany(data: IterableOnce[ValidData[V, D]])(using VersionSelection): Unit =
    underlying ++ data.iterator.map(underlyingValidDataFromVersionBoundary)

  /**
    * $setIfNoConflictDesc $mutableAction
    *
    * @param data
    *   $setIfNoConflictParamData
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(data: ValidData[V, D])(using VersionSelection): Boolean =
    underlying.setIfNoConflict(underlyingValidDataFromVersionBoundary(data))

  /**
    * $updateDesc $mutableAction
    *
    * @param data
    *   $updateParamData
    */
  def update(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.update(underlyingValidDataFromVersionBoundary(data))

  /**
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  def remove(interval: Interval[D])(using VersionSelection): Unit =
    underlying - underlyingIntervalFromVersionBoundary(interval)

  /**
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  def removeMany(intervals: IterableOnce[Interval[D]])(using VersionSelection): Unit =
    underlying -- intervals.iterator.map(underlyingIntervalFromVersionBoundary)

  /**
    * $removeValueDesc $mutableAction
    *
    * @param value
    *   $removeValueParamValue
    */
  def removeValue(value: V)(using VersionSelection): Unit =
    intervals(value).foreach: interval =>
      underlying - underlyingIntervalFromVersionBoundary(interval)

  /**
    * $fillDesc $mutableAction
    *
    * @param data
    *   $fillParamData
    */
  def fill(data: ValidData[V, D])(using VersionSelection): Unit =
    underlying.fill(underlyingValidDataFromVersionBoundary(data))

  // ------ Implement methods similar to those from MutableVersionedBase, without version selection context ------

  /**
    * $compressDesc $mutableAction $noVersionSelection
    *
    * @param value
    *   $compressParamValue
    */
  def compress(value: V): Unit = underlying.compress(value)

  /**
    * $compressAllDesc $mutableAction $noVersionSelection
    */
  def compressAll(): Unit = underlying.compressAll()

  /**
    * $recompressAllDesc $mutableAction $noVersionSelection
    *
    * @param otherIntervals
    *   $recompressAllParamOtherIntervals
    */
  def recompressAll(otherIntervals: IterableOnce[Interval[Versioned[D]]] = Iterable.empty): Unit =
    underlying.recompressAll(otherIntervals)

  /**
    * $applyDiffActionsDesc $mutableAction $noVersionSelection
    *
    * @param diffActions
    *   $applyDiffActionsParamDiffActions
    */
  def applyDiffActions(diffActions: IterableOnce[DiffAction[V, Versioned[D]]]): Unit =
    underlying.applyDiffActions(diffActions)

  /**
    * $syncWithDesc $mutableAction $noVersionSelection
    *
    * @param that
    *   $syncWithParamThat
    */
  def syncWith(that: DataVersioned[V, D]): Unit =
    applyDiffActions(that.diffActionsFrom(this))

  /**
    * $filterDesc $mutableAction
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param p
    *   $filterParamP
    */
  def filter(p: ValidData[V, Versioned[D]] => Boolean): Unit = underlying.filter(p)

  /**
    * $mapDesc $mutableAction
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param f
    *   $mapParamF
    */
  def map(f: ValidData[V, Versioned[D]] => ValidData[V, Versioned[D]]): Unit = underlying.map(f)

  /**
    * $collectDesc $mutableAction
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param pf
    *   $collectParamPf
    */
  def collect(
    pf: PartialFunction[ValidData[V, Versioned[D]], ValidData[V, Versioned[D]]]
  ): Unit = underlying.collect(pf)

  /**
    * $mapValuesDesc $mutableAction
    *
    * $noVersionSelectionFunction maps all values in all versions.
    *
    * @param f
    *   $mapValuesParamF
    */
  def mapValues(f: V => V): Unit = underlying.mapValues(f)

  /**
    * $mapIntervalsDesc $mutableAction
    *
    * $noVersionSelectionFunction maps all intervals in all versions.
    *
    * @param f
    *   $mapIntervalsParamF
    */
  def mapIntervals(f: Interval[Versioned[D]] => Interval[Versioned[D]]): Unit = underlying.mapIntervals(f)

  /**
    * $flatMapDesc and updates valid values from the elements of the resulting structures. $mutableAction
    *
    * $noVersionSelectionFunction $noVersionSelectionApply
    *
    * @param f
    *   $flatMapParamF
    */
  def flatMap(f: ValidData[V, Versioned[D]] => DataVersioned[V, D]): Unit =
    underlying.flatMap(f(_).underlying)

  /**
    * $mergeDesc $mutableAction
    *
    * @param that
    *   $mergeParamThat
    * @param mergeValues
    *   $mergeParamMergeValues
    */
  def merge(
    that: DimensionalVersionedBase[V, D],
    mergeValues: (V, V) => V = (thisDataValue, _) => thisDataValue
  ): Unit = synchronized:
    underlying.merge(that.getVersionedData, mergeValues)
    mergeVersionTimestampsInPlace(that)

  // --- API methods unique to this "versioned" variant

  /**
    * $setCurrentVersionDesc $mutableAction
    * @throws Exception
    *   if the version is too large
    * @param version
    *   $setCurrentVersionParamVersion
    */
  def setCurrentVersion(
    version: VersionDomainValue,
    comment: String = "version set"
  )(using CurrentInstant): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else
      currentVersion = version
      addVersionTimestampInPlace(comment)

  /**
    * $incrementCurrentVersionDesc $mutableAction
    * @throws Exception
    *   if we run out of versions
    */
  def incrementCurrentVersion(comment: String = "incremented")(using CurrentInstant): Unit = synchronized:
    if currentVersion + 1 == unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else
      currentVersion = currentVersion + 1
      addVersionTimestampInPlace(comment)

  /**
    * $resetToVersionDesc $mutableAction
    *
    * @param version
    *   $resetToVersionParamVersion
    */
  def resetToVersion(version: VersionDomainValue): Unit = synchronized:
    val keep = VersionSelection(version)
    filter(versionInterval(_) intersects keep.intervalTo)
    map(extendInterval(keep))
    setCurrentVersion(version)
    versionTimestamps.filterInPlace((key, _) => key <= version)
    compressAll()

  /**
    * $collapseVersionHistoryDesc $mutableAction
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): Unit = synchronized:
    filter(versionInterval(_) contains versionSelection.boundary)
    map(d => withVersionUpdate(d, _ => Interval1D.intervalFrom(Domain1D.domain(initialVersion))))
    compressAll()
    setCurrentVersion(initialVersion)

  /**
    * $approveDesc $mutableAction
    *
    * @param data
    *   $approveDesc
    * @return
    *   true if the unapproved version was found and approved, false otherwise
    */
  def approve(data: ValidData[V, D]): Boolean = synchronized:
    val allUnapproved = underlying
      .getIntersecting(underlyingIntervalWithVersion(data.interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersionDomain) // only unapproved
    allUnapproved.headOption match
      case Some(d) if publicValidData(d) == data =>
        set(data)(using VersionSelection.Current)
        true
      case _ =>
        false

  /**
    * $approveAllDesc $mutableAction
    *
    * @param interval
    *   $approveAllParamInterval
    */
  def approveAll(interval: Interval[D]): Unit = synchronized:
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Unapproved.intervalFrom))
      .filter(versionInterval(_).start equiv unapprovedStartVersionDomain) // only unapproved
      .map(publicValidData)
      .foreach(approve)
    underlying
      .getIntersecting(underlyingIntervalWithVersion(interval, VersionSelection.Current.intervalFrom))
      .filter(versionInterval(_).end equiv approvedEndVersionDomain) // only related to unapproved removes
      .flatMap(publicValidData(_).interval ∩ interval)
      .foreach(remove(_)(using VersionSelection.Current))

  // equivalent symbolic method names

  /**
    * Same as [[set]]
    *
    * $setDesc $mutableAction
    *
    * @param data
    *   $setParamData
    */
  infix def +(data: ValidData[V, D])(using VersionSelection): Unit = set(data)

  /**
    * Same as [[setMany]]
    *
    * $setManyDesc $mutableAction
    *
    * @note
    *   $setManyNote
    * @param data
    *   $setManyParamData
    */
  infix def ++(data: IterableOnce[ValidData[V, D]])(using VersionSelection): Unit = setMany(data)

  /**
    * Same as [[remove]]
    *
    * $removeDesc $mutableAction
    *
    * @param interval
    *   $removeParamInterval
    */
  infix def -(interval: Interval[D])(using VersionSelection): Unit = remove(interval)

  /**
    * Same as [[removeMany]]
    *
    * $removeManyDesc $mutableAction
    *
    * @param intervals
    *   $removeManyParamIntervals
    */
  infix def --(intervals: IterableOnce[Interval[D]])(using VersionSelection): Unit =
    removeMany(intervals)

// These may be problematic/misunderstood in the versioned space, so leaving them out for now.
//  def replace(oldData: ValidData[V, D], newData: ValidData[V, D]): Unit
//  def replaceByKey(key: D, newData: ValidData[V, D]): Unit
