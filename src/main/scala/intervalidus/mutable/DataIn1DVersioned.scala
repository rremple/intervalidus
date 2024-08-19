package intervalidus.mutable

import intervalidus.*
import intervalidus.DataIn1DBase.ValidData1D
import intervalidus.DataIn2DBase.{DiffAction2D, ValidData2D}
import intervalidus.immutable.DataIn1DVersioned as DataIn1DVersionedImmutable

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

object DataIn1DVersioned:
  /**
    * Shorthand constructor for a single initial value that is valid in a specific discrete interval starting at the
    * initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value
    * @param value
    *   value to start with
    * @param interval
    *   interval in which the value is valid
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataIn1DVersioned structure with a single valid value
    */
  def of[V, R: DiscreteValue](
    value: V,
    interval: DiscreteInterval1D[R],
    initialVersion: Int
  ): DataIn1DVersioned[V, R] = from(
    Iterable(ValidData1D(value, DiscreteInterval1D.unbounded)),
    initialVersion
  )

  /**
    * Shorthand constructor for a single initial value that is valid in the full discrete interval starting at the
    * initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value
    * @param value
    *   value to start with
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataIn1DVersioned structure with a single valid value
    */
  def of[V, R: DiscreteValue](
    value: V,
    initialVersion: Int = 0
  ): DataIn1DVersioned[V, R] = of(value, DiscreteInterval1D.unbounded, initialVersion)

  /**
    * Shorthand constructor for a collection of initial valid values starting at the initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @tparam R
    *   the type of discrete value used in the discrete interval assigned to each value
    * @param initialData
    *   valid data to start with
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataIn1DVersioned structure with the provided initial values
    */
  def from[V, R: DiscreteValue](
    initialData: Iterable[ValidData1D[V, R]] = Iterable.empty[ValidData1D[V, R]],
    initialVersion: Int = 0 // could use summon[DiscreteValue[Int]].minValue to extend range
  ): DataIn1DVersioned[V, R] = DataIn1DVersioned[V, R](
    initialData.map(d => ValidData2D(d.value, d.interval x DiscreteInterval1D.intervalFrom(initialVersion))),
    initialVersion
  )

/**
  * Interface is similar to [[DataIn1D]], but it operates on an underlying [[DataIn2D]] using an integer-valued
  * interval2 to version data. One use case (as defined in [[DataTimeboundVersioned]]) would be that R = LocalDate, so
  * data values may vary in terms of both version and date. Most methods require some generic version selection criteria
  * rather than specific integer intervals.
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too. Note that updates starting with
  * "current" will update unapproved changes as well (since intervalFrom goes to the Top)
  *
  * @tparam V
  *   the type of the value managed as data
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   note that two dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
class DataIn1DVersioned[V, R: DiscreteValue](
  initialData: Iterable[ValidData2D[V, R, Int]] = Iterable.empty[ValidData2D[V, R, Int]],
  initialVersion: Int = 0, // could use summon[DiscreteValue[Int]].minValue to extend range
  withCurrentVersion: Option[DiscreteDomain1D[Int]] = None
) extends DataIn1DVersionedBase[V, R](initialData, initialVersion, withCurrentVersion):

  import DataIn1DVersionedBase.VersionSelection

  /**
    * Returns this as a immutable structure.
    */
  def toImmutable: DataIn1DVersionedImmutable[V, R] = DataIn1DVersionedImmutable(
    underlying2D.getAll,
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Applies a sequence of 2D diff actions to this structure. Does not use a version selection context -- operates on
    * full underlying 2D structure.
    *
    * @param diffActions
    *   actions to be applied.
    */
  def applyDiffActions(diffActions: Iterable[DiffAction2D[V, R, Int]]): Unit =
    underlying2D.applyDiffActions(diffActions)

  /**
    * Synchronizes this with another structure by getting and applying the applicable diff actions. Does not use a
    * version selection context -- operates on full underlying 2D structure.
    *
    * @param that
    *   the structure with which this will be synchronized.
    */
  def syncWith(that: DataIn1DVersioned[V, R]): Unit = applyDiffActions(that.diffActionsFrom(this))

  /**
    * Creates a copy.
    *
    * @return
    *   a new structure with the same data and current version.
    */
  def copy: DataIn1DVersioned[V, R] = DataIn1DVersioned(
    underlying2D.getAll,
    initialVersion,
    Some(currentVersion)
  )

  /**
    * Updates structure to only include elements which satisfy a predicate. Data are mutated in place.
    *
    * Does not use a version selection context -- the predicate is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param p
    *   the predicate used to test elements.
    */
  def filter(p: ValidData2D[V, R, Int] => Boolean): Unit = underlying2D.filter(p)

  /**
    * Applies a function to all valid data. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def map(f: ValidData2D[V, R, Int] => ValidData2D[V, R, Int]): Unit = underlying2D.map(f)

  /**
    * Applies a function to all valid data values. Data are mutated in place.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it maps all
    * values in all versions. To only map values meeting specific version criteria, use [[map]] instead
    *
    * @param f
    *   the function to apply to the value part of each valid data element.
    */
  def mapValues(f: V => V): Unit = underlying2D.mapValues(f)

  /**
    * Applies a function to all the elements of this structure and updates valid values from the elements of the
    * resulting structures.
    *
    * Does not use a version selection context -- the function is applied to the underlying 2D data, so it can operate
    * on the underlying version information as well as the valid interval/value.
    *
    * @param f
    *   the function to apply to each valid data element which results in a new structure.
    * @throws IllegalArgumentException
    *   if the mapping function results in invalid data (e.g., introduces overlaps).
    */
  def flatMap(f: ValidData2D[V, R, Int] => DataIn1DVersioned[V, R]): Unit =
    underlying2D.flatMap(d => DataIn2D(f(d).underlying2D.getAll))

  override def zip[B](that: DataIn1DVersionedBase[B, R]): DataIn1DVersioned[(V, B), R] =
    DataIn1DVersioned(
      underlying2D.zip(that.getDataIn2D).getAll,
      initialVersion,
      Some(currentVersion)
    )

  override def zipAll[B](that: DataIn1DVersionedBase[B, R], thisElem: V, thatElem: B): DataIn1DVersioned[(V, B), R] =
    DataIn1DVersioned(
      underlying2D.zipAll(that.getDataIn2D, thisElem, thatElem).getAll,
      initialVersion,
      Some(currentVersion)
    )

  /**
    * Set new valid data. Note that any data previously valid in this interval and the given version selection context
    * are replaced by this data.
    *
    * @param data
    *   the valid data to set.
    */
  def set(data: ValidData1D[V, R])(using VersionSelection): Unit = underlying2D.set(validDataIn2D(data))

  /**
    * Set new valid data. Note that any value previously valid in this interval and the given version selection context
    * are replace by this value.
    *
    * @param value
    *   the valid data value to set.
    * @param interval
    *   the valid data interval to set.
    * @param versionSelection
    *   version selection context for new/replaced data.
    */
  def set(value: V, interval: DiscreteInterval1D[R])(using versionSelection: VersionSelection): Unit =
    underlying2D.set(
      value,
      interval x versionSelection.intervalFrom
    )

  /**
    * Set new valid data, but only if there are no previously valid values in its interval and given the version
    * selection context.
    *
    * @param newData
    *   the valid data to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(newData: ValidData1D[V, R])(using VersionSelection): Boolean =
    underlying2D.setIfNoConflict(validDataIn2D(newData))

  /**
    * Set new valid value, but only if there are no previously valid values in the interval and given the version
    * selection context.
    *
    * @param value
    *   the valid data value to set.
    * @param interval
    *   the valid data interval to set.
    * @return
    *   true if there were no conflicts and new data was set, false otherwise.
    */
  def setIfNoConflict(value: V, interval: DiscreteInterval1D[R])(using VersionSelection): Boolean =
    setIfNoConflict(ValidData1D(value, interval))

  /**
    * Remove valid values on the interval and the given version selection context. If there are values valid on portions
    * of the interval, those values have their intervals adjusted (e.g., shortened, shifted, split) accordingly.
    *
    * @param interval
    *   the interval where any valid values are removed.
    * @param versionSelection
    *   version used for removing data -- default is the current version.
    */
  def remove(interval: DiscreteInterval1D[R])(using versionSelection: VersionSelection): Unit =
    underlying2D.remove(
      interval x versionSelection.intervalFrom
    )

  /**
    * Update everything valid in the specified interval and the given version selection context to have the specified
    * value. Note that no new intervals of validity are added as part of this operation. Data with overlaps are adjusted
    * accordingly.
    *
    * @param value
    *   the new value existing data should take on.
    * @param interval
    *   the interval in which the update should be applied.
    * @param versionSelection
    *   version used for updating data -- default is the current version.
    */
  def update(value: V, interval: DiscreteInterval1D[R])(using versionSelection: VersionSelection): Unit =
    underlying2D.update(
      value,
      interval x versionSelection.intervalFrom
    )

  /**
    * Compress out adjacent intervals with the same value. Does not use a version selection context -- operates on full
    * underlying 2D structure.
    *
    * @param value
    *   value for which valid data will be compressed.
    */
  def compress(value: V): Unit = underlying2D.compress(value)

  /**
    * Compress out adjacent intervals with the same value for all values. Does not use a version selection context -- *
    * operates on full underlying 2D structure. (Shouldn't ever need to do this.)
    */
  def compressAll(): Unit = underlying2D.compressAll()

  // --- API methods unique to this "versioned" variant

  /**
    * Sets the current version. Note that no version history is rewritten, which may cause some unexpected results. Use
    * with caution.
    *
    * @param version
    *   the new current version
    */
  def setCurrentVersion(version: DiscreteDomain1D[Int]): Unit = synchronized:
    if version >= unapprovedStartVersion then throw Exception("version too large")
    else if version equiv DiscreteDomain1D.Bottom then throw Exception("version too small")
    else currentVersion = version

  /**
    * Increments the current version.
    */
  def incrementCurrentVersion(): Unit = synchronized:
    if currentVersion.successor equiv unapprovedStartVersion then throw Exception("wow, ran out of versions!")
    else currentVersion = currentVersion.successor

  /**
    * Eliminate all version information after the specified version (including any unapproved changes).
    *
    * @param version
    *   the version after which all version information is removed.
    */
  def resetToVersion(version: DiscreteDomain1D[Int]): Unit =
    val keep = VersionSelection(version)
    filter(_.interval.vertical intersects keep.intervalTo)
    map(d =>
      if d.interval.vertical.end >= keep.boundary
      then ValidData2D(d.value, d.interval.withVertical(d.interval.vertical.toTop))
      else d
    )
    setCurrentVersion(version)

  /**
    * Creates a new structure based on the version selection context, but without any version history.
    */
  def collapseVersionHistory(using versionSelection: VersionSelection): Unit = synchronized:
    filter(_.interval.vertical contains versionSelection.boundary)
    map(d => ValidData2D(d.value, d.interval.withVertical(DiscreteInterval1D.intervalFrom(initialVersion))))
    compressAll()
    setCurrentVersion(initialVersion)

  /**
    * Special case where we change the version interval in a constrained way.
    *
    * @param data
    *   currently unapproved data to approved
    * @return
    *   true if unapproved version was found and approved, false otherwise
    */
  def approve(data: ValidData1D[V, R]): Boolean =
    val allUnapproved = underlying2D
      .getIntersecting(data.interval x VersionSelection.Unapproved.intervalFrom)
      .filter(_.interval.vertical.start equiv unapprovedStartVersion) // only unapproved
    allUnapproved.headOption match
      case Some(d) if data.value == d.value && d.interval.horizontal == data.interval =>
        set(data)(using VersionSelection.Current)
        true
      case other =>
        false

  /**
    * Useful when approving everything in a range, including empty space (i.e., an unapproved removal)
    *
    * @param interval
    *   interval in which all changes (upserts and deletes) are approved
    */
  def approveAll(interval: DiscreteInterval1D[R]): Unit = synchronized:
    underlying2D
      .getIntersecting(interval x VersionSelection.Unapproved.intervalFrom)
      .filter(_.interval.vertical.start equiv unapprovedStartVersion) // only unapproved
      .map(d => ValidData1D(d.value, d.interval.horizontal)) // as 1D
      .foreach(approve)
    underlying2D
      .getIntersecting(interval x VersionSelection.Current.intervalFrom)
      .filter(_.interval.vertical.end equiv unapprovedStartVersion.predecessor) // only related to unapproved removes
      .flatMap(_.interval.horizontal intersectionWith interval)
      .foreach(remove(_)(using VersionSelection.Current))
