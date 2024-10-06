package intervalidus

import scala.language.implicitConversions

object DataIn1DVersionedBase:

  /**
    * Context parameter for selecting the version interval on which to operate in selection and mutation. Typically
    * "Current" is the right thing to use (and is provided as the default given) unless you are operating only on
    * potentially unapproved data or delving into version history by operating on a specific past version.
    */
  enum VersionSelection:
    case Current
    case Unapproved // potentially: when used in selection, returns unapproved plus approved not occluded by unapproved
    case Specific(version: DiscreteDomain1D[Int])

  /**
    * Default version selection context when not overridden locally or specified explicitly
    */
  given VersionSelection = VersionSelection.Current

  object VersionSelection:
    def apply(version: DiscreteDomain1D[Int]): VersionSelection = VersionSelection.Specific(version)

/**
  * Interface is similar to [[DataIn1DBase]], but it operates on an underlying [[mutable.DataIn2D]] using an
  * integer-valued vertical dimension to version data. One use case (as defined in DataTimeboundVersioned) would be that
  * R = LocalDate, so data values may vary in terms of both version and time. Most methods require some generic version
  * selection criteria rather than specific integer intervals, therefore this does not extend [[DimensionalBase]].
  *
  * The "current" version is managed as state (a var). Versioning also separates notions of approved vs. unapproved data
  * (unapproved data are pushed up to start at version maxValue).
  *
  * When getting data, by default, we return "current" version data (a.k.a., approved). When updating data, by default,
  * we don't rewrite history, so mutations start with the "current" version too. Note that updates starting with
  * "current" will update unapproved changes as well (since intervalFrom goes to the Top).
  *
  * @tparam V
  *   the type of the value managed as data
  * @tparam R
  *   the type of discrete value used in the discrete interval assigned to each value
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version) to start with --
  *   note that two dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero. (Could, for example, use `IntDiscreteValue.minValue` instead
  *   of zero to extend the version range.)
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  */
trait DataIn1DVersionedBase[V, R: DiscreteValue](
  initialData: Iterable[ValidData2D[V, R, Int]],
  val initialVersion: Int,
  withCurrentVersion: Option[DiscreteDomain1D[Int]]
)(using Experimental)
  extends PartialFunction[DiscreteDomain2D[R, Int], V]:

  import DataIn1DVersionedBase.VersionSelection

  extension (versionSelection: VersionSelection)
    /**
      * Returns the boundary of this version selection as a domain element
      */
    protected def boundary: DiscreteDomain1D[Int] = versionSelection match
      case VersionSelection.Current           => currentVersion
      case VersionSelection.Unapproved        => unapprovedStartVersion
      case VersionSelection.Specific(version) => version

    /**
      * Returns the interval to the boundary of this version selection
      */
    def intervalTo: DiscreteInterval1D[Int] = DiscreteInterval1D.intervalTo(boundary)

    /**
      * Returns the interval from the boundary of this version selection
      */
    def intervalFrom: DiscreteInterval1D[Int] = DiscreteInterval1D.intervalFrom(boundary)

  // --- definitions unique to this "versioned" variant

  // Underlying 2D representation of versioned 1D data (mutable)
  protected val underlying2D: mutable.DataIn2D[V, R, Int] = mutable.DataIn2D(initialData)

  // The current version, mutable via access methods only
  protected var currentVersion: DiscreteDomain1D[Int] =
    withCurrentVersion.getOrElse(DiscreteDomain1D.Point(initialVersion))

  /**
    * Returns the current version
    */
  def getCurrentVersion: DiscreteDomain1D[Int] = currentVersion

  // Special version at which we place (or delete) unapproved stuff (fixed)
  protected val unapprovedStartVersion: DiscreteDomain1D[Int] = summon[DiscreteValue[Int]].maxValue

  // Construct 2D data from 1D data plus version selection
  protected def validDataIn2D(
    data: ValidData1D[V, R]
  )(using versionSelection: VersionSelection): ValidData2D[V, R, Int] =
    (data.interval x versionSelection.intervalFrom) -> data.value

  /**
    * Based on the given version selection context, gets all the data as a 1D structure (uncompressed, mutable).
    *
    * Useful when you don't need the overhead of creating an immutable structure and compressing the data, e.g., if just
    * doing a `getAt` after version selection, which will give the same answer compressed or uncompressed, mutable or
    * immutable.
    *
    * @param versionSelection
    *   version selection context for this operation.
    * @return
    *   new, uncompressed, mutable 1D structure meeting version selection criteria.
    */
  def getDataIn1DMutable(using versionSelection: VersionSelection): mutable.DataIn1D[V, R] =
    underlying2D.getByVerticalIndex(versionSelection.boundary)

  /**
    * Based on the given version selection context, gets all the data as a 1D structure (compressed, immutable).
    *
    * @param versionSelection
    *   version selection context for this operation.
    * @return
    *   new, compressed, immutable 1D structure meeting version selection criteria.
    */
  def getDataIn1D(using versionSelection: VersionSelection): immutable.DataIn1D[V, R] =
    getDataIn1DMutable.toImmutable.compressAll()

  /**
    * Gets all the data in all versions as a 2D structure (immutable)
    *
    * @return
    *   new 2D structure.
    */
  def getDataIn2D: immutable.DataIn2D[V, R, Int] = underlying2D.toImmutable

  // --- API methods similar to those in DataIn1D/DataIn2D, often with version selection (can't use common super)

  /**
    * Returns this as a mutable structure.
    */
  def toMutable: DataIn1DVersionedBase[V, R]

  /**
    * Returns this as a immutable structure.
    */
  def toImmutable: DataIn1DVersionedBase[V, R]

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
  def zip[B](that: DataIn1DVersionedBase[B, R]): DataIn1DVersionedBase[(V, B), R]

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
  def zipAll[B](
    that: DataIn1DVersionedBase[B, R],
    thisElem: V,
    thatElem: B
  ): DataIn1DVersionedBase[(V, B), R]

  // ---------- Implement methods not like those in DimensionalBase ----------

  // from PartialFunction
  override def isDefinedAt(key: DiscreteDomain2D[R, Int]): Boolean = underlying2D.isDefinedAt(key)

  // from PartialFunction
  override def apply(key: DiscreteDomain2D[R, Int]): V = underlying2D(key)

  // from Object - print the current version and a uniform 2D grid representing the versioned data.
  override def toString: String = s"current version = $currentVersion\n$underlying2D"

  /**
    * Constructs a sequence of 2D diff actions that, if applied to the old structure, would synchronize it with this
    * one. Does not use a version selection context -- operates on full underlying 2D structure.
    *
    * @param old
    *   the old structure from which we are comparing.
    * @return
    *   a sequence of diff actions that would synchronize it with this.
    */
  def diffActionsFrom(old: DataIn1DVersionedBase[V, R]): Iterable[DiffAction2D[V, R, Int]] =
    underlying2D.diffActionsFrom(old.underlying2D)

  // ---------- Implement methods like those in DimensionalBase ----------

  /**
    * Returns true when there are no valid data in this structure, otherwise false.
    */
  def isEmpty: Boolean = underlying2D.isEmpty

  /**
    * Applies a binary operator to a start value and all valid data, going left to right. Does not use a version
    * selection context -- the function is applied to the underlying 2D data, so it can operate on the underlying
    * version information as well as the valid interval/value.
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
  def foldLeft[B](z: B)(op: (B, ValidData2D[V, R, Int]) => B): B = underlying2D.foldLeft(z)(op)

  /**
    * Returns a the value if there is a single, unbounded valid value given some version selection context. Otherwise
    * throws an exception.
    *
    * @return
    *   a single valid value.
    * @throws NoSuchElementException
    *   if there isn't any valid data, or valid data are bounded (i.e., take on different values in different
    *   intervals), given the version selection provided.
    */
  def get(using VersionSelection): V = getDataIn1D.get

  /**
    * Returns a Some value if there is a single, unbounded valid value given some version selection context. Otherwise
    * returns None.
    */
  def getOption(using VersionSelection): Option[V] = getDataIn1D.getOption

  /**
    * Get all valid data given some version selection context.
    *
    * @return
    *   all valid data given some version selection context in interval.start order
    */
  def getAll(using VersionSelection): Iterable[ValidData1D[V, R]] = getDataIn1D.getAll

  /**
    * Returns a value that is valid in the specified interval domain element given some version selection context. That
    * is, where the specified domain element is a member of some valid data interval. If no such valid data exists,
    * returns None.
    *
    * @param r
    *   the domain element where data may be valid. Note that the domain element can be a specific data point or the
    *   special notions of "bottom" or "top" of the domain.
    * @return
    *   Some value if valid at the specified domain element, otherwise None.
    */
  def getAt(r: DiscreteDomain1D[R])(using VersionSelection): Option[V] = getDataIn1DMutable.getAt(r)

  /**
    * Is a value defined at a part of the domain given some version selection context.
    *
    * @param key
    *   the domain element to test.
    * @return
    *   true if data is valid in the specified domain, false otherwise.
    */
  infix def isValidAt(key: DiscreteDomain1D[R])(using VersionSelection): Boolean = getDataIn1DMutable.isDefinedAt(key)

  /**
    * Returns all data that are valid on some or all of the provided interval given some version selection context.
    *
    * @param interval
    *   the interval to check.
    * @return
    *   all data that are valid on some or all of the interval (some intersection).
    */
  def getIntersecting(interval: DiscreteInterval1D[R])(using VersionSelection): Iterable[ValidData1D[V, R]] =
    getDataIn1D.getIntersecting(interval)

  /**
    * Are there values that are valid on some or all of the provided interval given some version selection context?
    *
    * @param interval
    *   the interval to check
    * @return
    *   true if there are values that are valid somewhere on the interval.
    */
  infix def intersects(interval: DiscreteInterval1D[R])(using VersionSelection): Boolean =
    getDataIn1DMutable intersects interval

  /**
    * Returns all the intervals (compressed) in which there are valid values given some version selection context.
    */
  def domain(using VersionSelection): Iterable[DiscreteInterval1D[R]] = getDataIn1DMutable.domain
