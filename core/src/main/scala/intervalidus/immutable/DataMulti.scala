package intervalidus.immutable

import intervalidus.*
import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * Constructs multi-data in multidimensional intervals.
  */
object DataMulti extends DimensionalMultiBaseObject:
  type In1D[V, R1] = DataMulti[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataMulti[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataMulti[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataMulti[V, Domain.In4D[R1, R2, R3, R4]]

  override def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): DataMulti[V, D] = DataMulti(Iterable(data.interval -> Set(data.value)))

  override def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): DataMulti[V, D] = of(Interval.unbounded[D] -> value)

  override def from[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]]
  )(using Experimental): DataMulti[V, D] =
    DataMulti[V, D]().addOneMany(initialData)

  override def from[V, D <: NonEmptyTuple: DomainLike](
    that: DimensionalBase[Set[V], D]
  )(using Experimental): DataMulti[V, D] = apply(that.getAll)

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[Set[V], D]] = Iterable.empty[ValidData[Set[V], D]]
  )(using Experimental): DataMulti[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new DataMulti(byStartAsc, byValue, inSearchTree)

  override def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], DataMulti[V, D]] = DimensionalDataMultiBuilder[V, D, DataMulti[V, D]](from(_))

/**
  * Immutable, multivalued dimensional data.
  *
  * Data may have multiple values valid in different intervals of arbitrary dimensions, conceptually similar to a
  * multimap. For example, one may want to represent when the data are valid in time and over certain versions, or in
  * two dimensions of time, simultaneously. When queried, values are returned as a set. The standard mutation methods
  * operate on these sets of values. There are also add and remove methods allow mutation of individual values across
  * intervals, and a concat method for combining two structures (a merge where overlaps are concatenated).
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type -- [[DomainLike]] non-empty tuples.
  */
class DataMulti[V, D <: NonEmptyTuple: DomainLike] protected (
  override val dataByStartAsc: mutable.TreeMap[D, ValidData[Set[V], D]],
  override val dataByValue: MultiMapSorted[Set[V], ValidData[Set[V], D]],
  override val dataInSearchTree: BoxTree[ValidData[Set[V], D]]
)(using experimental: Experimental)
  extends ImmutableBase[Set[V], D, DataMulti[V, D]]
  with DimensionalMultiBase[V, D]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Specific multivalue methods that have immutable signatures ----------

  /**
    * Concatenates all valid data in this and that structure into a new one.
    *
    * @param that
    *   the structure which is going to be concatenated.
    * @return
    *   a new, updated structure.
    */
  def mergeOne(that: DataMulti[V, D]): DataMulti[V, D] =
    merge(that, _ ++ _)

  /**
    * Update everything valid in data's interval to have the data's value. New intervals of validity are added where no
    * data in the interval are valid. Data with overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to add
    * @return
    *   a new, updated structure
    */
  def addOne(data: ValidData[V, D]): DataMulti[V, D] =
    copyAndModify(_.addOneInPlace(data))

  /**
    * Remove valid values on the interval. Intervals of validity are removed where only this value is valid. Data with
    * overlaps are adjusted accordingly.
    *
    * @param data
    *   the data to remove
    * @return
    *   a new, updated structure
    */
  def removeOne(data: ValidData[V, D]): DataMulti[V, D] =
    copyAndModify(_.removeOneInPlace(data))

  /**
    * Add all the values following the logic in [[addOne]].
    *
    * @param allData
    *   the data to add
    * @return
    *   a new, updated structure
    */
  def addOneMany(allData: Iterable[ValidData[V, D]]): DataMulti[V, D] = copyAndModify: result =>
    allData.foreach(result.addOneInPlace)

  /**
    * Remove all the values following the logic in [[removeOne]].
    *
    * @param allData
    *   the data to remove
    * @return
    *   a new, updated structure
    */
  def removeOneMany(allData: Iterable[ValidData[V, D]]): DataMulti[V, D] = copyAndModify: result =>
    allData.foreach(result.removeOneInPlace)

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  //   (these return Data rather than DataMulti because B isn't necessarily a Set type)

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => ValidData[B, S]
  ): Data[B, S] = Data(
    getAll.map(f)
  ).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[Set[V], D], ValidData[B, S]]
  ): Data[B, S] = Data(
    getAll.collect(pf)
  ).compressAll()

  override def mapValues[B](
    f: Set[V] => B
  ): Data[B, D] = Data(
    getAll.map(d => d.copy(value = f(d.value)))
  ).compressAll()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): DataMulti[V, S] = DataMulti(
    getAll.map(d => d.copy(interval = f(d.interval)))
  ).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[Set[V], D] => DimensionalBase[B, S]
  ): Data[B, S] = Data(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: DataMulti[V, D] =
    new DataMulti(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(Set[V], B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: Set[V], thatDefault: B): Data[(Set[V], B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadIndex[H: DomainValueLike](headIndex: Domain1D[H])(using
    Tuple.Head[D] =:= Domain1D[H],
    Tuple.Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tuple.Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): DataMulti[V, NonEmptyTail[D]] =
    DataMulti(getByHeadIndexData(headIndex)).compressAll()

  override def toMutable: intervalidus.mutable.DataMulti[V, D] =
    intervalidus.mutable.DataMulti(getAll)

  override def toImmutable: intervalidus.immutable.DataMulti[V, D] =
    this
