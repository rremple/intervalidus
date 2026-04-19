package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * Constructs monoid data in multidimensional intervals.
  */
object DataMonoid extends DimensionalMonoidBaseObject[DataMonoid]:
  type In1D[V, R1] = DataMonoid[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataMonoid[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataMonoid[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataMonoid[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V: Monoid, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using config: CoreConfig[D]): DataMonoid[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new DataMonoid(byStartAsc, byValue, inSearchTree)

/**
  * Immutable dimensional data where values can be combined as monoids.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataMonoid[V, D <: NonEmptyTuple: DomainLike] private (
  override val dataByStart: mutable.TreeMap[D, ValidData[V, D]],
  override val dataByValue: MultiMapSorted[V, ValidData[V, D]],
  override val dataInBoxTree: BoxTree[ValidData[V, D]]
)(using val config: CoreConfig[D], monoid: Monoid[V])
  extends ImmutableBase[V, D, DataMonoid[V, D]]
  with DimensionalMonoidBase[V, D]:

  // ---------- Algebra API methods: Set Algebra ----------
  // ---- Standard set-theoretic operations for multidimensional shapes. ----

  /**
    * $unionDesc
    *
    * @param that
    *   $unionParmThat
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def union(that: DimensionalBase[V, D]): DataMonoid[V, D] = merge(that, monoid.combine)

  /**
    * Same as [[union]].
    *
    * $unionDesc
    *
    * @param that
    *   $unionParmThat
    * @return
    *   a new shape that is the union of this and that.
    */
  infix def ∪(that: DimensionalBase[V, D]): DataMonoid[V, D] = union(that)

  /**
    * $intersectionDesc
    *
    * @param that
    *   $intersectionParamThat
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def intersection(that: DimensionalBase[V, D]): DataMonoid[V, D] =
    DataMonoid(zipData(that, monoid.combine))

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc
    *
    * @param that
    *   $intersectionParamThat
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def ∩(that: DimensionalBase[V, D]): DataMonoid[V, D] = intersection(that)

  /**
    * $complementDesc
    *
    * @return
    *   a new shape that is the complement of this.
    */
  def complement: DataMonoid[V, D] = DataMonoid(domainComplement.map(_ -> monoid.identity))

  /**
    * Same as [[complement]].
    *
    * $complementDesc
    *
    * @return
    *   a new shape that is the complement of this.
    */
  def c: DataMonoid[V, D] = complement

  // ---------- Implement methods from ImmutableBase that create new instances ----------
  // ----  (some return Data rather than DataMonoid because the resultant value type isn't necessarily a Monoid) ----

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = Data(
    getAll.map(f)
  )(using config = altConfig).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = Data(
    getAll.flatMap(f(_).getAll)
  )(using config = altConfig).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = Data(
    getAll.collect(pf)
  )(using config = altConfig).compressAll()

  override def mapValues[B](
    f: V => B
  ): Data[B, D] =
    map(d => d.copy(value = f(d.value)))

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): DataMonoid[V, S] = DataMonoid(
    getAll.map(d => d.copy(interval = f(d.interval)))
  )(using config = altConfig).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataMonoid because the resultant value type isn't necessarily a Monoid) ----

  override def copy: DataMonoid[V, D] =
    new DataMonoid(dataByStart.clone(), dataByValue.clone(), dataInBoxTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] =
    Data(zipData(that, (_, _)))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): Data[(V, B), D] =
    Data(zipAllData(that, thisDefault, thatDefault, (_, _)))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using altConfig: CoreConfig[Domain.NonEmptyTail[D]]): DataMonoid[V, Domain.NonEmptyTail[D]] =
    DataMonoid(getByHeadDimensionData(domain))(using config = altConfig).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DataMonoid[V, R] =
    DataMonoid(getByDimensionData(dimensionIndex, domain))(using config = altConfig).compressAll()

  override def toMutable: intervalidus.mutable.DataMonoid[V, D] =
    intervalidus.mutable.DataMonoid(getAll)

  override def toImmutable: intervalidus.immutable.DataMonoid[V, D] =
    this
