package intervalidus.immutable

import intervalidus.*
import intervalidus.DimensionalBase.{Transaction, State}

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
    new DataMonoid(State.from(initialData))

/**
  * Immutable dimensional data where values can be combined as monoids.
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataMonoid[V, D <: NonEmptyTuple: DomainLike] private (
  initialState: State[V, D]
)(using val config: CoreConfig[D], monoid: Monoid[V])
  extends ImmutableBase[V, D, DataMonoid[V, D]]
  with DimensionalMonoidBase[V, D]:

  @volatile override protected var state: State[V, D] = initialState

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
  infix def intersection(that: DimensionalBase[V, D]): DataMonoid[V, D] = transactionalReadWith(that): thatTx =>
    DataMonoid(zipData(that, thatTx, monoid.combine))

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
  def complement: DataMonoid[V, D] = transactionalRead:
    DataMonoid(domainComplementInternal.map(_ -> monoid.identity))

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
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    mapInternal(f)

  protected def mapInternal[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  )(using altConfig: CoreConfig[S])(using Transaction[V, D]): Data[B, S] =
    Data(
      getAllInternal.map(f)
    )(using config = altConfig).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.flatMap(f(_).getAll)
    )(using config = altConfig).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  )(using altConfig: CoreConfig[S]): Data[B, S] = transactionalRead:
    Data(
      getAllInternal.collect(pf)
    )(using config = altConfig).compressAll()

  override def mapValues[B](
    f: V => B
  ): Data[B, D] = transactionalRead:
    mapInternal(d => d.copy(value = f(d.value)))

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  )(using altConfig: CoreConfig[S]): DataMonoid[V, S] = transactionalRead:
    DataMonoid(
      getAllInternal.map(d => d.copy(interval = f(d.interval)))
    )(using config = altConfig).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataMonoid because the resultant value type isn't necessarily a Monoid) ----

  override def copy: DataMonoid[V, D] =
    new DataMonoid(state.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] = transactionalReadWith(that): thatTx =>
    Data(zipData(that, thatTx, (_, _)))

  override def zipAll[B](
    that: DimensionalBase[B, D],
    thisDefault: V,
    thatDefault: B
  ): Data[(V, B), D] = transactionalReadWith(that): thatTx =>
    Data(zipAllData(that, thatTx, thisDefault, thatDefault, (_, _)))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using
    altConfig: CoreConfig[Domain.NonEmptyTail[D]]
  ): DataMonoid[V, Domain.NonEmptyTail[D]] = transactionalRead:
    DataMonoid(getByHeadDimensionData(domain))(using config = altConfig).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DataMonoid[V, R] = transactionalRead:
    DataMonoid(getByDimensionData(dimensionIndex, domain))(using config = altConfig).compressAll()

  override def toMutable: intervalidus.mutable.DataMonoid[V, D] = transactionalRead:
    intervalidus.mutable.DataMonoid(getAllInternal)

  override def toImmutable: intervalidus.immutable.DataMonoid[V, D] =
    this
