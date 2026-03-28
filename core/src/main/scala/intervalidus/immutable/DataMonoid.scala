package intervalidus.immutable

import intervalidus.*
import intervalidus.Interval.unbounded
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * Constructs monoid data in multidimensional intervals.
  */
object DataMonoid extends DimensionalMonoidBaseObject:
  type In1D[V, R1] = DataMonoid[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = DataMonoid[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = DataMonoid[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = DataMonoid[V, Domain.In4D[R1, R2, R3, R4]]

  override def empty[V: Monoid, D <: NonEmptyTuple: DomainLike]: DataMonoid[V, D] = apply()

  override def ∅[V: Monoid, D <: NonEmptyTuple: DomainLike]: DataMonoid[V, D] = empty

  override def universe[V, D <: NonEmptyTuple: DomainLike](using m: Monoid[V]): DataMonoid[V, D] = of(m.identity)

  override def ξ[V: Monoid, D <: NonEmptyTuple: DomainLike]: DataMonoid[V, D] = universe

  override def of[V: Monoid, D <: NonEmptyTuple: DomainLike](data: ValidData[V, D]): DataMonoid[V, D] =
    apply(Iterable.single(data))

  override def of[V: Monoid, D <: NonEmptyTuple: DomainLike](value: V): DataMonoid[V, D] = of(unbounded[D] -> value)

  override def apply[V: Monoid, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using Experimental): DataMonoid[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new DataMonoid(byStartAsc, byValue, inSearchTree)

  override def newBuilder[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], DataMonoid[V, D]] = ValidData.Builds[V, D, DataMonoid[V, D]](apply(_))

/**
  * Immutable dimensional data where values can be combined as monoids.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class DataMonoid[V, D <: NonEmptyTuple: DomainLike] protected (
  override val dataByStartAsc: mutable.TreeMap[D, ValidData[V, D]],
  override val dataByValue: MultiMapSorted[V, ValidData[V, D]],
  override val dataInSearchTree: BoxTree[ValidData[V, D]]
)(using monoid: Monoid[V])(using Experimental)
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
  infix def union(that: DimensionalMonoidBase[V, D]): DataMonoid[V, D] = merge(that, monoid.combine)

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
  infix def ∪(that: DimensionalMonoidBase[V, D]): DataMonoid[V, D] = union(that)

  /**
    * $intersectionDesc
    *
    * @param that
    *   $intersectionParamThat
    * @return
    *   a new shape that is the intersection of this and that.
    */
  infix def intersection(that: DimensionalMonoidBase[V, D]): DataMonoid[V, D] =
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
  infix def ∩(that: DimensionalMonoidBase[V, D]): DataMonoid[V, D] = intersection(that)

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

  override infix def intersection(interval: Interval[D]): DataMonoid[V, D] =
    DataMonoid(intersectionData(interval))

  override infix def symmetricDifference(that: DimensionalBase[V, D]): DataMonoid[V, D] =
    DataMonoid(symmetricDifferenceData(that))

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  ): Data[B, S] = Data(
    getAll.map(f)
  ).compressAll()

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  ): Data[B, S] = Data(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  override def collect[B, S <: NonEmptyTuple: DomainLike](
    pf: PartialFunction[ValidData[V, D], ValidData[B, S]]
  ): Data[B, S] = Data(
    getAll.collect(pf)
  ).compressAll()

  override def mapValues[B](
    f: V => B
  ): Data[B, D] =
    map(d => d.copy(value = f(d.value)))

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): DataMonoid[V, S] = DataMonoid(
    getAll.map(d => d.copy(interval = f(d.interval)))
  ).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------
  // ----  (some return Data rather than DataMonoid because the resultant value type isn't necessarily a Monoid) ----

  override def copy: DataMonoid[V, D] =
    new DataMonoid(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): Data[(V, B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): DataMonoid[V, Domain.NonEmptyTail[D]] =
    DataMonoid(getByHeadDimensionData(domain)).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): DataMonoid[V, R] =
    DataMonoid(getByDimensionData(dimensionIndex, domain)).compressAll()

  override def toMutable: intervalidus.mutable.DataMonoid[V, D] =
    intervalidus.mutable.DataMonoid(getAll)

  override def toImmutable: intervalidus.immutable.DataMonoid[V, D] =
    this
