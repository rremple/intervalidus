package intervalidus.mutable

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
  * @param config
  *   $configParam
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
  extends MutableBase[V, D]
  with DimensionalMonoidBase[V, D]:

  // ---------- Algebra API methods: Set Algebra ----------
  // ---- Standard set-theoretic operations for multidimensional shapes. ----

  /**
    * $unionDesc $mutableAction
    *
    * @param that
    *   $unionParmThat
    */
  infix def union(that: DimensionalMonoidBase[V, D]): Unit = merge(that, monoid.combine)

  /**
    * Same as [[union]].
    *
    * $unionDesc $mutableAction
    *
    * @param that
    *   $unionParmThat
    */
  infix def ∪(that: DimensionalMonoidBase[V, D]): Unit = union(that)

  /**
    * $intersectionDesc $mutableAction
    *
    * @param that
    *   $intersectionParamThat
    */
  infix def intersection(that: DimensionalMonoidBase[V, D]): Unit = synchronized:
    replaceValidData(zipData(that, monoid.combine))

  /**
    * Same as [[intersection]].
    *
    * $intersectionDesc $mutableAction
    *
    * @param that
    *   $intersectionParamThat
    */
  infix def ∩(that: DimensionalMonoidBase[V, D]): Unit = intersection(that)

  /**
    * $complementDesc $mutableAction
    */
  def complement(): Unit = synchronized:
    replaceValidData(domainComplement.map(_ -> monoid.identity))

  /**
    * Same as [[complement]].
    *
    * $complementDesc $mutableAction
    */
  def c(): Unit = complement()

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
    val result = DataMonoid(getByHeadDimensionData(domain))(using config = altConfig)
    result.compressAll()
    result

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  )(using altConfig: CoreConfig[R]): DataMonoid[V, R] =
    val result = DataMonoid(getByDimensionData(dimensionIndex, domain))(using config = altConfig)
    result.compressAll()
    result

  override def toMutable: intervalidus.mutable.DataMonoid[V, D] =
    this

  override def toImmutable: intervalidus.immutable.DataMonoid[V, D] =
    intervalidus.immutable.DataMonoid(getAll)
