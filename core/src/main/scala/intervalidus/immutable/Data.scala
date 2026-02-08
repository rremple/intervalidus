package intervalidus.immutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * $objectDesc
  */
object Data extends DimensionalBaseObject with DimensionalBaseConstructorParams:

  type In1D[V, R1] = Data[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = Data[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = Data[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = Data[V, Domain.In4D[R1, R2, R3, R4]]

  override def of[V, D <: NonEmptyTuple: DomainLike](
    data: ValidData[V, D]
  )(using Experimental): Data[V, D] = Data(Iterable(data))

  override def of[V, D <: NonEmptyTuple: DomainLike](
    value: V
  )(using Experimental): Data[V, D] = of(Interval.unbounded[D] -> value)

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using Experimental): Data[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new Data(byStartAsc, byValue, inSearchTree)

  override def newBuilder[V, D <: NonEmptyTuple: DomainLike](using
    Experimental
  ): mutable.Builder[ValidData[V, D], Data[V, D]] = ValidData.Builds[V, D, Data[V, D]](apply(_))

/**
  * Immutable dimensional data.
  *
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class Data[V, D <: NonEmptyTuple: DomainLike] protected (
  override val dataByStartAsc: mutable.TreeMap[D, ValidData[V, D]],
  override val dataByValue: MultiMapSorted[V, ValidData[V, D]],
  override val dataInSearchTree: BoxTree[ValidData[V, D]]
)(using experimental: Experimental)
  extends ImmutableBase[V, D, Data[V, D]]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Implement methods from ImmutableBase that create new instances ----------

  override def map[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => ValidData[B, S]
  ): Data[B, S] = Data(
    getAll.map(f)
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
  ): Data[V, S] =
    map(d => d.copy(interval = f(d.interval)))

  override def flatMap[B, S <: NonEmptyTuple: DomainLike](
    f: ValidData[V, D] => DimensionalBase[B, S]
  ): Data[B, S] = Data(
    getAll.flatMap(f(_).getAll)
  ).compressAll()

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: Data[V, D] =
    new Data(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): Data[(V, B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  ): Data[V, Domain.NonEmptyTail[D]] =
    Data(getByHeadDimensionData(domain)).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Domain.DimensionIndex,
    domain: Domain1D[H]
  )(using
    Domain.HasIndex[D, dimensionIndex.type],
    Domain.IsAtIndex[D, dimensionIndex.type, H],
    Domain.IsUpdatableAtIndex[D, dimensionIndex.type, H],
    Domain.IsDroppedInResult[D, dimensionIndex.type, R]
  ): Data[V, R] =
    Data(getByDimensionData(dimensionIndex, domain)).compressAll()

  override def toMutable: intervalidus.mutable.Data[V, D] =
    intervalidus.mutable.Data(getAll)

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    this
