package intervalidus.mutable

import intervalidus.*
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

/**
  * $objectDesc
  */
object Data extends DimensionalBaseObject[Data] with DimensionalBaseConstructorParams:

  type In1D[V, R1] = Data[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = Data[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = Data[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = Data[V, Domain.In4D[R1, R2, R3, R4]]

  override def apply[V, D <: NonEmptyTuple: DomainLike](
    initialData: Iterable[ValidData[V, D]] = Iterable.empty[ValidData[V, D]]
  )(using config: CoreConfig[D]): Data[V, D] =
    val (byStartAsc, byValue, inSearchTree) = constructorParams(initialData)
    new Data(byStartAsc, byValue, inSearchTree)

/**
  * Mutable dimensional data.
  *
  * @param config
  *   $configParam
  * @tparam V
  *   $dataValueType
  * @tparam D
  *   $intervalDomainType
  */
class Data[V, D <: NonEmptyTuple: DomainLike] private (
  override val dataByStart: mutable.TreeMap[D, ValidData[V, D]],
  override val dataByValue: MultiMapSorted[V, ValidData[V, D]],
  override val dataInBoxTree: BoxTree[ValidData[V, D]]
)(using val config: CoreConfig[D])
  extends MutableBase[V, D]:

  config.experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: Data[V, D] =
    new Data(dataByStart.clone(), dataByValue.clone(), dataInBoxTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] =
    Data(zipData(that, (_, _)))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): Data[(V, B), D] =
    Data(zipAllData(that, thisDefault, thatDefault, (_, _)))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Domain.IsAtLeastTwoDimensional[D],
    Domain.IsAtHead[D, H],
    Domain.IsUpdatableAtHead[D, H],
    DomainLike[Domain.NonEmptyTail[D]]
  )(using altConfig: CoreConfig[Domain.NonEmptyTail[D]]): Data[V, Domain.NonEmptyTail[D]] =
    val result = Data(getByHeadDimensionData(domain))
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
  )(using altConfig: CoreConfig[R]): Data[V, R] =
    val result = Data(getByDimensionData(dimensionIndex, domain))
    result.compressAll()
    result

  override def toMutable: intervalidus.mutable.Data[V, D] =
    this

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    intervalidus.immutable.Data(getAll)
