package intervalidus.mutable

import intervalidus.*
import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.Tuple.{Concat, Drop, Elem, Head, Tail, Take}
import scala.collection.mutable
import scala.compiletime.ops.int.S

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
  ): mutable.Builder[ValidData[V, D], Data[V, D]] = DimensionalDataBuilder[V, D, Data[V, D]](apply(_))

/**
  * Mutable dimensional data.
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
  extends MutableBase[V, D]:

  experimental.control("requireDisjoint")(
    nonExperimentalResult = (),
    experimentalResult = require(Interval.isDisjoint(getAll.map(_.interval)), "data must be disjoint")
  )

  // ---------- Implement methods from DimensionalBase that create new instances ----------

  override def copy: Data[V, D] =
    new Data(dataByStartAsc.clone(), dataByValue.clone(), dataInSearchTree.copy)

  override def zip[B](that: DimensionalBase[B, D]): Data[(V, B), D] =
    Data(zipData(that))

  override def zipAll[B](that: DimensionalBase[B, D], thisDefault: V, thatDefault: B): Data[(V, B), D] =
    Data(zipAllData(that, thisDefault, thatDefault))

  override def getByHeadDimension[H: DomainValueLike](domain: Domain1D[H])(using
    Head[D] =:= Domain1D[H],
    Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): Data[V, NonEmptyTail[D]] =
    val result = Data(getByHeadDimensionData(domain))
    result.compressAll()
    result

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int,
    domain: Domain1D[H]
  )(using
    Elem[D, dimensionIndex.type] =:= Domain1D[H],
    Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D,
    Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
  ): DimensionalBase[V, R] =
    val result = Data(getByDimensionData(dimensionIndex, domain))
    result.compressAll()
    result

  override def toMutable: intervalidus.mutable.Data[V, D] =
    this

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    intervalidus.immutable.Data(getAll)
