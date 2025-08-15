package intervalidus.immutable

import intervalidus.*
import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.Tuple.{Concat, Drop, Elem, Head, Tail, Take}
import scala.collection.mutable
import scala.compiletime.ops.int.S

/**
  * Constructs data in multidimensional intervals.
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
  * Immutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type -- [[DomainLike]] non-empty tuples.
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
  ): Data[B, D] = Data(
    getAll.map(d => d.copy(value = f(d.value)))
  ).compressAll()

  override def mapIntervals[S <: NonEmptyTuple: DomainLike](
    f: Interval[D] => Interval[S]
  ): Data[V, S] = Data(
    getAll.map(d => d.copy(interval = f(d.interval)))
  ).compressAll()

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
    Head[D] =:= Domain1D[H],
    Tail[D] =:= NonEmptyTail[D],
    Domain1D[H] *: Tail[D] =:= D,
    DomainLike[NonEmptyTail[D]]
  ): Data[V, NonEmptyTail[D]] =
    Data(getByHeadDimensionData(domain)).compressAll()

  override def getByDimension[H: DomainValueLike, R <: NonEmptyTuple: DomainLike](
    dimensionIndex: Int,
    domain: Domain1D[H]
  )(using
    Elem[D, dimensionIndex.type] =:= Domain1D[H],
    Concat[Take[D, dimensionIndex.type], Domain1D[H] *: Drop[D, S[dimensionIndex.type]]] =:= D,
    Concat[Take[D, dimensionIndex.type], Drop[D, S[dimensionIndex.type]]] =:= R
  ): Data[V, R] =
    Data(getByDimensionData(dimensionIndex, domain)).compressAll()

  override def toMutable: intervalidus.mutable.Data[V, D] =
    intervalidus.mutable.Data(getAll)

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    this
