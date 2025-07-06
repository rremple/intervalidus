package intervalidus.immutable

import intervalidus.*
import intervalidus.Domain.NonEmptyTail
import intervalidus.collection.mutable.{BoxTree, MultiMapSorted}

import scala.collection.mutable

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

/**
  * Immutable dimensional data.
  *
  * @tparam V
  *   the value type for valid data.
  * @tparam D
  *   the domain type for intervals, must be [[DomainLike]].
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

  override def mapValues[B](
    f: V => B
  ): Data[B, D] = Data(
    getAll.map(d => d.copy(value = f(d.value)))
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

  override def zipAll[B](that: DimensionalBase[B, D], thisElem: V, thatElem: B): Data[(V, B), D] =
    Data(zipAllData(that, thisElem, thatElem))

  override def toMutable: intervalidus.mutable.Data[V, D] =
    intervalidus.mutable.Data(getAll)

  override def toImmutable: intervalidus.immutable.Data[V, D] =
    this

  override def getByHeadIndex[H: DomainValueLike](headIndex: Domain1D[H])(using
    D =:= Domain1D[H] *: Tuple.Tail[D],
    DomainLike[NonEmptyTail[D]]
  ): Data[V, NonEmptyTail[D]] =
    Data(getByHeadIndexData(headIndex))
