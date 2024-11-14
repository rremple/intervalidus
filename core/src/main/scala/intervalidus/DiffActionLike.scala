package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information.
  * @note
  *   intervalidus does not have event-sourced data structures, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam D
  *   the domain type for intervals. Must be [[DiscreteDomainLike]] and have an [[Ordering]].
  * @tparam I
  *   the interval type, based on the domain type. Must be [[DiscreteIntervalLike]] based on [[D]].
  * @tparam ValidData
  *   the valid data type. Must be [[ValidDataLike]] based on [[V]], [[D]], and [[I]].
  * @tparam Self
  *   F-bounded self type.
  */
trait DiffActionLike[
  V,
  D <: DiscreteDomainLike[D],
  I <: DiscreteIntervalLike[D, I],
  ValidData <: ValidDataLike[V, D, I, ValidData],
  Self <: DiffActionLike[V, D, I, ValidData, Self]
]
