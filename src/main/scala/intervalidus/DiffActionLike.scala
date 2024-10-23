package intervalidus

/**
  * Create/update/delete actions (like CQRS mutation commands). Used when extrapolating or applying event source-style
  * information. Note that this is not an event-sourced data structure, and history of mutations are not maintained.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam D
  *   the type of discrete domain used in the discrete interval assigned to each value (the domain).
  * @tparam I
  *   the type of discrete interval in which the value is valid.
  * @tparam ValidData
  *   the valid data type. Must be `DataLike` based on V, D, and I.
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
