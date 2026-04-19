package intervalidus

import intervalidus.collection.CollectionConfig

case class CoreConfig[D <: NonEmptyTuple: DomainLike](
  /**
    * If specified, an interval representing the expected extent of the data domain, excluding unbounded portions. This
    * helps prevent unnecessary box tree reorgs as data are inserted. The capacity hint is used to establish the spatial
    * capacity, which should be large enough to contain at least the initial data. If it isn't, the boundary of the tree
    * will be resized along the way (a performance issue, not a functional one).
    */
  capacityHint: Option[Interval[D]]
)(using
  /**
    * Enables/disables experimental features.
    */
  val experimental: Experimental,

  /**
    * Configuration for underlying data structures like BoxTree.
    */
  val collectionConfig: CollectionConfig
):
  /**
    * Used for logging, which logs to the console by default. (You can swap for 'yourLogger.info')
    */
  def log(s: Any): Unit = collectionConfig.log(s)

  /**
    * A new configuration with a new capacity hint value.
    */
  def withCapacityHint(value: Interval[D]): CoreConfig[D] =
    copy(capacityHint = Some(value))

  /**
    * A new configuration with a new experimental value.
    */
  def withExperimental(value: Experimental): CoreConfig[D] =
    copy()(using experimental = value)

  /**
    * A new configuration with a new collection config value.
    */
  def withCollectionConfig(value: CollectionConfig): CoreConfig[D] =
    copy()(using collectionConfig = value)

  /**
    * A new configuration with a new collection config log action.
    */
  def withLogAction(value: Any => Unit): CoreConfig[D] =
    copy()(using collectionConfig = collectionConfig.withLogAction(value))

object CoreConfig:
  given default[D <: NonEmptyTuple](using DomainLike[D], Experimental, CollectionConfig): CoreConfig[D] =
    CoreConfig(capacityHint = None)
