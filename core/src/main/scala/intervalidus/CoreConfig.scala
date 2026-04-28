package intervalidus

import intervalidus.collection.CollectionConfig

/**
  * @param capacityHint
  *   If specified, an interval representing the expected extent of the data domain, excluding unbounded portions. This
  *   helps prevent unnecessary box tree reorgs as data are inserted. The capacity hint is used to establish the spatial
  *   capacity, which should be large enough to contain at least the initial data. If it isn't, the boundary of the tree
  *   will be resized along the way (a performance issue, not a functional one). Default is None.
  * @param isolationLevel
  *   How isolated should readers be from writers? Default is Serializable.
  * @param compressOnUpdate
  *   Should affected values be compressed following an update operation? Default is true.
  * @param experimental
  *   Enables/disables experimental features.
  * @param collectionConfig
  *   Configuration for underlying data structures like BoxTree.
  */
case class CoreConfig[D <: NonEmptyTuple: DomainLike](
  capacityHint: Option[Interval[D]],
  isolationLevel: CoreConfig.IsolationLevel,
  compressOnUpdate: Boolean
)(using
  val experimental: Experimental,
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
    * A new configuration with a new capacity hint value.
    */
  def withIsolationLevel(value: CoreConfig.IsolationLevel): CoreConfig[D] =
    copy(isolationLevel = value)

  /**
    * A new configuration with a new compress on update value.
    */
  def withCompressOnUpdate(value: Boolean): CoreConfig[D] =
    copy(compressOnUpdate = value)

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
  /**
    * How isolated should readers be from writers?
    */
  enum IsolationLevel:
    /**
      * Uses a "Clean" transaction (start) providing absolute thread safety. Readers are 100% isolated from writers, so
      * partially-updated structures are not visible. Performance of mutable structures is on par with immutable
      * structures.
      */
    case Serializable

    /**
      * Uses a "Dirty" transaction (startDirty) providing maximum throughput. No surgical array clones happen during an
      * update. Performance of the "set" operation has been measured to be about 20% faster. The risk is that concurrent
      * readers might see "wonky" data or hit a rare array-reorg crash, so this should only be used in situations that
      * have no concurrency.
      */
    case ReadUncommitted

  given default[D <: NonEmptyTuple](using DomainLike[D], Experimental, CollectionConfig): CoreConfig[D] =
    CoreConfig(capacityHint = None, isolationLevel = IsolationLevel.Serializable, compressOnUpdate = true)
