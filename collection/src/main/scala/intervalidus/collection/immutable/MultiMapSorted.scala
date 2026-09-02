package intervalidus.collection.immutable

import intervalidus.collection.MultiMapSortedLike

import scala.collection.immutable.SortedSet

/**
  * Constructors for immutable multimaps.
  */
object MultiMapSorted:
  /**
    * Constructs a new, empty multimap.
    * @tparam K
    *   the key type.
    * @tparam V
    *   value type (uses `Ordering[V]`)
    * @return
    *   a new multimap
    */
  def apply[K, V: Ordering](): MultiMapSorted[K, V] = new MultiMapSorted(
    Map[K, SortedSet[V]]().withDefaultValue(SortedSet.empty)
  )

  /**
    * Constructs a new multimap from initial data.
    * @param elems
    *   key-value pairs used as initial data.
    * @tparam K
    *   the key type.
    * @tparam V
    *   value type (uses `Ordering[V]`)
    * @return
    *   a new multimap
    */
  def from[K, V: Ordering](elems: Iterable[(key: K, value: V)]): MultiMapSorted[K, V] =
    val dict = elems
      .groupMap(_.key)(_.value)
      .map((key, values) => key -> SortedSet.from(values))
      .withDefaultValue(SortedSet.empty)
    new MultiMapSorted[K, V](dict)

/**
  * $classDesc
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type (uses `Ordering[V]`)
  */
class MultiMapSorted[K, V: Ordering] private (dict: Map[K, SortedSet[V]]) extends MultiMapSortedLike[K, V](dict):

  override def clone(): MultiMapSorted[K, V] = new MultiMapSorted[K, V](dict)

  /**
    * Associate a value with a key.
    * @param elem
    *   a key-value pair to associate.
    * @return
    *   a new multimap that includes the new association
    */
  def addOne(elem: (key: K, value: V)): MultiMapSorted[K, V] = new MultiMapSorted(
    dict.updated(elem.key, dict(elem.key) + elem.value)
  )

  /**
    * Disassociate a value from a key.
    * @param elem
    *   a key-value pair which should no longer be associated.
    * @return
    *   a new multimap that excludes the association
    */
  def subtractOne(elem: (key: K, value: V)): MultiMapSorted[K, V] =
    val newValue = dict(elem.key) - elem.value
    new MultiMapSorted(
      if newValue.isEmpty then dict.removed(elem.key)
      else dict.updated(elem.key, newValue)
    )

  /**
    * Associate many keys and values.
    * @param elems
    *   key-value pairs to associate.
    * @return
    *   a new multimap that includes the new associations
    */
  def addAll(elems: Iterable[(key: K, value: V)]): MultiMapSorted[K, V] = new MultiMapSorted(
    elems
      .groupMap(_.key)(_.value)
      .foldLeft(dict):
        case (updatedDict, (key, values)) =>
          updatedDict.updated(key, updatedDict(key) ++ values)
  )

  /**
    * Clear all associations.
    * @return
    *   a new multimap with no key-value associations.
    */
  def clear: MultiMapSorted[K, V] = MultiMapSorted()
