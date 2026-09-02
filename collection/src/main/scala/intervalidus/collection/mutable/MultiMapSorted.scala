package intervalidus.collection.mutable

import intervalidus.collection.MultiMapSortedLike

import scala.collection.mutable
import scala.collection.immutable.SortedSet

/**
  * Constructors for mutable multimaps.
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
  def apply[K, V: Ordering](): MultiMapSorted[K, V] = new MultiMapSorted[K, V](
    mutable.Map[K, SortedSet[V]]().withDefaultValue(SortedSet.empty)
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
    val elements = elems
      .groupMap(_.key)(_.value)
      .map((key, values) => key -> SortedSet.from(values))
    val dict = mutable.Map.from(elements).withDefaultValue(SortedSet.empty)
    new MultiMapSorted[K, V](dict)

/**
  * $classDesc
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type (uses `Ordering[V]`)
  */
class MultiMapSorted[K, V: Ordering] private (dict: mutable.Map[K, SortedSet[V]])
  extends MultiMapSortedLike[K, V](dict):

  override def clone(): MultiMapSorted[K, V] = new MultiMapSorted[K, V](dict.clone())

  /**
    * Associate a value with a key.
    * @param elem
    *   a key-value pair to associate.
    */
  def addOne(elem: (key: K, value: V)): Unit = dict.update(elem.key, dict(elem.key) + elem.value)

  /**
    * Disassociate a value from a key.
    * @param elem
    *   a key-value pair which should no longer be associated.
    */
  def subtractOne(elem: (key: K, value: V)): Unit =
    val newValue = dict(elem.key) - elem.value
    if newValue.isEmpty then dict.remove(elem.key)
    else dict.update(elem.key, newValue)

  /**
    * Associate many keys and values.
    * @param elems
    *   key-value pairs to associate.
    */
  def addAll(elems: Iterable[(key: K, value: V)]): Unit =
    elems
      .groupMap(_.key)(_.value)
      .foreach: (key, values) =>
        dict.update(key, dict(key) ++ values)

  /**
    * Clear all associations.
    */
  def clear(): Unit = dict.clear()
