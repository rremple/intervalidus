package intervalidus.collection

import scala.collection.immutable.SortedSet

/**
  * A multimap where multiple values can be associated with the same key. Similar to `SortedMultiDict` in
  * `scala-collection-contrib`, but this returns values in order (values are stored in a sorted set). Also, this only
  * implements a small subset of methods needed in this project.
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type (uses `Ordering[V]`)
  */
trait MultiMapSortedLike[K, V: Ordering](dict: collection.Map[K, SortedSet[V]]):

  /**
    * Retrieves all the values associated with the given key.
    *
    * @param key
    *   the key.
    * @return
    *   the values associated with the given key.
    */
  def get(key: K): Iterable[V] = dict.apply(key).toSeq

  /**
    * Collects all keys of this map in a set.
    *
    * @return
    *   a set containing all keys of this multimap.
    */
  def keySet: collection.Set[K] = dict.keySet
