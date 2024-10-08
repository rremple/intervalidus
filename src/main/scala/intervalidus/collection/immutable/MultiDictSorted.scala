package intervalidus.collection.immutable

import scala.collection.immutable
import scala.collection.immutable.SortedSet

/**
  * Constructors for immutable multi-maps.
  */
object MultiDictSorted:
  def apply[K, V: Ordering](): MultiDictSorted[K, V] = new MultiDictSorted(
    Map[K, SortedSet[V]]().withDefaultValue(SortedSet.empty)
  )

  def from[K, V: Ordering](elems: Iterable[(K, V)]): MultiDictSorted[K, V] =
    val dict = elems.groupMap(_._1)(_._2).map((k, vs) => k -> SortedSet.from(vs)).withDefaultValue(SortedSet.empty)
    new MultiDictSorted[K, V](dict)

/**
  * An immutable multi-map where multiple values can be associated with the same key. Similar to `SortedMultiDict` in
  * `scala-collection-contrib`, but this returns values in order (values are stored in a sorted set). Also, this only
  * implements a small subset of methods needed in this project.
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type, must have an Ordering[V] given
  */
class MultiDictSorted[K, V: Ordering] private (dict: Map[K, SortedSet[V]]):

  override def clone(): MultiDictSorted[K, V] = new MultiDictSorted[K, V](dict)

  def addOne(elem: (K, V)): MultiDictSorted[K, V] = new MultiDictSorted(
    dict.updated(elem._1, dict(elem._1) + elem._2)
  )

  def subtractOne(elem: (K, V)): MultiDictSorted[K, V] = new MultiDictSorted(
    dict.updated(elem._1, dict(elem._1) - elem._2)
  )

  def addAll(elems: Iterable[(K, V)]): MultiDictSorted[K, V] =
    elems.foldLeft(this)(_.addOne(_))

  def get(key: K): Iterable[V] = dict(key).toSeq

  def keySet: collection.Set[K] = dict.keySet

  def clear: MultiDictSorted[K, V] = MultiDictSorted()
