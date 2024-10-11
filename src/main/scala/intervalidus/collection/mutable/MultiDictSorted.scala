package intervalidus.collection.mutable

import scala.collection.mutable
import scala.collection.immutable.SortedSet

/**
  * Constructors for mutable multimaps.
  */
object MultiDictSorted:
  def apply[K, V: Ordering](): MultiDictSorted[K, V] = new MultiDictSorted[K, V](
    mutable.Map[K, SortedSet[V]]().withDefaultValue(SortedSet.empty)
  )

  def from[K, V: Ordering](elems: Iterable[(K, V)]): MultiDictSorted[K, V] =
    val elements = elems.groupMap(_._1)(_._2).map((k, vs) => k -> SortedSet.from(vs))
    val dict = mutable.Map.from(elements).withDefaultValue(SortedSet.empty)
    new MultiDictSorted[K, V](dict)

/**
  * A mutable multimap where multiple values can be associated with the same key. Similar to `SortedMultiDict` in
  * `scala-collection-contrib`, but this returns values in order (values are stored in a sorted set). Also, this only
  * implements a small subset of methods needed in this project.
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type, must have an Ordering[V] given
  */
class MultiDictSorted[K, V: Ordering] private (dict: mutable.Map[K, SortedSet[V]]):

  override def clone(): MultiDictSorted[K, V] = new MultiDictSorted[K, V](dict.clone())

  def addOne(elem: (K, V)): Unit = dict.update(elem._1, dict(elem._1) + elem._2)

  def subtractOne(elem: (K, V)): Unit = dict.update(elem._1, dict(elem._1) - elem._2)

  def addAll(elems: Iterable[(K, V)]): Unit = elems.foreach(addOne)

  def get(key: K): Iterable[V] = dict(key).toSeq

  def keySet: collection.Set[K] = dict.keySet

  def clear(): Unit = dict.clear()
