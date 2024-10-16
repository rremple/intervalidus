package intervalidus.collection.mutable

import intervalidus.collection.MultiDictSortedBase

import scala.collection.mutable
import scala.collection.immutable.SortedSet

/**
  * Constructors for mutable multimaps.
  */
object MultiDictSorted:
  /**
    * Constructs a new, empty multimap.
    * @tparam K
    *   the key type.
    * @tparam V
    *   the value type.
    * @return
    *   a new multimap
    */
  def apply[K, V: Ordering](): MultiDictSorted[K, V] = new MultiDictSorted[K, V](
    mutable.Map[K, SortedSet[V]]().withDefaultValue(SortedSet.empty)
  )

  /**
    * Constructs a new multimap from initial data.
    * @param elems
    *   key-value pairs used as initial data.
    * @tparam K
    *   the key type.
    * @tparam V
    *   the value type.
    * @return
    *   a new multimap
    */
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
class MultiDictSorted[K, V: Ordering] private (dict: mutable.Map[K, SortedSet[V]])
  extends MultiDictSortedBase[K, V](dict):

  override def clone(): MultiDictSorted[K, V] = new MultiDictSorted[K, V](dict.clone())

  /**
    * Associate a value with a key.
    * @param elem
    *   a key-value pair to associate.
    */
  def addOne(elem: (K, V)): Unit = dict.update(elem._1, dict(elem._1) + elem._2)

  /**
    * Disassociate a value from a key.
    * @param elem
    *   a key-value pair which should no longer be associated.
    */
  def subtractOne(elem: (K, V)): Unit = dict.update(elem._1, dict(elem._1) - elem._2)

  /**
    * Associate many keys and values.
    * @param elems
    *   key-value pairs to associate.
    */
  def addAll(elems: Iterable[(K, V)]): Unit = elems.foreach(addOne)

  /**
    * Clear all associations.
    */
  def clear(): Unit = dict.clear()
