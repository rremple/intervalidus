package intervalidus

import intervalidus.collection.Coordinate

/**
  * Template for a type class with operations on a discrete domain. A discrete domain is used in defining and operating
  * on a discrete interval.
  *
  * @tparam D
  *   discrete domain type
  */
trait DomainLike[D: Ordering]:
  extension (domain: D)
    /**
      * Tests if domain is infinite (i.e., Top or Bottom) in all dimensions.
      */
    def isUnbounded: Boolean

    /**
      * Alternative to toString for something that looks more like code
      */
    def toCodeLikeString: String

    /**
      * Approximate this domain as a coordinate in double space based on the domain ordered hash.
      *
      * @return
      *   a new coordinate for boxes managed in box search trees
      */
    def asCoordinate: Coordinate

    /**
      * Successor of this in all dimensions, where `Bottom` and `Top` are their own successors, and the successor of
      * `maxValue` is `Top`. See [[https://en.wikipedia.org/wiki/Successor_function]].
      *
      * @return
      *   successor of this
      */
    def successor: D

    /**
      * Predecessor of this in all dimensions, where `Bottom` and `Top` are their own predecessors, and the predecessor
      * of `minValue` is `Bottom`. See [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]].
      *
      * @return
      *   successor of this
      */
    def predecessor: D

    /**
      * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
      *
      * @param interval
      *   interval to test.
      * @return
      *   true if this belongs to the specified interval, false otherwise.
      */
    infix def belongsTo[I <: IntervalLike[D, I]](interval: I): Boolean = interval contains domain

    // equivalent symbolic method names

    /**
      * Same as [[belongsTo]]
      *
      * Tests if this belongs to an interval. See [[https://en.wikipedia.org/wiki/Element_(mathematics)]].
      *
      * @param interval
      *   interval to test.
      * @return
      *   true if this belongs to the specified interval, false otherwise.
      */
    def ∈[I <: IntervalLike[D, I]](interval: I): Boolean = domain belongsTo interval
