package intervalidus

import intervalidus.collection.Coordinate

/**
  * Template for a type class with operations on a domain (discrete or continuous). A domain is used in defining the
  * boundaries of an interval.
  *
  * @tparam D
  *   domain type
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
      * Domain adjacent to this in all dimensions from the "right", where `Bottom` and `Top` are considered
      * self-adjacent. For discrete domains, this is the successor, where the right adjacent of `maxValue` is `Top` --
      * see [[https://en.wikipedia.org/wiki/Successor_function]]. For continuous domains, this maps open points to
      * closed ones, and closed points to open ones (right and left complements are the same).
      *
      * @return
      *   right complement of this
      */
    def rightAdjacent: D

    /**
      * Domain adjacent to this in all dimensions from the "left", where `Bottom` and `Top` are considered
      * self-adjacent. For discrete domains, this is the predecessor, where the left adjacent of `minValue` is `Bottom`
      * -- see [[https://en.wikipedia.org/wiki/Primitive_recursive_function#Predecessor]]. For continuous domains, this
      * maps open points to closed ones, and closed points to open ones (right and left complements are the same).
      *
      * @return
      *   left complement of this
      */
    def leftAdjacent: D

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
