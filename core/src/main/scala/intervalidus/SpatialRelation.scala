package intervalidus

/**
  * In the language of Mereotopology, these are the eight relations in region connection calculus, see
  * [[http://en.wikipedia.org/wiki/Region_connection_calculus]], and [[https://en.wikipedia.org/wiki/Mereotopology]]. In
  * intervalidus, "touching" is defined as being adjacent in at least one dimension and overlapping in the others -- it
  * does not involve any shared regions (i.e., the intersection is empty). Being "tangential", on the other hand,
  * involves sharing a boundary.
  */
enum SpatialRelation:
  /**
    * Disconnected
    */
  case DC

  /**
    * Externally Connected
    */
  case EC

  /**
    * Partially Overlapping
    */
  case PO

  /**
    * Equal
    */
  case EQ

  /**
    * Tangential Proper Part
    */
  case TPP

  /**
    * Non-Tangential Proper Part
    */
  case NTPP

  /**
    * Tangential Proper Part inverse
    */
  case TPPi

  /**
    * Non-Tangential Proper Part inverse
    */
  case NTPPi
