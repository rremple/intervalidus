package intervalidus

import intervalidus.collection.BoxedPayloadLike

import java.time.LocalDate

/**
  * A value that is valid in some discrete interval. This defines a partial function where all domain elements that are
  * part of the interval map to the specified value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam D
  *   the type of discrete domain used in the discrete interval assigned to each value (the domain).
  * @tparam I
  *   the type of discrete interval in which the value is valid.
  * @tparam Self
  *   F-bounded self type.
  */
trait ValidDataLike[
  V,
  D <: DiscreteDomainLike[D],
  I <: DiscreteIntervalLike[D, I],
  Self <: ValidDataLike[V, D, I, Self]
] extends PartialFunction[D, V]:
  this: Self =>

  type BoxedPayloadType <: BoxedPayloadLike[Self, ?, ?, BoxedPayloadType]

  /**
    * Approximate this valid data as a boxed payload in double space based on the domain ordered hash.
    *
    * @return
    *   a new boxed payload that can be managed in a box tree
    */
  def asBoxedPayload: BoxedPayloadType

  /**
    * The value valid in this interval.
    */
  def value: V

  /**
    * The interval in which the value is valid.
    */
  def interval: I

  override def toString: String = s"$interval -> $value"

  // by default, put the interval in parens so the x function works in 2D and 3D
  protected def qualifiedInterval: String = s"(${interval.toCodeLikeString})"

  /**
    * Alternative to toString for something that looks more like code.
    */
  def toCodeLikeString: String = s"$qualifiedInterval -> ${value match
      case s: String => s"\"$s\""
      case _         => value.toString
    }"

  /**
    * When stored in a collection, the start of the corresponding interval is the key.
    *
    * @return
    *   a tuple of the domain-like key with this valid data
    */
  def withKey: (D, Self) = interval.start -> this

  override def apply(domainIndex: D): V =
    if isDefinedAt(domainIndex) then value else throw new Exception(s"Not defined at $domainIndex")

  override def isDefinedAt(d: D): Boolean = interval contains d
