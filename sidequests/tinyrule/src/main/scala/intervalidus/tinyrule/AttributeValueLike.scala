package intervalidus.tinyrule

import java.time.LocalDate

/**
  * Type class for attribute values. Implementations are given for Boolean, Int, Double, String, and LocalDate.
  *
  * @tparam T
  *   the attribute value type
  */
trait AttributeValueLike[T]:
  def matches(matchType: MatchType, thisAttribute: Attribute[T], thatAttribute: Attribute[?]): Boolean

object AttributeValueLike:
  import MatchType.*

  private type Matches[T] = PartialFunction[(T, MatchType, T), Boolean]

  private inline def nameAndAttributeMatch[T: AttributeValueLike](
    matchType: MatchType,
    thisAttribute: Attribute[T],
    thatAttribute: Attribute[?],
    matches: Matches[T]
  ): Boolean = thatAttribute match
    case Attribute(thatName, thatValue: T) if thatName == thisAttribute.name =>
      matches.applyOrElse((thatValue, matchType, thisAttribute.value), _ => false)
    case _ => false

  given AttributeValueLike[Boolean] with
    private val valueMatch: Matches[Boolean] =
      case (thatValue, Equals, thisValue) => thatValue == thisValue

    inline def matches(matchType: MatchType, thisAttribute: Attribute[Boolean], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(matchType, thisAttribute, thatAttribute, valueMatch)

  given AttributeValueLike[Int] with
    private val valueMatch: Matches[Int] =
      case (thatValue, Equals, thisValue)      => thatValue == thisValue
      case (thatValue, GreaterThan, thisValue) => thatValue > thisValue
      case (thatValue, LessThan, thisValue)    => thatValue < thisValue

    inline def matches(matchType: MatchType, thisAttribute: Attribute[Int], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(matchType, thisAttribute, thatAttribute, valueMatch)

  given AttributeValueLike[Double] with
    private val valueMatch: Matches[Double] =
      case (thatValue, Equals, thisValue)      => thatValue == thisValue
      case (thatValue, GreaterThan, thisValue) => thatValue > thisValue
      case (thatValue, LessThan, thisValue)    => thatValue < thisValue

    inline def matches(matchType: MatchType, thisAttribute: Attribute[Double], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(matchType, thisAttribute, thatAttribute, valueMatch)

  given AttributeValueLike[String] with
    private val valueMatch: Matches[String] =
      case (thatValue, Equals, thisValue)      => thatValue == thisValue
      case (thatValue, GreaterThan, thisValue) => thatValue > thisValue
      case (thatValue, LessThan, thisValue)    => thatValue < thisValue
      case (thatValue, Contains, thisValue)    => thatValue.contains(thisValue)

    inline def matches(matchType: MatchType, thisAttribute: Attribute[String], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(matchType, thisAttribute, thatAttribute, valueMatch)

  given AttributeValueLike[LocalDate] with
    private val valueMatch: Matches[LocalDate] =
      case (thatValue, Equals, thisValue)      => thatValue.isEqual(thisValue)
      case (thatValue, GreaterThan, thisValue) => thatValue.isAfter(thisValue)
      case (thatValue, LessThan, thisValue)    => thatValue.isBefore(thisValue)

    inline def matches(
      matchType: MatchType,
      thisAttribute: Attribute[LocalDate],
      thatAttribute: Attribute[?]
    ): Boolean = nameAndAttributeMatch(matchType, thisAttribute, thatAttribute, valueMatch)
