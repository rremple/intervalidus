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
  private inline def nameAndAttributeMatch[T: AttributeValueLike](
    thisAttribute: Attribute[T],
    thatAttribute: Attribute[?]
  )(
    attributeMatch: PartialFunction[Attribute[?], Boolean]
  ): Boolean = thatAttribute.name == thisAttribute.name &&
    attributeMatch.applyOrElse(thatAttribute, _ => false)

  given AttributeValueLike[Boolean] with
    inline def matches(matchType: MatchType, thisAttribute: Attribute[Boolean], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(thisAttribute, thatAttribute):
        case Attribute(_, thatValue: Boolean) =>
          matchType match
            case MatchType.Equals => thatValue == thisAttribute.value
            case _                => false

  given AttributeValueLike[Int] with
    inline def matches(matchType: MatchType, thisAttribute: Attribute[Int], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(thisAttribute, thatAttribute):
        case Attribute(_, thatValue: Int) =>
          matchType match
            case MatchType.Equals      => thatValue == thisAttribute.value
            case MatchType.GreaterThan => thatValue > thisAttribute.value
            case MatchType.LessThan    => thatValue < thisAttribute.value
            case _                     => false

  given AttributeValueLike[Double] with
    inline def matches(matchType: MatchType, thisAttribute: Attribute[Double], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(thisAttribute, thatAttribute):
        case Attribute(_, thatValue: Double) =>
          matchType match
            case MatchType.Equals      => thatValue == thisAttribute.value
            case MatchType.GreaterThan => thatValue > thisAttribute.value
            case MatchType.LessThan    => thatValue < thisAttribute.value
            case _                     => false

  given AttributeValueLike[String] with
    inline def matches(matchType: MatchType, thisAttribute: Attribute[String], thatAttribute: Attribute[?]): Boolean =
      nameAndAttributeMatch(thisAttribute, thatAttribute):
        case Attribute(_, thatValue: String) =>
          matchType match
            case MatchType.Equals      => thatValue == thisAttribute.value
            case MatchType.GreaterThan => thatValue > thisAttribute.value
            case MatchType.LessThan    => thatValue < thisAttribute.value
            case MatchType.Contains    => thatValue.contains(thisAttribute.value)
            case _                     => false

  given AttributeValueLike[LocalDate] with
    inline def matches(
      matchType: MatchType,
      thisAttribute: Attribute[LocalDate],
      thatAttribute: Attribute[?]
    ): Boolean =
      nameAndAttributeMatch(thisAttribute, thatAttribute):
        case Attribute(_, thatValue: LocalDate) =>
          matchType match
            case MatchType.Equals      => thatValue.isEqual(thisAttribute.value)
            case MatchType.GreaterThan => thatValue.isAfter(thisAttribute.value)
            case MatchType.LessThan    => thatValue.isBefore(thisAttribute.value)
            case _                     => false
