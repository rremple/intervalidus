package intervalidus.tinyrule

import java.time.LocalDate

/**
  * A rule can be applied to a fact with a boolean result (i.e., in the language of predicate logic, rules are
  * predicates, facts are subjects, and the applications of rules to facts are propositions). Rules can be combined
  * through conjunction, disjunction, and negation.
  */
sealed trait Rule:
  def apply(f: Fact): Boolean
  def and(that: Rule): AndRule = AndRule(this, that)
  def or(that: Rule): OrRule = OrRule(this, that)
object Rule:
  def not(that: Rule): NotRule = NotRule(that)
  def always: Rule = WildcardRule

case class AndRule(rules: List[Rule]) extends Rule:
  override def apply(that: Fact): Boolean = rules.forall(_.apply(that))
object AndRule:
  def apply(rules: Rule*): AndRule = AndRule(rules.toList)

case class OrRule(rules: List[Rule]) extends Rule:
  override def apply(that: Fact): Boolean = rules.exists(_.apply(that))
object OrRule:
  def apply(rules: Rule*): OrRule = OrRule(rules.toList)

case class NotRule(rule: Rule) extends Rule:
  override def apply(that: Fact): Boolean = !rule.apply(that)

case object WildcardRule extends Rule:
  override def apply(f: Fact): Boolean = true

/**
  * Ways one attribute's value can be compared to another. Not all attribute rule types support all match types, e.g.,
  * only StringRules support Contains.
  */
enum MatchType:
  case Equals, GreaterThan, LessThan, Contains

  /**
    * Provides a way to define attribute rules in an even less verbose way. For example `Equals(Attribute("myInt", 10))`
    * is equivalent to `IntRule(Equals, IntAttribute("myInt", 10))`, interpreted as `myInt == 10` when applying the rule
    * to a fact.
    * @param anyAttribute
    *   attribute to match, some specific subtype of `Attribute[?]`
    * @return
    *   a new attribute rule with a specific subtype of `AttributeRule[?]` based on the attribute type
    */
  transparent inline def apply(anyAttribute: Any): Any = AttributeRule(this, anyAttribute)

  /**
    * Provides a way to define attribute rules in a less verbose way. For example `Equals("myInt", 10)` is equivalent to
    * `IntRule(Equals, IntAttribute("myInt", 10))`, interpreted as `myInt == 10` when applying the rule to a fact.
    *
    * @param name
    *   attribute name to match
    * @param anyValue
    *   attribute value to match
    * @return
    *   a new attribute rule with a specific subtype of `AttributeRule[?]` based on the attribute value type
    */
  transparent inline def apply(name: String, anyValue: Any): Any = AttributeRule(this, Attribute(name, anyValue))

extension (name: String)
  transparent inline def attributeEquals(anyValue: Any): Any = MatchType.Equals(name, anyValue)
  transparent inline def attributeGreaterThan(anyValue: Any): Any = MatchType.GreaterThan(name, anyValue)
  transparent inline def attributeLessThan(anyValue: Any): Any = MatchType.LessThan(name, anyValue)
  transparent inline def attributeContains(anyValue: Any): Any = MatchType.Contains(name, anyValue)

/**
  * Some match attribute that is tested (based on the match type) against all of a fact's attributes
  * @tparam S
  *   match attribute type
  */
sealed trait AttributeRule[S] extends Rule:
  override def apply(f: Fact): Boolean = f.attributes.exists(apply)

  // match a single attribute
  def apply(that: Attribute[?]): Boolean

  def matchType: MatchType
  def matchAttribute: Attribute[S]
  protected def nameAndAttributeMatch(that: Attribute[?])(
    attributeMatch: PartialFunction[Attribute[?], Boolean]
  ): Boolean = that.name == matchAttribute.name && attributeMatch.applyOrElse(that, _ => false)

object AttributeRule:
  transparent inline def apply(matchType: MatchType, anyAttribute: Any): Any = inline anyAttribute match
    case v: BooleanAttribute => BooleanRule(matchType, v)
    case v: IntAttribute     => IntRule(matchType, v)
    case v: DoubleAttribute  => DoubleRule(matchType, v)
    case v: StringAttribute  => StringRule(matchType, v)
    case v: DateAttribute    => DateRule(matchType, v)

case class BooleanRule(matchType: MatchType, matchAttribute: BooleanAttribute) extends AttributeRule[Boolean]:
  override def apply(that: Attribute[?]): Boolean = nameAndAttributeMatch(that):
    case BooleanAttribute(_, thatValue) =>
      matchType match
        case MatchType.Equals => thatValue == matchAttribute.value
        case _                => false

case class IntRule(matchType: MatchType, matchAttribute: IntAttribute) extends AttributeRule[Int]:
  override def apply(that: Attribute[?]): Boolean = nameAndAttributeMatch(that):
    case IntAttribute(_, thatValue) =>
      matchType match
        case MatchType.Equals      => thatValue == matchAttribute.value
        case MatchType.GreaterThan => thatValue > matchAttribute.value
        case MatchType.LessThan    => thatValue < matchAttribute.value
        case _                     => false

case class DoubleRule(matchType: MatchType, matchAttribute: DoubleAttribute) extends AttributeRule[Double]:
  override def apply(that: Attribute[?]): Boolean = nameAndAttributeMatch(that):
    case DoubleAttribute(_, thatValue) =>
      matchType match
        case MatchType.Equals      => thatValue == matchAttribute.value
        case MatchType.GreaterThan => thatValue > matchAttribute.value
        case MatchType.LessThan    => thatValue < matchAttribute.value
        case _                     => false

case class StringRule(matchType: MatchType, matchAttribute: StringAttribute) extends AttributeRule[String]:
  override def apply(that: Attribute[?]): Boolean = nameAndAttributeMatch(that):
    case StringAttribute(_, thatValue) =>
      matchType match
        case MatchType.Equals      => thatValue == matchAttribute.value
        case MatchType.GreaterThan => thatValue > matchAttribute.value
        case MatchType.LessThan    => thatValue < matchAttribute.value
        case MatchType.Contains    => thatValue.contains(matchAttribute.value)

case class DateRule(matchType: MatchType, matchAttribute: DateAttribute) extends AttributeRule[LocalDate]:
  override def apply(that: Attribute[?]): Boolean = nameAndAttributeMatch(that):
    case DateAttribute(_, thatValue) =>
      matchType match
        case MatchType.Equals      => thatValue.isEqual(matchAttribute.value)
        case MatchType.GreaterThan => thatValue.isAfter(matchAttribute.value)
        case MatchType.LessThan    => thatValue.isBefore(matchAttribute.value)
        case _                     => false
