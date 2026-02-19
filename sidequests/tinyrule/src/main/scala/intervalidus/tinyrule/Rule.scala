package intervalidus.tinyrule

/**
  * A rule can be applied to a fact with a boolean result (i.e., in the language of predicate logic, rules are
  * predicates, facts are constants, and the application of facts to a rule is a proposition). Rules can be combined
  * through conjunction, disjunction, and negation.
  */
sealed trait Rule:
  def apply(f: Fact): Boolean
  infix def and(that: Rule): AndRule = AndRule(this, that)
  infix def or(that: Rule): OrRule = OrRule(this, that)

/**
  * Common definitions for rules.
  */
object Rule:
  def not(that: Rule): NotRule = NotRule(that)
  def always: Rule = WildcardRule

/**
  * Rule conjunction.
  */
case class AndRule(rules: List[Rule]) extends Rule:
  override def apply(that: Fact): Boolean = rules.forall(_.apply(that))

/**
  * Common definitions for conjunctions.
  */
object AndRule:
  def apply(rules: Rule*): AndRule = AndRule(rules.toList)

/**
  * Rule disjunction.
  */
case class OrRule(rules: List[Rule]) extends Rule:
  override def apply(that: Fact): Boolean = rules.exists(_.apply(that))

/**
  * Common definitions for disjunctions.
  */
object OrRule:
  def apply(rules: Rule*): OrRule = OrRule(rules.toList)

/**
  * Rule negation.
  */
case class NotRule(rule: Rule) extends Rule:
  override def apply(that: Fact): Boolean = !rule.apply(that)

/**
  * Rule that is always true.
  */
case object WildcardRule extends Rule:
  override def apply(f: Fact): Boolean = true

/**
  * How one attribute's value can be compared to another. Not all attribute rule types support all match types, e.g.,
  * only StringRule support Contains.
  */
enum MatchType:
  case Equals, GreaterThan, LessThan, Contains

  /**
    * Provides a way to define attribute rules in an even less verbose way.
    *
    * For example, `Equals(Attribute("myInt", 10))` is equivalent to `AttributeRule(Equals, Attribute("myInt", 10))`,
    * interpreted as `myInt == 10` when applying the rule to a fact.
    *
    * @param anyAttribute
    *   attribute to match, some specific subtype of `Attribute[?]`
    * @return
    *   a new attribute rule with a specific subtype of `AttributeRule[?]` based on the attribute type
    */
  def apply[T: AttributeValueLike](anyAttribute: Attribute[T]): AttributeRule[T] =
    AttributeRule(this, anyAttribute)

  /**
    * Provides a way to define attribute rules in a less verbose way.
    *
    * For example, `Equals("myInt", 10)` is equivalent to `AttributeRule(Equals, Attribute("myInt", 10))`, interpreted
    * as `myInt == 10` when applying the rule to a fact.
    *
    * @param name
    *   attribute name to match
    * @param anyValue
    *   attribute value to match
    * @return
    *   a new attribute rule with a specific subtype of `AttributeRule[?]` based on the attribute value type
    */
  def apply[T: AttributeValueLike](name: String, anyValue: T): AttributeRule[T] =
    AttributeRule(this, Attribute(name, anyValue))

extension (name: String)
  infix def attributeEquals[T: AttributeValueLike](anyValue: T): AttributeRule[T] =
    MatchType.Equals(name, anyValue)
  infix def attributeGreaterThan[T: AttributeValueLike](anyValue: T): AttributeRule[T] =
    MatchType.GreaterThan(name, anyValue)
  infix def attributeLessThan[T: AttributeValueLike](anyValue: T): AttributeRule[T] =
    MatchType.LessThan(name, anyValue)
  infix def attributeContains[T: AttributeValueLike](anyValue: T): AttributeRule[T] =
    MatchType.Contains(name, anyValue)

/**
  * Some match attribute that is tested (based on the match type) against all of a fact's attributes
  * @tparam S
  *   match attribute type
  */
case class AttributeRule[S](
  matchType: MatchType,
  matchAttribute: Attribute[S]
)(using attributeValueLike: AttributeValueLike[S])
  extends Rule:

  override def apply(f: Fact): Boolean = f.attributes.exists(apply)

  // match a single attribute
  def apply(that: Attribute[?]): Boolean = attributeValueLike.matches(matchType, matchAttribute, that)
