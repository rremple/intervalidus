package intervalidus.tinyrule

/**
  * A fact filter applies rules to a set of facts to come up with a new (possibly smaller) set of facts. These new facts
  * can have some attributes augmented or redacted. Filters can be combined through conjunction and disjunction.
  */
sealed trait FactFilter:
  def apply(facts: Set[Fact]): Set[Fact]

  def excluding(factIds: String*): ExcludeFilter = ExcludeFilter(this, factIds.toSet)
  def and(that: FactFilter): AndFilter = AndFilter(List(this, that))

  def or(that: FactFilter): OrFilter = OrFilter(List(this, that), FactMergeStyle.KeepingAll)
  def orWhenAbsent(that: FactFilter): OrFilter = OrFilter(List(this, that), FactMergeStyle.WhenAbsent)
  def orAsReplacement(that: FactFilter): OrFilter = OrFilter(List(this, that), FactMergeStyle.AsReplacement)

object FactFilter:
  extension (r: Rule)
    def filter: SelectFilter = SelectFilter(r)
    def redact(attributeNames: String*): RedactFilter = RedactFilter(r, attributeNames.toSet)

    def augment(fact: Fact): AugmentFilter = AugmentFilter(r, fact, FactMergeStyle.KeepingAll)
    def augmentWhenAbsent(fact: Fact): AugmentFilter = AugmentFilter(r, fact, FactMergeStyle.WhenAbsent)
    def augmentAsReplacement(fact: Fact): AugmentFilter = AugmentFilter(r, fact, FactMergeStyle.AsReplacement)

/**
  * Select a subset of facts based on a rule.
  */
case class SelectFilter(
  shouldInclude: Rule
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] = facts.filter(shouldInclude(_))

/**
  * Augments the attributes of some facts based on a rule.
  */
case class AugmentFilter(
  shouldAugment: Rule,
  augmentWith: Fact,
  mergeStyle: FactMergeStyle = FactMergeStyle.KeepingAll
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] = facts.map: f =>
    if shouldAugment(f) then f.merge(augmentWith, mergeStyle) else f

/**
  * Redacts the attributes of some facts based on a rule.
  */
case class RedactFilter(
  shouldRedact: Rule,
  excludedAttributeNames: Set[String]
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] = facts.map: f =>
    if shouldRedact(f) then f.copy(attributes = f.attributes.filterNot(a => excludedAttributeNames.contains(a.name)))
    else f

/**
  * Excludes facts from the results of a base filter by id.
  */
case class ExcludeFilter(
  baseFilter: FactFilter,
  shouldExcludeFactIds: Set[String]
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] =
    baseFilter(facts).filterNot(f => shouldExcludeFactIds.contains(f.id))

/**
  * Combines the result of a collection of filters. Facts that make it through any filter are included. Attributes are
  * combined based on the merge style.
  */
case class OrFilter(
  includeFilters: List[FactFilter],
  mergeStyle: FactMergeStyle = FactMergeStyle.KeepingAll
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] =
    includeFilters.foldLeft(Set.empty[Fact]): (priorFacts, filter) =>
      Fact.mergeAll(priorFacts, filter(facts), mergeStyle)

/**
  * Combines the result of a collection of filters. Only facts that make it through all the filters are included. unlike
  * [[OrFilter]], attributes are combined based strictly on how each include filter is defined.
  */
case class AndFilter(
  includeFilters: List[FactFilter]
) extends FactFilter:
  override def apply(facts: Set[Fact]): Set[Fact] =
    includeFilters.foldLeft(facts): (priorFacts, filter) =>
      filter(priorFacts)
