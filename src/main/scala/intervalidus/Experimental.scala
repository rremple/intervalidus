package intervalidus

opaque type Experimental = Map[String, Boolean]

object Experimental:
  /**
    * No experimental features are enabled
    *
    * @return
    *   configuration with no experimental features enabled
    */
  def none: Experimental = Map().withDefaultValue(false)

  /**
    * Only specific experimental features are enabled
    *
    * @param features
    *   string description of experimental features to enable, e.g., "noSearchTree", "bruteForceUpdate",
    *   "requireDisjoint"
    * @return
    *   configuration for these experimental features enabled, and all others disabled
    */
  def apply(features: String*): Experimental = features.foldLeft(none)(_.updated(_, true))

  /**
    * All experiments are enabled
    *
    * @return
    *   configuration with all experimental features enabled
    */
  def all: Experimental = Map().withDefaultValue(true)

  /**
    * Only specific experimental features are disabled
    *
    * @param features
    *   string description of experimental features to disable, e.g., "noSearchTree", "bruteForceUpdate",
    *   "requireDisjoint"
    * @return
    *   configuration for having all experimental features enabled except those specified
    */
  def allExcept(features: String*): Experimental = features.foldLeft(all)(_.updated(_, false))

  // by default, experimental features are turned off
  given Experimental = Experimental.none // ("printExperimental")

extension (enabledExperimental: Experimental)
  /**
    * Is this experimental feature enabled?
    *
    * @param feature
    *   name of the experimental feature
    * @return
    *   is the feature is enabled?
    */
  def enabled(feature: String): Boolean = enabledExperimental(feature)

  /**
    * If the experimental feature is enabled, return the experimental result. If not, return the non-experimental
    * result.
    *
    * @param feature
    *   name of the experimental feature
    * @param nonExperimental
    *   result of non-experimental variant
    * @param experimental
    *   result of experimental variant
    * @tparam T
    *   result type
    * @return
    *   either experimental or non-experimental result, depending on what is enabled.
    */
  def control[T](
    feature: String
  )(
    nonExperimental: => T,
    experimental: => T
  ): T =
    if enabledExperimental(feature)
    then
      if enabledExperimental("printExperimental") then println(s"experimental: $feature enabled")
      experimental
    else
      if enabledExperimental("printExperimental") then println(s"non-experimental: $feature disabled")
      nonExperimental

  /**
    * If the experimental feature is not enabled, return the non-experimental result. If it is enabled, compare the
    * experimental and non-experimental results. If they match, return that result. Otherwise, return the onFailure
    * result (which would usually just throw after maybe logging some details about the mismatch).
    *
    * @param feature
    *   name of the experimental feature
    * @param nonExperimental
    *   result of non-experimental variant
    * @param experimental
    *   result of experimental variant
    * @param onFailure
    *   when non-experimental and experimental do not match, this function is applied to the results (may log/throw, or
    *   choose one to return, or whatever). By default, it just throws.
    * @tparam T
    *   result type
    * @return
    *   either experimental or non-experimental result, depending on what is enabled.
    */
  def parallelCheck[T](
    feature: String
  )(
    nonExperimental: => T,
    experimental: => T
  )(
    onFailure: (T, T) => T = (_: T, _: T) => throw new Exception("Experiment failed")
  ): T =
    if enabledExperimental(feature)
    then
      if enabledExperimental("printExperimental") then println(s"experimental: $feature enabled (with parallel check)")
      if experimental == nonExperimental then experimental
      else
        println(s"Experiment $feature failed")
        println(s"Non-Experimental result: $nonExperimental")
        println(s"Experimental result: $experimental")
        onFailure(nonExperimental, experimental)
    else
      if enabledExperimental("printExperimental") then println(s"non-experimental: $feature disabled")
      nonExperimental