package intervalidus.examples.spfm

import intervalidus.*
import intervalidus.ContinuousValue.LocalDateTimeContinuousValue
import intervalidus.DiscreteValue.{IntDiscreteValue, LocalDateDiscreteValue}
import intervalidus.Domain1D.Point
import intervalidus.Interval.Patterns.*
import intervalidus.Interval1D.*
import intervalidus.mutable.{DataMulti, DataVersioned}

import java.time.{LocalDate, LocalDateTime}
import scala.language.implicitConversions

// NotebookLM suggested this example, and its scenarios are captured in the Scaladoc.
// It also created most of the sample data.

/**
  * Based on the capabilities outlined in the sources and our ongoing discussion about Intervalidus's strengths, an
  * application that would be particularly interesting and demonstrate a comprehensive use of the library would be a
  * "Software Product Feature Lifecycle and Rollout Management System."
  *
  * This application would be a robust demonstration of Intervalidus's ability to model complex, evolving, and
  * multidimensional data where "validity as a kind of collection" is paramount. It would highlight the utility of
  * DataMulti, DataVersioned, and the full range of query methods, especially `values` and `intervals(value)`, which
  * enable reverse lookups that are often crucial in such systems.
  *
  * Think of it like a multi-layered, living map of a software product's evolution. Just as a map shows you where a
  * specific landmark (feature) exists (its intervals of validity) and what landmarks are present in a given area
  * (feature sets for a release), this system would enable navigating the complex landscape of product features across
  * versions, environments, and time, both forward and backward.
  *
  * Here's why this application would be an excellent fit and how it would leverage Intervalidus's core features...
  */
object SoftwareProductFeatureManagement:

  /**
    * Product Version/Release: A discrete, one-dimensional timeline of product versions (e.g., 1.0, 1.1, 2.0, 3.0).
    */
  case class Release(major: Int, minor: Int, patch: Int = 0):
    override def toString: String = s"v$major.$minor${if patch == 0 then "" else s".$patch"}"

  object Release:
    def apply(double: Double): Release =
      val major = double.toInt
      val minor = ((double - major) * 10).toInt
      Release(major, minor)

    def apply(string: String): Release = string.split('.').toList match
      case major :: Nil                   => Release(major.toInt, 0)
      case major :: minor :: Nil          => Release(major.toInt, minor.toInt)
      case major :: minor :: patch :: Nil => Release(major.toInt, minor.toInt, patch.toInt)
      case _                              => throw Exception(s"Invalid release: $string")

    val productRoadmap: IndexedSeq[Release] = IndexedSeq(
      Release(1.0),
      Release(1.1),
      Release(2.0),
      Release(3.0),
      Release("3.0.1")
    )

    given DiscreteValue[Release] = DiscreteValue.fromSeq(productRoadmap)

    // SDLC releasing implies availability in later releases

    def inRelease(release: Release): Interval1D[Release] = intervalFrom(release)

    def inRelease(double: Double): Interval1D[Release] = inRelease(Release(double))

    def inRelease(string: String): Interval1D[Release] = inRelease(Release(string))

  /**
    * Deployment Environment: Features might be active in Development, then Staging, and finally Production
    * environments. These could be modeled as a discrete dimension using custom domain values (e.g., an enum Environment
    * derives DiscreteValue).
    *
    * Custom Domain Values: Beyond Int for versions or LocalDate for time, defining custom DiscreteValue type classes
    * for Environment (e.g., Dev, Test, Prod)...
    */
  enum Environment derives DiscreteValue:
    case Development, Staging, Production

  object Environment:
    // In general, SDLC staging implies availability in lower environments leading up to the deployment environment
    def availableIn(environment: Environment): Interval1D[Environment] = interval(Environment.Development, environment)

    // But sometimes environments might get skipped, e.g., hotfixes may go straight to production
    def availableInOnly(environment: Environment): Interval1D[Environment] = intervalAt(environment)

    def anyEnvironment: Interval1D[Environment] = unbounded[Environment]

  /**
    * ...and Region (e.g., NA, EMEA, APAC) would make the code highly readable and domain-specific.
    *
    * Geographical Region/Market Segment: A feature might only be rolled out to specific regions (e.g., North America,
    * EMEA, APAC) or customer segments.
    */
  enum Region:
    case NorthAmerica, EMEA, APAC

  // Features
  case class Feat(id: String)

  def now(using dateTimeOps: CurrentDateTime): LocalDateTime = dateTimeOps.now()

  def today(using CurrentDateTime): LocalDate = now.toLocalDate

  // Effectivity persists until it is truncated (removed or superseded)
  def effectiveOn(localDate: LocalDate): Interval1D[LocalDate] = intervalFrom(localDate)

  def effectiveToday(using CurrentDateTime): Interval1D[LocalDate] = effectiveOn(today)

  // Plan/knowledge persists until it is replaced by a new plan/knowledge
  def plannedOn(localDateTime: LocalDateTime): Interval1D[LocalDateTime] = intervalFrom(localDateTime)

  def plannedOn(localDate: LocalDate): Interval1D[LocalDateTime] = plannedOn(localDate.atStartOfDay())

  def plannedNow(using CurrentDateTime): Interval1D[LocalDateTime] = plannedOn(now)

  // Create test data and prints query results to demonstrate the functions of the underlying datastructures
  def main(args: Array[String]): Unit =
    import DimensionalVersionedBase.VersionSelection.{Current, Unapproved}
    import Environment.*
    import Region.*
    import Release.inRelease

    // Simulate "now" as a fixed datetime
    given CurrentDateTime = CurrentDateTime.simulated(LocalDate.of(2025, 8, 1).atStartOfDay)

    // Shorthand date constructors
    def july(day: Int) = LocalDate.of(2025, 7, day)
    def august(day: Int) = LocalDate.of(2025, 8, day)
    def sept(day: Int) = LocalDate.of(2025, 9, day)

    /**
      * Multidimensionality: Software features often have validity across several dimensions
      *
      * Time (Effective Date/Bitemporality): As seen in the billing example, features become effective at certain dates,
      * and the knowledge of their existence might also change over time (bitemporal modeling).
      *
      * DataMulti for Feature Sets: A single product version or release typically contains multiple features.
      * Intervalidus's DataMulti is explicitly designed for this, allowing you to associate a Set of values (features)
      * with a given interval. This means you could query "What features were in Release 4?" and "[What were the release
      * events for a feature set] `Set(Feat(A), Feat(C))`.
      */
    val featureReleasePlan = DataMulti[
      Feat, // a release can include multiple features
      Domain.In3D[
        LocalDateTime, // when the release plan included (or maybe pulled) the feature
        LocalDate, // effective period when the feature was actually included (pro- or retrospective of the plan)
        Release // Product releases in this plan/effective period
      ]
    ]()

    def currentFeatureReleasePlan(using CurrentDateTime): DataMulti[Feat, Domain.In2D[LocalDate, Release]] =
      featureReleasePlan.getByHeadDimension(now)

    // Samples

    featureReleasePlan.addOne(
      (plannedOn(july(1)) x effectiveOn(july(1)) x inRelease(1.0)) -> Feat("A")
    )

    /**
      * ...complex scenarios like "Feature X is available in Production for North America from Version 2.0 onwards,
      * known as of today."
      */
    featureReleasePlan.addOne(
      (plannedNow x effectiveOn(july(26)) x inRelease(2.0)) -> Feat("X")
    )

    // Feature B added to Release 1.1, known slightly later than Release A for 1.0
    featureReleasePlan.addOne(
      (plannedOn(july(5)) x effectiveOn(july(15)) x inRelease(1.1)) -> Feat("B")
    )
    // Feature C added to Release 2.0, known from early July
    featureReleasePlan.addOne(
      (plannedOn(july(10)) x effectiveOn(july(20)) x inRelease(2.0)) -> Feat("C")
    )
    // Feature D added to Release 2.0, known from mid-July
    featureReleasePlan.addOne(
      (plannedOn(july(15)) x effectiveOn(july(20)) x inRelease(2.0)) -> Feat("D")
    )
    // Feat A is removed from Release 3.0, known as of now (Aug 1)
    featureReleasePlan.removeOne(
      (plannedNow x effectiveOn(august(1)) x inRelease(3.0)) -> Feat("A")
    )
    // Feat Y is planned for Release 3.0, known now, effective in mid-August
    featureReleasePlan.addOne(
      (plannedNow x effectiveOn(august(15)) x inRelease(3.0)) -> Feat("Y")
    )
    // Feat Z is added retrospectively to Release 2.0 (effective July 20), but known much later (Sept 1)
    featureReleasePlan.addOne(
      (plannedOn(sept(1)) x effectiveOn(july(20)) x inRelease(2.0)) -> Feat("Z")
    )
    // Feat B is upgraded/replaced in Release 2.0 (effective August 1), known now
    featureReleasePlan.removeOne(
      (plannedNow x effectiveOn(august(1)) x inRelease(2.0)) -> Feat("B")
    )
    featureReleasePlan.addOne(
      (plannedNow x effectiveOn(august(1)) x inRelease(2.0)) -> Feat("B_v2")
    )
    // A hotfix release 3.0.1 for Feat Y, known immediately
    featureReleasePlan.addOne(
      (plannedNow x effectiveOn(august(5)) x inRelease("3.0.1")) -> Feat("Y")
    )
    // Another feature for 3.0.1 known a bit later, effective same day
    featureReleasePlan.addOne(
      (plannedOn(august(3)) x effectiveOn(august(5)) x inRelease("3.0.1")) -> Feat("SecurityPatch")
    )

    // Print the current feature set release plan with release ranges by effective date
    println("Current feature release plan:")
    println(currentFeatureReleasePlan)

    // Extend the feature release plan with information about region-specific deployments.
    // (A deployment can be roughly thought of as a release to a specific environment)
    val regionalDeploymentPlan = DataMulti[
      Region, // a deployment can be to multiple regions
      Domain.In4D[
        LocalDateTime, // when the release plan included (or maybe pulled) the region/environment
        LocalDate, // effective period when the release was actually deployed (pro- or retrospective of the plan)
        Environment, // Deployment environments in this plan/effective period
        Release // Product releases in this plan/effective period
      ]
    ]()

    def currentRegionalDeploymentPlan(using
      CurrentDateTime
    ): DataMulti[Region, Domain.In3D[LocalDate, Environment, Release]] =
      regionalDeploymentPlan.getByHeadDimension(now)

    // Samples

    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(july(26)) x availableIn(Development) x inRelease(2.0)) -> NorthAmerica
    )
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(july(28)) x availableIn(Development) x inRelease(2.0)) -> APAC
    )

    /**
      * ...complex scenarios like "...available in Production for North America from Version 2.0 onwards, known as of
      * today."
      */
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(august(1)) x availableIn(Production) x inRelease(2.0)) -> NorthAmerica
    )

    // Release 2.0 deployed to Staging in EMEA, known mid-July
    regionalDeploymentPlan.addOne(
      (plannedOn(july(15)) x effectiveOn(july(25)) x availableIn(Staging) x inRelease(2.0)) -> EMEA
    )
    // Release 2.0 deployed to Staging in North America, known late July, effective same as Production for NA
    regionalDeploymentPlan.addOne(
      (plannedOn(july(30)) x effectiveOn(august(1)) x availableIn(Staging) x inRelease(2.0)) -> NorthAmerica
    )
    // Release 1.0 deployed to Production in APAC, known and effective from very early July
    regionalDeploymentPlan.addOne(
      (plannedOn(july(1)) x effectiveOn(july(1)) x availableIn(Production) x inRelease(1.0)) -> APAC
    )
    // Release 3.0 is planned for Development in all regions, known now, effective late August
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(august(20)) x availableIn(Development) x inRelease(3.0)) -> NorthAmerica
    )
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(august(20)) x availableIn(Development) x inRelease(3.0)) -> EMEA
    )
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(august(20)) x availableIn(Development) x inRelease(3.0)) -> APAC
    )

    // Release 2.0 is pulled from Production in APAC (retroactively), known mid-August, effective from Aug 10
    // (This would remove the previous entry for Production/APAC for 2.0 from Aug 10 onwards if it existed, or just
    // indicate its end)
    regionalDeploymentPlan.removeOne(
      (plannedOn(august(15)) x effectiveOn(august(10)) x availableIn(Production) x inRelease(2.0)) -> APAC
    )
    // Release 3.0.1 for North America Production, known now, effective early Sept
    regionalDeploymentPlan.addOne(
      (plannedNow x effectiveOn(sept(5)) x availableIn(Production) x inRelease("3.0.1")) -> NorthAmerica
    )

    // print region sets with environment and release ranges by effective date
    println("\nCurrent regional deployment plan:")
    println(currentRegionalDeploymentPlan)

    /**
      * DataVersioned for Release Planning & Approvals: The DataVersioned structure is perfect for managing the
      * "planned" versus "actual" or "approved" versus "unapproved" state of features.
      *
      * ◦ New features might be added to a "future" or "unapproved" version.
      *
      * ◦ As releases are finalized, these "unapproved" features (at maxValue in the hidden version dimension) can be
      * "approved" to become part of a specific release version, which then becomes the "current" version.
      *
      * ◦ This would allow users to ask, "What features are planned for our next major release (unapproved data)?"
      * versus "What features are currently live in production (approved/current version data)?".
      */
    val releasePlan = DataVersioned[
      Feat, // what features are approved (or pending approval) for deployment in what releases/environments
      Domain.In2D[
        Environment, // Deployment environments
        Release // Product releases
      ]
    ]()

    def proposeRelease(featureRelease: ValidData[Feat, Domain.In2D[Environment, Release]]): Unit =
      releasePlan.set(featureRelease)(using Unapproved)

    def approveRelease(featureRelease: ValidData[Feat, Domain.In2D[Environment, Release]]): Boolean =
      releasePlan.approve(featureRelease)

    def releasePlanFor(environment: Environment): DataVersioned[Feat, Domain.In1D[Release]] =
      releasePlan.getByHeadDimension(environment)

    val developA = (availableIn(Development) x inRelease(2.0)) -> Feat("A")
    proposeRelease(developA)
    println(s"\nRelease plan before feature A development approval:\n$releasePlan")
    val developmentApproved = approveRelease(developA)
    releasePlan.incrementCurrentVersion()
    println(s"*feature A development approved: $developmentApproved\n$releasePlan")

    val stageA = (availableIn(Staging) x inRelease(2.0)) -> Feat("A")
    proposeRelease(stageA)
    println(s"\nRelease plan before feature A staging approval:\n$releasePlan")
    val stagingApproved = approveRelease(stageA)
    releasePlan.incrementCurrentVersion()
    println(s"*feature A staging approved: $stagingApproved\n$releasePlan")

    val releaseA = (availableIn(Production) x inRelease(2.0)) -> Feat("A")
    proposeRelease(releaseA)
    val unapproved = releasePlan.getSelectedData(using Unapproved)
    val approved = releasePlan.getSelectedData(using Current)
    println(s"\nDiff actions from approved to unapproved:")
    unapproved
      .diffActionsFrom(approved)
      .foreach: action =>
        println(s" - $action") //  prints - Update({[Development..Production], [v2.0..+∞)} -> Feat(A))

    println("\nRelease plan before feature A production approval:\n$releasePlan")

    println("\n[before production approval] What features are planned for our next major release (+unapproved data)?")
    println(releasePlanFor(Production).getSelectedData(using Unapproved))

    println("\n[before production approval] What features are currently live in production (approved/current data)?")
    println(releasePlanFor(Production).getSelectedData(using Current))

    val releaseApproved = approveRelease(releaseA)
    releasePlan.incrementCurrentVersion()
    println(s"*feature A release approved: $releaseApproved\n$releasePlan")

    // Feat B for Release 2.0, initially planned for Development (unapproved)
    val developB = (availableIn(Development) x inRelease(2.0)) -> Feat("B")
    proposeRelease(developB)
    // Feat B for Release 2.0 then gets approved for Development
    approveRelease(developB)

    // Feat C for Release 2.0, planned for Development and Staging (unapproved)
    val developC = (availableIn(Development) x inRelease(2.0)) -> Feat("C")
    proposeRelease(developC)
    val stageC = (availableIn(Staging) x inRelease(2.0)) -> Feat("C")
    proposeRelease(stageC)
    // Feat C is approved for Development but not yet Staging
    approveRelease(developC)

    // A new feature, Feat D, is introduced for Release 3.0 in Development (unapproved)
    val developD = (availableIn(Development) x inRelease(3.0)) -> Feat("D")
    proposeRelease(developD)
    // Feat D is also planned for Staging in Release 3.0 (unapproved)
    val stageD = (availableIn(Staging) x inRelease(3.0)) -> Feat("D")
    proposeRelease(stageD)
    // Feat D is then approved for Development in Release 3.0
    approveRelease(developD)

    // Simulate an unplanned hotfix feature, Feat E, for Release 3.0.1 directly into Production (unapproved first)
    val prodE = (availableInOnly(Production) x inRelease("3.0.1")) -> Feat("E")
    proposeRelease(prodE)
    // And quickly approved
    approveRelease(prodE)

    // Feat F for Release 3.0 is planned for Production (unapproved)
    val prodF = (availableIn(Production) x inRelease(3.0)) -> Feat("F")
    proposeRelease(prodF)

    println(s"\nversion timestamps: ${releasePlan.getVersionTimestamps.toList}")

    println("\n[after production approval] What features are planned for our next major release (+unapproved data)?")
    println(releasePlanFor(Production).getSelectedData(using Unapproved))

    println("\n[after production approval] What features are currently live in production (approved/current data)?")
    println(releasePlanFor(Production).getSelectedData(using Current))

    println(s"\n--- Release Plan (with unapproved and approved data) ---\n$releasePlan")

    println("\n--- Queries on `releasePlan` with new data ---")
    println("[current version] What features are currently approved for Release 2.0 in Production?")
    println(releasePlanFor(Production).getAt(Release(2.0))(using Current))

    println("[unapproved version] What features are *planned* for Release 3.0 in Production?")
    // Querying Production environment specifically for Release 3.0's unapproved state [S68]
    println(releasePlanFor(Production).getAt(Release(3.0))(using Unapproved))

    println("[current version] What features are currently approved for Release 3.0 in Development?")
    println(releasePlanFor(Development).getAt(Release(3.0))(using Current))

    /**
      * Extensive Use of values and intervals(value):
      *
      * productSystem.valuesOne: To get a complete list of all unique features that have ever existed across all
      * versions, environments, and regions. This is useful for feature catalogs or audit trails.
      */
    def formatPlan[S, T](source: S, extractValues: S => Iterable[T], format: T => String) =
      extractValues(source).toList.map(format).sorted.mkString(" | ")
    def formatIterable[T](format: T => String)(source: Iterable[T]): String =
      source.toList.map(format).sorted.mkString("/")

    println(s"\nAll features: ${formatPlan(featureReleasePlan, _.valuesOne, _.id)}")
    println(s"All feature sets: ${formatPlan(featureReleasePlan, _.values, formatIterable(_.id))}")
    println(s"All deployment regions: ${formatPlan(regionalDeploymentPlan, _.valuesOne, _.toString)}")
    println(s"All deployment regions sets: ${formatPlan(regionalDeploymentPlan, _.values, formatIterable(_.toString))}")

    /**
      * productSystem.intervalsOne(Feat("A")): Crucially, this method (which we discussed adding examples for) would
      * allow direct queries like: "In which product versions was Feat('A') available?".
      */
    println(s"\nFeature A releases: ${currentFeatureReleasePlan.intervalsOne(Feat("A"))}")
    println(s"NorthAmerica regional releases: ${currentRegionalDeploymentPlan.intervalsOne(NorthAmerica)}")

    /**
      * "In which environments did Feat('B') get deployed?"
      */
    println(s"\nFeature B release environments/regions: ${releasePlan.intervals(Feat("B"))}")

    println("Release 2.0 regional deployments:")
    val regionsByEnvironment: DataMulti[Region, Domain.In1D[Environment]] =
      currentRegionalDeploymentPlan
        .getByHeadDimension(today) // effective today
        .getByDimension(1, Release(2.0)) // for v2.0
    regionsByEnvironment.getAll.foreach:
      case ValidData(regions, environments) =>
        println(s" - in region(s) ${regions.mkString("/")} - environment(s): $environments")

    /**
      * "Across what time periods and environments was the specific combination {Feat('A'), Feat('B')} active?" This
      * goes beyond individual features to sets of features, leveraging DataMulti's design.
      */
    val comboReleased: Option[(LocalDate, Release)] = currentFeatureReleasePlan.toImmutable
      .filter(d => (d.value contains Feat("A")) && (d.value contains Feat("B")))
      .getAll
      .map(_.interval)
      .collectFirst: // gets the earliest effective date and release for that feature combination
        case Interval1D(Point(starting), _) x_: Interval1D(Point(release), _) => (starting, release)

    print(s"\nFeature A/B combination was ")
    comboReleased match
      case None => println(s"never released")
      case Some((effectiveDate, release)) =>
        println(s"initially released on $effectiveDate in $release - regional deployment history:")
        val subsetData: DataMulti[Region, Domain.In2D[LocalDate, Environment]] =
          currentRegionalDeploymentPlan.getByDimension(2, release) // for this release
        subsetData
          .getIntersecting(effectiveOn(effectiveDate) x anyEnvironment) // effective in our timeline
          .foreach:
            case (effective x_: environments) ->: regions =>
              println(s" - effective $effective deployed to ${regions.mkString("/")} in $environments environment(s)")
