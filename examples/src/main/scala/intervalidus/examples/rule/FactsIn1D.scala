package intervalidus.examples.rule

import intervalidus.DiscreteValue.given
import intervalidus.immutable.Data
import intervalidus.Interval1D.*
import intervalidus.tinyrule.*

import java.time.LocalDate
import java.util.UUID
import scala.language.implicitConversions

object FactsIn1D:
  import Rule.always
  import FactFilter.{augmentWhenAbsent, redact}

  /*
   * Categorize blood pressure
   */
  case class BloodPressure(bloodPressureCategory: String, hypertension: Boolean = true)

  object BloodPressure:
    val whenCrisis: Rule = ("systolic" attributeGreaterThan 180.0) or ("diastolic" attributeGreaterThan 120.0)
    val whenStage2: Rule = ("systolic" attributeGreaterThan 140.0) or ("diastolic" attributeGreaterThan 90.0)
    val whenStage1: Rule = ("systolic" attributeGreaterThan 130.0) or ("diastolic" attributeGreaterThan 80.0)
    val whenElevated: Rule = "systolic" attributeGreaterThan 120.0

    val categorize: FactFilter =
      whenCrisis.augmentWhenAbsent(Fact.from(BloodPressure("crisis"))) and
        whenStage2.augmentWhenAbsent(Fact.from(BloodPressure("stage2"))) and
        whenStage1.augmentWhenAbsent(Fact.from(BloodPressure("stage1"))) and
        whenElevated.augmentWhenAbsent(Fact.from(BloodPressure("elevated", hypertension = false))) and
        always.augmentWhenAbsent(Fact.from(BloodPressure("normal", hypertension = false))) // default

  /*
   * Categorize age, where the rule definition depends on the notion of a "current" date
   */
  case class Age(ageBracket: String, minor: Boolean = false)

  object Age:
    def categorize(currentDate: LocalDate): FactFilter =
      def whenYoungerThan(ageInYears: Int, ageAttributes: Age): FactFilter =
        ("birthdate" attributeGreaterThan currentDate.minusYears(ageInYears))
          .augmentWhenAbsent(Fact.from(ageAttributes))

      whenYoungerThan(18, Age("under 18", minor = true)) and
        whenYoungerThan(25, Age("18-24")) and
        whenYoungerThan(35, Age("25-34")) and
        whenYoungerThan(45, Age("35-44")) and
        whenYoungerThan(55, Age("45-54")) and
        whenYoungerThan(65, Age("55-64")) and
        always.augmentWhenAbsent(Fact.from(Age("65 and over"))) // default

  /*
   * Remove specifics
   */
  val redactSpecifics = always.redact("firstName", "lastName", "birthdate", "diastolic", "systolic", "weight")

  /*
   * Test data: sources (records) and results (after applying fact filters)
   */
  enum Gender:
    case M, F, Other

  case class SourcePatientRecord(
    patientId: String,
    recorded: LocalDate,
    firstName: String,
    lastName: String,
    birthdate: LocalDate,
    gender: Gender, // will automatically get represented as a StringAttribute
    trialGroup: Int,
    diastolic: Double,
    systolic: Double,
    weight: Double
  )

  case class ResultPatientRecord(
    patientId: String,
    recorded: LocalDate,
    gender: String,
    trialGroup: Int,

    // redacted attributes
    // firstName: String,
    // lastName: String,
    // birthdate: LocalDate,
    // diastolic: Double,
    // systolic: Double,
    // weight: Double,

    // augmented attributes
    bloodPressureCategory: String,
    hypertension: Boolean,
    ageBracket: String,
    minor: Boolean
  )

  /*
   * Represent timebound data as data valid in discrete date intervals
   */
  type Timebound[V] = Data.In1D[V, LocalDate]

  /*
   * Test data progress, where the value of each attribute can vary over time (apart from the id)
   */
  case class PatientProgress(
    patientId: String,
    gender: Timebound[String],
    trialGroup: Timebound[Int],
    ageBracket: Timebound[String],
    minor: Timebound[Boolean],
    bloodPressureCategory: Timebound[String],
    hypertension: Timebound[Boolean]
  ):
    override def toString: String = s"PatientProgress for $patientId:\n" +
      s"gender:\n$gender\n" +
      s"trialGroup:\n$trialGroup\n" +
      s"ageBracket:\n$ageBracket\n" +
      s"minor:\n$minor\n" +
      s"bloodPressureCategory:\n$bloodPressureCategory\n" +
      s"hypertension:\n$hypertension\n"

  /*
   * Process records for a particular day (changes in age, BP, etc.) and update the progress based on updated results
   */
  def updateProgress(
    startDate: LocalDate
  )(
    baselineProgress: Map[String, PatientProgress],
    daysFromStart: Int
  ): Map[String, PatientProgress] =
    val recordDate = startDate.plusDays(daysFromStart)
    val resultFilter: FactFilter = BloodPressure.categorize and Age.categorize(recordDate) and redactSpecifics

    /*
     * John starts as under 55, but turns 55 along the way
     * His hypertension worsens over time
     */
    val johnRecord = SourcePatientRecord(
      patientId = "12345",
      recorded = recordDate,
      firstName = "John",
      lastName = "Doe",
      birthdate = startDate.minusYears(55).plusDays(2), // under 55
      gender = Gender.M,
      trialGroup = if daysFromStart < 10 then 1 else 2, // changes group mid-program
      systolic = 120 + daysFromStart, // worsens over time
      diastolic = 80 + daysFromStart,
      weight = 150
    )
    /*
     * Jane starts as under 18 (a minor), but turns 18 along the way
     * Her hypertension improves over time
     */
    val janeRecord = SourcePatientRecord(
      patientId = "67890",
      recorded = recordDate,
      firstName = "Jane",
      lastName = "Dae",
      birthdate = startDate.minusYears(18).plusDays(5), // minor
      gender = Gender.F,
      trialGroup = 2,
      systolic = 150 - daysFromStart * 2, // improves over time
      diastolic = 90 - daysFromStart * 2,
      weight = 110
    )
    val records: Set[Fact] = Set(Fact.from(johnRecord), Fact.from(janeRecord))

    /*
     * Get results by applying the result filter
     */
    val results: Set[ResultPatientRecord] = resultFilter(records).map(_.to[ResultPatientRecord])

    /*
     * Update baseline progress with new results.
     */
    results.foldLeft(baselineProgress): (progress, updatedRecord) =>
      val patientId = updatedRecord.patientId // can't change
      val fromRecorded = intervalFrom(updatedRecord.recorded)
      val prior = progress(patientId)
      val latest = prior.copy(
        gender = prior.gender + (fromRecorded -> updatedRecord.gender),
        trialGroup = prior.trialGroup + (fromRecorded -> updatedRecord.trialGroup),
        bloodPressureCategory = prior.bloodPressureCategory + (fromRecorded -> updatedRecord.bloodPressureCategory),
        hypertension = prior.hypertension + (fromRecorded -> updatedRecord.hypertension),
        ageBracket = prior.ageBracket + (fromRecorded -> updatedRecord.ageBracket),
        minor = prior.minor + (fromRecorded -> updatedRecord.minor)
      )
      progress.updated(patientId, latest)

  def main(args: Array[String]): Unit =
    val initialProgress = Map[String, PatientProgress]().withDefault: id =>
      PatientProgress(id, Data(), Data(), Data(), Data(), Data(), Data())

    // run through many days of turning records into results into progress
    val patientProgressById = (0 until 20).foldLeft(initialProgress)(updateProgress(LocalDate.now()))

    patientProgressById.values.foreach(println)
