package intervalidus

import java.time.LocalDateTime

/**
  * Type class for getting the current datetime.
  */
trait CurrentDateTime:
  def now(): LocalDateTime

object CurrentDateTime:
  /**
    * Default behavior, using the actual clock.
    */
  given CurrentDateTime with
    override def now(): LocalDateTime = LocalDateTime.now()

  /**
    * Simulated behavior, for testing.
    */
  def simulated(simulatedNow: LocalDateTime): CurrentDateTime =
    () => simulatedNow
