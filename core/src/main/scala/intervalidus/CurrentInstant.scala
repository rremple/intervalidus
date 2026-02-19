package intervalidus

import java.time.Instant

/**
  * Type class for getting the current instant.
  */
trait CurrentInstant:
  def now(): Instant

/**
  * For getting the actual (default) or a simulated current instant.
  */
object CurrentInstant:
  /**
    * Default behavior, using the actual clock.
    */
  given CurrentInstant with
    override def now(): Instant = Instant.now()

  /**
    * Simulated behavior, for testing.
    */
  def simulated(simulatedNow: Instant): CurrentInstant =
    () => simulatedNow
