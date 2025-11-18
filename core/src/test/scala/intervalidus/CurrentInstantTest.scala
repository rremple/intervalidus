package intervalidus

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.time.{Instant, LocalDateTime, ZoneOffset}

class CurrentInstantTest extends AnyFunSuite:
  test("Default"):
    val nowBase = Instant.now()
    val nowSim = summon[CurrentInstant].now()
    assert(nowBase == nowSim || (nowBase isBefore nowSim))

  test("Non-default"):
    val simDate = LocalDateTime.of(2025, 11, 18, 12, 15).toInstant(ZoneOffset.UTC)
    given CurrentInstant = CurrentInstant.simulated(simDate)
    val nowSim = summon[CurrentInstant].now()
    assert(nowSim == simDate)

    def checkDate(d: Instant)(using dateTime: CurrentInstant) =
      assert(d == dateTime.now())

    checkDate(simDate)(using CurrentInstant.simulated(simDate))
