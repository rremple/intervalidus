package intervalidus

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalDateTime

class CurrentDateTimeTest extends AnyFunSuite:
  test("Default"):
    val nowBase = LocalDateTime.now()
    val nowSim = summon[CurrentDateTime].now()
    assert(nowBase == nowSim || (nowBase isBefore nowSim))

  test("Non-default"):
    val simDate = LocalDateTime.of(2025, 8, 1, 8, 25)
    given CurrentDateTime = CurrentDateTime.simulated(simDate)
    val nowSim = summon[CurrentDateTime].now()
    assert(nowSim == simDate)

    def checkDate(d: LocalDateTime)(using dateTime: CurrentDateTime) =
      assert(d == dateTime.now())

    checkDate(simDate)(using CurrentDateTime.simulated(simDate))
