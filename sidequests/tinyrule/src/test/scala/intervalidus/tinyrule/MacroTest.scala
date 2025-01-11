package intervalidus.tinyrule

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class MacroTest extends AnyFunSuite with Matchers:

  test("Get stringy types"):
    val set = "scala.collection.immutable.Set"
    val option = "scala.Option"
    val bool = "scala.Boolean"
    val int = "scala.Int"
    val double = "scala.Double"
    val javaLangString = "java.lang.String" // when at the top-level (for some reason)
    val string = "scala.Predef.String" // otherwise this, the alias for java.lang.String
    val date = "java.time.LocalDate"

    type tupledSingleTypes = (Boolean, Int, Double, String, LocalDate)
    type tupledOptionTypes = (Option[Boolean], Option[Int], Option[Double], Option[String], Option[LocalDate])
    type tupledSetTypes = (Set[Boolean], Set[Int], Set[Double], Set[String], Set[LocalDate])

    val tupledSingleTypeNames = Macro.summonTypeNames[tupledSingleTypes].mkString(", ")
    val tupledOptionTypeNames = Macro.summonTypeNames[tupledOptionTypes].mkString(", ")
    val tupledSetTypeNames = Macro.summonTypeNames[tupledSetTypes].mkString(", ")

    tupledSingleTypeNames shouldBe s"$bool, $int, $double, $javaLangString, $date"
    tupledOptionTypeNames shouldBe s"$option[$bool], $option[$int], $option[$double], $option[$string], $option[$date]"
    tupledSetTypeNames shouldBe s"$set[$bool], $set[$int], $set[$double], $set[$string], $set[$date]"
