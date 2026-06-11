package intervalidus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Assertion, Assertions}

import scala.util.NotGiven
import scala.compiletime.summonFrom

/**
  * Trait for expressing assertions in tests for type-level proofs, a.k.a., witnesses.
  *
  * For example, if you mix [[ProofMatchers]] into a suite class, you can write an assertion in that suite like this:
  * {{{
  *   assertProven[String <:< Any]("a String is a subtype of Any")
  *   assertNotProven[String =:= Int]("a String is the same as an Int")
  * }}}
  */
trait ProofMatchers extends Assertions:
  inline def assertProven[T](description: String): Assertion = summonFrom:
    case _: T           => succeed
    case _: NotGiven[T] => fail(s"Unable to prove $description")

  inline def assertNotProven[T](description: String): Assertion = summonFrom:
    case _: NotGiven[T] => succeed
    case _: T           => fail(s"Unable to disprove $description")

class ProofMatchersTest extends AnyFunSuite with ProofMatchers:
  test("built-in witnesses work"):
    assertProven[String <:< Any]("a String is a subtype of Any")
    assertNotProven[String =:= Int]("a String is the same as an Int")
