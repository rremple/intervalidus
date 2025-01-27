package intervalidus.examples.billing

import intervalidus.Interval1D.*
import intervalidus.immutable.DataIn2D

import java.time.LocalDate
import scala.language.implicitConversions

/**
  * Toy billing system showing how intervalidus data in two dimensions can be used to drive billing logic.
  */
object BillingIn2D extends Billing:
  import BillingData.*
  import Month.*

  /**
    * The horizontal dimension represents the effective dates of the tiers selections, where the vertical dimension
    * represents when these tier selections were known
    */
  private val customerTiers0: Map[CustomerId, DataIn2D[Tier, LocalDate, LocalDate]] =
    Map.empty.withDefaultValue(DataIn2D())

  /**
    * Based on the prior billing cycle, calculate new transactions for all customers and return the current billing
    * cycle. As a side effect, adds generated transaction to transaction history and completes the new billing cycle.
    *
    * @param customerTiers
    *   history of customer tier assignments (with two dimensions of time)
    * @param priorCycle
    *   prior billing cycle
    * @param index
    *   billing cycle index (for logging)
    * @return
    *   completed billing cycle
    */
  private def billAllCustomers(
    customerTiers: Map[CustomerId, DataIn2D[Tier, LocalDate, LocalDate]]
  )(
    priorCycle: BillingCycle,
    index: Int
  ): BillingCycle =
    val thisCycle = priorCycle.nextCycle
    val newTransactions: Iterable[Transaction] = Customer.all.values.flatMap: customer =>
      val tierHistory = customerTiers(customer.id)
      billCustomer(
        customer,
        priorTiers = tierHistory.getByVerticalIndex(priorCycle.runDate),
        newTiers = tierHistory.getByVerticalIndex(thisCycle.runDate),
        priorCycle,
        thisCycle
      )
    // save results
    addTransactions(newTransactions)
    completeBillingCycle(thisCycle)
    println(s"#$index: $thisCycle generated ${pluralize(newTransactions.size, "transaction")}")
    thisCycle

  /**
    * Demonstrates billing over multiple billing cycles with the tier assignment history being updated incrementally.
    */
  private def oneAtATime(): Unit =
    initializeBillingState()

    // On January 15, John signs up for a basic plan starting February 1
    val johnUpdate1 =
      customerTiers0(Customer.john.id).set((intervalFrom(Feb / 1) x intervalFrom(Jan / 15)) -> Tier.basic)
    // Also on January 15, Jane signs up for a premium plan starting March 1
    val janeUpdate1 =
      customerTiers0(Customer.jane.id).set((intervalFrom(Mar / 1) x intervalFrom(Jan / 15)) -> Tier.premium)
    val customerTiers1 = customerTiers0
      .updated(Customer.john.id, johnUpdate1)
      .updated(Customer.jane.id, janeUpdate1)
    val cycle1 = billAllCustomers(customerTiers1)(currentCycle, 1) // mid-Jan for Feb
    val cycle2 = billAllCustomers(customerTiers1)(currentCycle, 2) // mid-Feb for Mar
    val cycle3 = billAllCustomers(customerTiers1)(currentCycle, 3) // mid-Mar for Apr

    // On March 17, John upgrades to premium starting April 1
    val johnUpdate2 =
      customerTiers1(Customer.john.id).set((intervalFrom(Apr / 1) x intervalFrom(Mar / 17)) -> Tier.premium)
    // On March 20, Jane terminates her subscription immediately and does not renew
    val janeUpdate2 = customerTiers1(Customer.jane.id).remove(intervalFromAfter(Mar / 20) x intervalFrom(Mar / 20))
    val customerTiers2 = customerTiers1
      .updated(Customer.john.id, johnUpdate2)
      .updated(Customer.jane.id, janeUpdate2)
    val cycle4 = billAllCustomers(customerTiers2)(currentCycle, 4) // mid-Apr for May
    val cycle5 = billAllCustomers(customerTiers2)(currentCycle, 5) // mid-May for Jun
    val cycle6 = billAllCustomers(customerTiers2)(currentCycle, 6) // mid-Jun for Jul

    // On June 28, John decides not to renew for July
    val johnUpdate3 = customerTiers2(Customer.john.id).remove(intervalFromAfter(Jun / 30) x intervalFrom(Jun / 28))
    val customerTiers3 = customerTiers2.updated(Customer.john.id, johnUpdate3)
    val cycle7 = billAllCustomers(customerTiers3)(currentCycle, 7) // mid-Jun for Jul

    printTransactions()

  /**
    * Demonstrates billing over multiple billing cycles with the tier assignment history being established up front.
    */
  private def allAtOnce(): Unit =
    initializeBillingState()

    // On January 15, John signs up for a basic plan starting February 1;
    // on March 17, John upgrades to premium starting April 1;
    // on June 28, John decides not to renew for July
    val johnUpdates = customerTiers0(Customer.john.id)
      .set((intervalFrom(Feb / 1) x intervalFrom(Jan / 15)) -> Tier.basic)
      .set((intervalFrom(Apr / 1) x intervalFrom(Mar / 17)) -> Tier.premium)
      .remove(intervalFromAfter(Jun / 30) x intervalFrom(Jun / 28))
    // Also on January 15, Jane signs up for a premium plan starting March 1;
    // on March 20, Jane terminates her subscription immediately and does not renew
    val janeUpdates = customerTiers0(Customer.jane.id)
      .set((intervalFrom(Mar / 1) x intervalFrom(Jan / 15)) -> Tier.premium)
      .remove(intervalFromAfter(Mar / 20) x intervalFrom(Mar / 20))
    val customerTiers = customerTiers0
      .updated(Customer.john.id, johnUpdates)
      .updated(Customer.jane.id, janeUpdates)

    // Calculate new transactions across many monthly billing cycles
    (1 to 7).foldLeft(currentCycle)(billAllCustomers(customerTiers))

    printTransactions()

  /**
    * Runs both the one-at-a-time and all-at-once versions of billing. Both should generate the same transactions.
    */
  def main(args: Array[String]): Unit =
    oneAtATime()
    allAtOnce()
