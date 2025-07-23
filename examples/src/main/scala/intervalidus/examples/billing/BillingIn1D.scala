package intervalidus.examples.billing

import intervalidus.Interval1D.*
import intervalidus.DiscreteValue.given
import intervalidus.immutable.Data

import java.time.LocalDate
import scala.language.implicitConversions

/**
  * Toy billing system showing how intervalidus data in one dimensions can be used to drive billing logic.
  */
object BillingIn1D extends Billing:
  import BillingData.*
  import Month.*

  /**
    * The dimension represents the effective dates of the tier selections
    */
  private val customerTiers0: Map[CustomerId, Data.In1D[Tier, LocalDate]] = Map.empty.withDefaultValue(Data())

  /**
    * Based on the prior billing cycle, calculate new transactions for all customers and return the current cycle. As a
    * side effect, adds generated transaction to transaction history and completes the new billing cycle.
    *
    * @param priorCustomerTiers
    *   tier assignments in prior billing cycle for all customers
    * @param newCustomerTiers
    *   tier assignments for this billing cycle for all customers
    * @param priorCycle
    *   the prior billing cycle
    * @param index
    *   billing cycle index (for logging)
    * @return
    *   completed billing cycle
    */
  private def billAllCustomers(
    priorCustomerTiers: Map[CustomerId, Data.In1D[Tier, LocalDate]],
    newCustomerTiers: Map[CustomerId, Data.In1D[Tier, LocalDate]]
  )(
    priorCycle: BillingCycle,
    index: Int
  ): BillingCycle =
    val thisCycle = priorCycle.nextCycle
    val newTransactions: Iterable[Transaction] = Customer.all.values.flatMap: customer =>
      billCustomer(
        customer,
        priorTiers = priorCustomerTiers(customer.id),
        newTiers = newCustomerTiers(customer.id),
        priorCycle,
        thisCycle
      )
    // save results
    addTransactions(newTransactions)
    completeBillingCycle(thisCycle)
    println(s"#$index: $thisCycle generated ${pluralize(newTransactions.size, "transaction")}")
    thisCycle

  /**
    * Demonstrates billing over multiple billing cycles with the tier assignments being updated incrementally.
    */
  def main(args: Array[String]): Unit =
    initializeBillingState()

    // On January 15, John signs up for a basic plan starting February 1
    val johnUpdate1 = customerTiers0(Customer.john.id) + (intervalFrom(Feb / 1) -> Tier.basic)
    // Also on January 15, Jane signs up for a premium plan starting March 1
    val janeUpdate1 = customerTiers0(Customer.jane.id) + (intervalFrom(Mar / 1) -> Tier.premium)
    val customerTiers1 = customerTiers0
      .updated(Customer.john.id, johnUpdate1)
      .updated(Customer.jane.id, janeUpdate1)
    val cycle1 = billAllCustomers(customerTiers0, customerTiers1)(currentCycle, 1) // mid-Jan for Feb
    val cycle2 = billAllCustomers(customerTiers1, customerTiers1)(currentCycle, 2) // mid-Feb for Mar
    val cycle3 = billAllCustomers(customerTiers1, customerTiers1)(currentCycle, 3) // mid-Mar for Apr

    // On March 17, John upgrades to premium starting April 1
    val johnUpdate2 = customerTiers1(Customer.john.id) + (intervalFrom(Apr / 1) -> Tier.premium)
    // On March 20, Jane terminates her subscription immediately and does not renew
    val janeUpdate2 = customerTiers1(Customer.jane.id) - intervalFromAfter(Mar / 20)
    val customerTiers2 = customerTiers1
      .updated(Customer.john.id, johnUpdate2)
      .updated(Customer.jane.id, janeUpdate2)
    val cycle4 = billAllCustomers(customerTiers1, customerTiers2)(currentCycle, 4) // mid-Apr for May
    val cycle5 = billAllCustomers(customerTiers2, customerTiers2)(currentCycle, 5) // mid-May for Jun
    val cycle6 = billAllCustomers(customerTiers2, customerTiers2)(currentCycle, 6) // mid-Jun for Jul

    // On June 28, John decides not to renew for July
    val johnUpdate3 = customerTiers2(Customer.john.id) - intervalFromAfter(Jun / 30)
    val customerTiers3 = customerTiers2.updated(Customer.john.id, johnUpdate3)
    val cycle7 = billAllCustomers(customerTiers2, customerTiers3)(currentCycle, 7) // mid-Jun for Jul

    printTransactions()
