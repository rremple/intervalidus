package intervalidus.examples.billing

import intervalidus.*
import intervalidus.Interval1D.*
import intervalidus.collection.mutable.MultiMapSorted
import intervalidus.immutable.DataIn1D

import java.time.LocalDate
import scala.collection.mutable
import scala.language.implicitConversions
import scala.math.Ordering.Implicits.infixOrderingOps

/**
  * Toy billing system showing how intervalidus data in one or two dimensions can be used to drive billing logic. This
  * trait includes logic common to both.
  */
trait Billing:
  import BillingData.*
  import Month.*

  /**
    * Defines the key dates of a billing cycle.
    * @param runDate
    *   when did the billing cycle run, i.e., when were the data used in billing known
    * @param billToDate
    *   through what date were bills/refunds generated
    */
  case class BillingCycle(runDate: LocalDate, billToDate: LocalDate):
    override def toString: String =
      s"billing run on ${runDate.slashFormat} for customers effective through ${billToDate.slashFormat}"
    def nextCycle: BillingCycle =
      val nextRunDate = runDate.plusMonths(1) // Jan / 15
      val nextBillToDate = LocalDate // end of the following month, e.g., if billToDate is Jan / 31...
        .of(billToDate.getYear, billToDate.getMonthValue, 1) // back to beginning of month, e.g., Jan / 1
        .plusMonths(2) // and forward, e.g., Mar / 1
        .minusDays(1) // and back again, e.g., Feb / 28
      BillingCycle(nextRunDate, nextBillToDate)

  /**
    * Evolving state of billing cycles (simulating external persistence). Maps run dates to corresponding bill-to dates.
    */
  private val billingCycles: mutable.Map[LocalDate, LocalDate] = mutable.Map.empty

  /**
    * Get current billing cycle.
    * @return
    *   billing cycle from history with the largest run date
    */
  def currentCycle: BillingCycle =
    val priorRunDate = billingCycles.keys.max
    BillingCycle(priorRunDate, billingCycles(priorRunDate))

  /**
    * Updates billing cycle history with the provided billing cycle.
    * @param billingCycle
    *   added to billing cycle history
    */
  def completeBillingCycle(billingCycle: BillingCycle): Unit =
    billingCycles.addOne(billingCycle.runDate -> billingCycle.billToDate)

  /**
    * Evolving state of customer transactions (simulating external persistence). Maps customer ids to a collection of
    * transactions (ordered primarily by date and secondarily by other stuff).
    */
  private val customerTransactions: MultiMapSorted[CustomerId, Transaction] = MultiMapSorted()

  /**
    * Updates customer transaction history with the provided transactions.
    * @param transactions
    *   added to customer transaction history
    */
  def addTransactions(transactions: Iterable[Transaction]): Unit =
    customerTransactions.addAll(transactions.map(_.withId))

  /**
    * Initial state. Pretend we ran prospective January billing (for customers effective through the end of January)
    * back in mid-December for customers effective through the end of January. Next billing cycle will be prospective
    * February billing (for customers effective through the end of February) based on what is known mid-January.
    */
  def initializeBillingState(): Unit =
    billingCycles.clear()
    customerTransactions.clear()
    completeBillingCycle(BillingCycle(Dec / 15 / 2024, (Feb / 1).minusDays(1)))

  /**
    * For printing count of nouns that may be plural.
    * @param count
    *   how many nouns
    * @param noun
    *   noun when not plural
    * @param plural
    *   function applied to noun to make it plural
    * @return
    *   string representation of the count and the correct pluralization of the noun
    */
  def pluralize(count: Int, noun: String, plural: String => String = _ + "s"): String =
    if count == 1 then s"$count $noun" else s"$count ${plural(noun)}"

  /**
    * Print all saved results for each customer
    */
  def printTransactions(): Unit =
    Customer.all.foreach: (id, customer) =>
      println(s"transactions for $customer:")
      val transactions = customerTransactions.get(id)
      if transactions.isEmpty then println(" - <none>")
      else
        transactions.foreach: transaction =>
          println(s" - $transaction")

  /**
    * Bills a customer given the differences between their prior and new tier assignments.
    *
    * @param customer
    *   customer to be billed
    * @param priorTiers
    *   tier assignments in prior billing cycle
    * @param newTiers
    *   tier assignments for this billing cycle
    * @param priorCycle
    *   prior billing cycle
    * @param thisCycle
    *   this billing cycle
    * @return
    *   transactions generated in this billing cycle
    */
  def billCustomer(
    customer: Customer,
    priorTiers: DataIn1D[Tier, LocalDate],
    newTiers: DataIn1D[Tier, LocalDate],
    priorCycle: BillingCycle,
    thisCycle: BillingCycle
  ): Iterable[Transaction] =
    def transactions(tier: Tier, billingPeriod: Period, refund: Boolean = false): Iterable[Transaction] =
      val (amountSign, transactionType) = if refund then (-1, "refund") else (1, "bill")
      for
        ValidData1D(dailyRate, ratePeriod) <- tier.dailyRates.getIntersecting(billingPeriod)
        period <- billingPeriod.intersectionWith(ratePeriod)
      yield
        val remark = s"$transactionType for $period at the ${tier.description} ${dailyRate.dollarFormat}/day rate"
        Transaction(thisCycle.runDate, customer, tier, period, dailyRate * period.days * amountSign, remark)

    import DiffAction1D.*
    val newTiersTruncated = newTiers.remove(intervalFromAfter(thisCycle.billToDate))
    val priorTiersTruncated = priorTiers.remove(intervalFromAfter(priorCycle.billToDate))
    val diffActions = newTiersTruncated.diffActionsFrom(priorTiersTruncated)

    diffActions.flatMap:
      case Create(ValidData1D(newTier, newInterval)) =>
        transactions(newTier, newInterval)

      case Update(ValidData1D(newTier, newInterval)) =>
        priorTiersTruncated.getDataAt(newInterval.start) match
          case Some(ValidData1D(priorTier, priorInterval)) if priorTier == newTier => // different interval
            if priorInterval.end < newInterval.end then transactions(newTier, newInterval.fromAfter(priorInterval.end))
            else transactions(priorTier, priorInterval.fromAfter(newInterval.end), refund = true)
          case Some(ValidData1D(priorTier, priorInterval)) => // different tier
            transactions(priorTier, priorInterval, refund = true) ++ transactions(newTier, newInterval)
          case None => // should never happen
            println(
              s"Update action, but updated data at ${newInterval.start} could not be found in $priorTiersTruncated"
            )
            Seq.empty

      case Delete(start) =>
        priorTiersTruncated.getDataAt(start) match
          case Some(ValidData1D(priorTier, priorInterval)) =>
            transactions(priorTier, priorInterval, refund = true)
          case None => // should never happen
            println(s"Delete action, but deleted data at $start could not be found in $priorTiersTruncated")
            Seq.empty
