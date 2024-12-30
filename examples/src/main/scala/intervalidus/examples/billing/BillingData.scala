package intervalidus.examples.billing

import intervalidus.DiscreteInterval1D
import java.time.LocalDate

/**
  * Supports toy billing systems with core data definition.
  */
object BillingData:
  // types, opaque and otherwise
  type Period = DiscreteInterval1D[LocalDate]
  extension (period: Period) def days: Int = period.points.size

  type Dollars = BigDecimal
  extension (m: Dollars) def dollarFormat = f"$$$m%2.2f"

  opaque type TierId = Int
  object TierId:
    def apply(i: Int): TierId = i

  opaque type CustomerId = Int
  object CustomerId:
    def apply(i: Int): CustomerId = i

  // Month enum, which is a tiny bit more readable/useful than java.time.Month
  enum Month:
    case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

    def /(day: Int): LocalDate = LocalDate.of(Month.defaultYear, ordinal + 1, day)

  object Month:
    private val defaultYear = 2025
    extension (d: LocalDate)
      def /(year: Int): LocalDate = LocalDate.of(year, d.getMonthValue, d.getDayOfMonth)
      def slashFormat: String = s"[${Month.fromOrdinal(d.getMonthValue - 1)} / ${d.getDayOfMonth} / ${d.getYear}]"

  // Tiers
  case class Tier(id: TierId, description: String, dailyRate: Dollars):
    def withId: (TierId, Tier) = id -> this
    override def toString: String = s"[$description ($id)]"

  object Tier:
    // Test tiers
    val basic: Tier = Tier(TierId(1), "basic", 1.0)
    val premium: Tier = Tier(TierId(2), "premium", 1.5)
    val all: Map[TierId, Tier] = Map.from(Seq(basic.withId, premium.withId))

  // Customers
  case class Customer(id: CustomerId, lastName: String, firstName: String):
    def withId: (CustomerId, Customer) = id -> this
    override def toString: String = s"[$firstName $lastName ($id)]"

  object Customer:
    // Test customers
    val john: Customer = Customer(CustomerId(1), "Doe", "John")
    val jane: Customer = Customer(CustomerId(2), "Dae", "Jane")
    val all: Map[CustomerId, Customer] = Map.from(Seq(john.withId, jane.withId))

  case class Transaction(
    date: LocalDate,
    customer: Customer,
    tier: Tier,
    period: Period,
    amount: Dollars,
    remark: String
  ):
    import Month.*
    def withId: (CustomerId, Transaction) = customer.id -> this
    override def toString: String = s"${date.slashFormat}: ${amount.dollarFormat} - $remark"

  object Transaction:
    // order transactions by date, amount, and remark
    given (using dateOrder: Ordering[LocalDate]): Ordering[Transaction] with
      override def compare(x: Transaction, y: Transaction): Int =
        val primary = dateOrder.compare(x.date, y.date)
        if primary != 0 then primary
        else
          val secondary = x.amount.compareTo(y.amount)
          if secondary != 0 then secondary else x.remark.compareTo(y.remark)
