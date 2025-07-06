package intervalidus.examples.billing

import intervalidus.DiscreteValue.given
import intervalidus.*
import intervalidus.immutable.Data

import java.time.LocalDate
import scala.language.implicitConversions

/**
  * Supports toy billing systems with core data definition.
  */
object BillingData:
  // types, opaque and otherwise
  type Period = Interval1D[LocalDate]
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

  import Month.*
  import Interval1D.intervalFromAfter

  // Tiers, where rates can vary over time
  case class Tier(id: TierId, description: String, dailyRates: Data.In1D[Dollars, LocalDate]):
    def withId: (TierId, Tier) = id -> this
    override def toString: String = s"[$description ($id)]"

  object Tier:
    // Test tiers, where on April 20, the rates go up 20% (basic from $1.00 to $1.20, premium from $1.50 to $1.80)
    private val basicDailyRate = Data.of[Dollars, Domain.In1D[LocalDate]](1.0).set(intervalFromAfter(Apr / 20) -> 1.2)
    val basic: Tier = Tier(TierId(1), "basic", basicDailyRate)
    val premium: Tier = Tier(TierId(2), "premium", basicDailyRate.mapValues(_ * 1.5))
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
    def withId: (CustomerId, Transaction) = customer.id -> this
    override def toString: String = s"${date.slashFormat}: ${amount.dollarFormat} - $remark"

  object Transaction:
    // order transactions by date, period, amount, and remark
    given Ordering[Transaction] with
      override def compare(x: Transaction, y: Transaction): Int =
        def by[T](getFrom: Transaction => T)(using order: Ordering[T]): Option[Int] =
          Some(order.compare(getFrom(x), getFrom(y))).filterNot(_ == 0)
        def byEverything: Int =
          if x == y then 0 else throw Exception(s"Transaction ordering insufficient for $x and $y")
        by(_.date).orElse(by(_.period)).orElse(by(_.amount)).orElse(by(_.remark)).getOrElse(byEverything)
