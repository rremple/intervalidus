package intervalidus

import intervalidus.Domain1D.{Top, Point, domain}

import java.time.{Duration, Instant}

/**
  * Common definitions for values that vary in time.
  */
object VariableBase:
  /**
    * Type class for instants as discrete values by nanosecond (weird, but works in this context)
    */
  given InstantDiscreteValue: DiscreteValue[Instant] with
    override def compare(lhs: Instant, rhs: Instant): Int = lhs.compareTo(rhs)

    // hashing uses millis, so this prevents long overflow when hashing
    // all instants above and below these values will collide respectively
    private val minMilliInstant = Instant.ofEpochMilli(Long.MinValue)
    private val maxMilliInstant = Instant.ofEpochMilli(Long.MaxValue)

    override def orderedHashOf(x: Instant): Double = x match
      case i if i.isBefore(minMilliInstant) => Long.MinValue.toDouble
      case i if i.isAfter(maxMilliInstant)  => Long.MaxValue.toDouble
      case i                                => i.toEpochMilli.toDouble

    override val maxValue: Instant = Instant.MAX

    override val minValue: Instant = Instant.MIN

    override def successorOf(x: Instant): Option[Instant] =
      if x.equals(maxValue) then None else Some(x.plusNanos(1))

    override def predecessorOf(x: Instant): Option[Instant] =
      if x.equals(minValue) then None else Some(x.minusNanos(1))

import VariableBase.given

/**
  * Base for companions of values that vary in time.
  */
trait VariableObjectBase:
  /**
    * @param initialValue
    *   initial value of this variable
    * @tparam T
    *   the value type
    * @return
    *   a new variable
    */
  def apply[T](initialValue: T): VariableBase[T]

  /**
    * @param history
    *   historical values of this variable
    * @tparam T
    *   the value type
    * @return
    *   a new variable
    */
  def fromHistory[T](history: Iterable[ValidData.In1D[T, Instant]]): VariableBase[T]

/**
  * A value that varies in time.
  *
  * @tparam T
  *   the value type
  */
trait VariableBase[T] extends (Domain.In1D[Instant] => T):

  // could be mutable or immutable
  protected def underlyingData: DimensionalBase.In1D[T, Instant]

  // from Object - print latest
  override def toString: String = get.toString

  // from Function - delegate to underlyingData
  override def apply(key: Domain.In1D[Instant]): T = underlyingData(key)

  // on conflict, add 10 ns (0.00001 ms) to make the instant unique (ouch!)
  private val fixedNanoBump = 10

  // gives a unique instant with respect to the last update
  protected def withUniqueTime[U](using time: CurrentInstant)(f: Domain1D[Instant] => U): U =
    val now = time.now()
    lastChange match
      // conflict -- rare, but could happen when calling in rapid succession or when using CurrentInstant.simulated
      case Some(prior) if prior.equals(now) =>
        Thread.sleep(0, fixedNanoBump)
        f(domain(now.plusNanos(fixedNanoBump)))
      // conflict, out of order -- super-rare, but could happen when using CurrentInstant.simulated
      case Some(prior) if prior.isAfter(now) =>
        val fullNanoBump = Duration.between(now, prior).toNanos + fixedNanoBump
        val milliBump = fullNanoBump / 1_000_000
        val nanoBump = fullNanoBump % 1_000_000
        Thread.sleep(milliBump, nanoBump.toInt)
        f(domain(now.plusNanos(fullNanoBump)))
      // no conflict or no prior change -- normal
      case _ => f(domain(now))

  private def priorTo(d: Instant): Domain.In1D[Instant] = domain(d).leftAdjacent.tupled

  // returns the prior update data as if the last change did not happen
  protected def unsetPriorData: Option[ValidData.In1D[T, Instant]] = for
    current <- lastChange
    data <- underlyingData.getDataAt(priorTo(current))
  yield data.interval.toTop -> data.value

  /**
    * @return
    *   all historical values as an intervalidus immutable data structure.
    */
  def history: immutable.Data.In1D[T, Instant]

  /**
    * Get the last change instant.
    *
    * @return
    *   Some instant if the value was changed since it was initially set, None if it was never changed.
    */
  def lastChange: Option[Instant] = underlyingData.getDataAt(Top.tupled) match
    case Some(ValidData(_, Interval(Point(time) *: EmptyTuple, _))) => Some(time)
    case _                                                          => None // hasn't changed yet

  /**
    * @return
    *   the most recent value.
    */
  def get: T = underlyingData(Top.tupled)

  /**
    * Get the prior value.
    *
    * @return
    *   a value from the past, if there is one.
    */
  def getPrior: Option[T] = lastChange.flatMap(c => underlyingData.getAt(priorTo(c)))

  /**
    * Get a past value.
    *
    * @param past
    *   when the value was valid.
    * @return
    *   a value from the past.
    */
  def getAt(past: Instant): T = underlyingData(domain(past).tupled)
