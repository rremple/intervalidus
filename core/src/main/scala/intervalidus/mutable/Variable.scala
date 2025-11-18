package intervalidus.mutable

import intervalidus.*
import intervalidus.Interval1D.{intervalFrom, unbounded}
import intervalidus.VariableBase.{Instant1D, given}

import scala.language.implicitConversions

/**
  * A value that varies in time.
  * @param initialValue
  *   initial value of this variable
  * @tparam T
  *   the value type
  */
class Variable[T](initialValue: T) extends VariableBase[T]:

  override protected val underlyingData: Data[T, Instant1D] = Data.of(initialValue)

  override def history: immutable.Data[T, Instant1D] = underlyingData.toImmutable

  /**
    * Set the value, starting now.
    * @param value
    *   new value, starting now.
    */
  def set(value: T)(using CurrentInstant): Unit = withUniqueTime: now =>
    underlyingData.update(intervalFrom(now) -> value)

  /**
    * Undo the last change, if there was one.
    */
  def unset(): Unit = unsetPriorData.foreach(underlyingData.update)

  /**
    * Only keep the current value
    */
  def reset(): Unit = underlyingData.update(unbounded -> get)
