package intervalidus.mutable

import intervalidus.*
import intervalidus.Interval1D.{intervalFrom, unbounded}
import intervalidus.VariableBase.{Instant1D, given}

import scala.language.implicitConversions

/**
  * A value that varies in time.
  */
object Variable extends VariableObjectBase:
  override def apply[T](initialValue: T): Variable[T] = new Variable(Data.of(initialValue))

  override def fromHistory[T](history: Iterable[ValidData[T, Instant1D]]): Variable[T] = new Variable(Data(history))

/**
  * A value that varies in time.
  *
  * @tparam T
  *   the value type
  */
class Variable[T] private (override protected val underlyingData: Data[T, Instant1D]) extends VariableBase[T]:

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
