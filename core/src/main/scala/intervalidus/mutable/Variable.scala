package intervalidus.mutable

import intervalidus.*
import intervalidus.Interval1D.{intervalFrom, unbounded}
import intervalidus.VariableBase.given

import java.time.Instant

/**
  * A value that varies in time.
  */
object Variable extends VariableObjectBase:
  override def apply[T](initialValue: T): Variable[T] = new Variable(Data.of(initialValue))

  override def fromHistory[T](history: Iterable[ValidData.In1D[T, Instant]]): Variable[T] = new Variable(Data(history))

/**
  * A value that varies in time.
  *
  * @tparam T
  *   the value type
  */
class Variable[T] private (override protected val underlyingData: Data.In1D[T, Instant]) extends VariableBase[T]:

  override def history: immutable.Data.In1D[T, Instant] = underlyingData.toImmutable

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
