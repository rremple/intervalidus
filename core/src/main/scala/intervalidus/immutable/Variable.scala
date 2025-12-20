package intervalidus.immutable

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

  override def history: Data.In1D[T, Instant] = underlyingData

  /**
    * Set the value, starting now.
    * @param value
    *   new value, starting now.
    * @return
    *   a new variable with the value set
    */
  def set(value: T)(using CurrentInstant): Variable[T] = withUniqueTime: now =>
    new Variable(underlyingData.update(intervalFrom(now) -> value))

  /**
    * Undo the last change, if there was one.
    * @return
    *   Some new variable with the last change unset if there was one, otherwise None
    */
  def unset(): Option[Variable[T]] =
    unsetPriorData.map(d => new Variable(underlyingData.update(d)))

  /**
    * Only keep the current value
    *
    * @return
    *   a new variable with the history deleted
    */
  def reset(): Variable[T] = new Variable(underlyingData.update(unbounded -> get))
