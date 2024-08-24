package intervalidus.immutable

import intervalidus.*

import java.time.LocalDate

object DataIn2DTimebound:

  /**
    * Shorthand constructor for a single initial value that is valid in both full time intervals
    *
    * @tparam V
    *   the type of the value managed as data
    * @param value
    *   value to start with
    * @return
    *   DataIn2DTimebound structure with a single valid value
    */
  def of[V](value: V): DataIn2DTimebound[V] = DataIn2DTimebound(
    Iterable(ValidData2D[V, LocalDate, LocalDate](value, DiscreteInterval2D.unbounded))
  )

/**
  * Demonstrate a way to extend [[DataIn2D]] where the two discrete intervals are both discrete time (i.e., LocalDate).
  *
  * @tparam V
  *   the type of the value managed as data
  * @param initialData
  *   (optional) a collection of valid data to start with -- note that if intervals are not disjoint in two dimensions,
  *   results may be unpredictable
  */
class DataIn2DTimebound[V](
  initialData: Iterable[ValidData2D[V, LocalDate, LocalDate]] = Iterable.empty[ValidData2D[V, LocalDate, LocalDate]]
) extends DataIn2D[V, LocalDate, LocalDate](initialData)
