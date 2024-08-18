package intervalidus.mutable

import intervalidus.*
import intervalidus.DataIn2DBase.ValidData2D

import java.time.LocalDate

object DataTimeboundVersioned:

  /**
    * Shorthand constructor for a single initial value that is valid in the full discrete interval starting at the
    * initial version.
    *
    * @tparam V
    *   the type of the value managed as data
    * @param value
    *   value to start with
    * @param initialVersion
    *   (optional) the version to start with, typically (and by default) zero
    * @return
    *   DataTimeboundVersioned structure with a single valid value
    */
  def of[V](value: V, initialVersion: Int = 0): DataTimeboundVersioned[V] = DataTimeboundVersioned(
    Iterable(ValidData2D[V, LocalDate, Int](value, DiscreteInterval2D.unbounded)),
    initialVersion
  )

/**
  * Demonstrate a way to extend [[DataIn1DVersioned]] where the discrete interval time (i.e., LocalDate).
  *
  * @param initialData
  *   (optional) a collection of valid data in two dimensions (the vertical dimension is the version and the horizontal
  *   is the time) to start with -- note that two dimensional intervals must be disjoint
  * @param initialVersion
  *   (optional) the version to start with, typically zero
  * @param withCurrentVersion
  *   (optional) the version to use as current if different form the initial version, e.g., when making a copy,
  *   typically None
  * @tparam V
  *   the type of the value managed as data
  */
class DataTimeboundVersioned[V](
  initialData: Iterable[ValidData2D[V, LocalDate, Int]] = Iterable.empty[ValidData2D[V, LocalDate, Int]],
  initialVersion: Int = 0,
  withCurrentVersion: Option[DiscreteDomain1D[Int]] = None
) extends DataIn1DVersioned[V, LocalDate](
    initialData,
    initialVersion,
    withCurrentVersion
  )
