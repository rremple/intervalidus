package intervalidus

import intervalidus.collection.BoxedPayload

/**
  * A value that is valid in an interval of arbitrary dimensions. Conceptually, this defines a partial function where
  * all domain elements that are part of the interval map to the value.
  *
  * @tparam V
  *   the type of the value managed as data (the codomain).
  * @tparam D
  *   the type of the domain upon which the interval is based.
  * @param value
  *   value that is valid in this interval.
  * @param interval
  *   the interval in which the value is valid.
  */
case class ValidData[V, D <: NonEmptyTuple](
  value: V,
  interval: Interval[D]
)(using domainLike: DomainLike[D])
  extends PartialFunction[D, V]:

  /**
    * Approximate this valid data as a boxed payload in double space based on the domain ordered hash.
    *
    * @return
    *   a new boxed payload that can be managed in a box search tree
    */
  def asBoxedPayload: BoxedPayload[ValidData[V, D]] = BoxedPayload(interval.asBox, this)

  override def toString: String = s"$interval -> $value"

  /**
    * Alternative to toString for something that looks more like code. When the dimension of the interval is bigger than
    * 1, puts the interval in parens so the `x` function would actually work.
    */
  def toCodeLikeString: String = s"${interval.toCodeLikeStringWithParens} -> ${value match
      case s: String => s"\"$s\""
      case _         => value.toString
    }"

  /**
    * When stored in a collection, the start of the corresponding interval is the key.
    *
    * @return
    *   a tuple of the domain-like key with this valid data
    */
  def withKey: (D, ValidData[V, D]) = interval.start -> this

  override def apply(domainIndex: D): V =
    if isDefinedAt(domainIndex) then value else throw new Exception(s"Not defined at $domainIndex")

  override def isDefinedAt(domainIndex: D): Boolean = interval contains domainIndex

  // inline because it is called from inline methods in DomainLikeTupleOps)
  inline def valueToString: String = value match
    case set: Set[?] => set.map(_.toString).mkString("{", ",", "}") // for DataMulti
    case _           => value.toString

  // first dimension start string, first dimension end string, value + remaining dimension string
  def preprocessForGrid: (String, String, String) = domainLike.validDataPreprocessForGrid(this)

object ValidData:

  type In1D[V, R1] = ValidData[V, Domain.In1D[R1]]
  type In2D[V, R1, R2] = ValidData[V, Domain.In2D[R1, R2]]
  type In3D[V, R1, R2, R3] = ValidData[V, Domain.In3D[R1, R2, R3]]
  type In4D[V, R1, R2, R3, R4] = ValidData[V, Domain.In4D[R1, R2, R3, R4]]

  /**
    * Valid data are ordered using interval start ordering
    */
  given [
    V,
    D <: NonEmptyTuple: DomainLike
  ](using intervalOrder: Ordering[Interval[D]]): Ordering[ValidData[V, D]] with
    override def compare(x: ValidData[V, D], y: ValidData[V, D]): Int =
      intervalOrder.compare(x.interval, y.interval)
