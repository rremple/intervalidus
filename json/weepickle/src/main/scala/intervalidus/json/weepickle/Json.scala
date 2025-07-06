package intervalidus.json.weepickle

import com.rallyhealth.weejson.v1.{Arr, Obj, Str, Value}
import com.rallyhealth.weepickle.v1.WeePickle.*
import intervalidus.*

import scala.language.implicitConversions

object Json:
  private val asValueObj: FromTo[Obj] = FromTo.join(ToValueObj, FromValueObj)
  private val asValueArr: FromTo[Arr] = FromTo.join(ToValueArr, FromValueArr)

  extension (value: Value) def as[T: To]: T = value.transform(to[T])

  private def writeJs[T: From](t: T): Value = fromScala(t).transform(ToValue)

  /*
   * Domains encoded as strings/objects
   */

  given [T: DomainValueLike: From: To]: FromTo[Domain1D[T]] =
    FromTo
      .join(ToValue, FromValue)
      .bimap[Domain1D[T]](
        {
          case Domain1D.Top          => Str("Top")
          case Domain1D.Bottom       => Str("Bottom")
          case Domain1D.Point(p)     => Obj("point" -> writeJs[T](p))
          case Domain1D.OpenPoint(p) => Obj("open" -> writeJs[T](p))
        },
        {
          case Str("Top")                       => Domain1D.Top
          case Str("Bottom")                    => Domain1D.Bottom
          case Obj(p) if p.isDefinedAt("point") => Domain1D.Point(p("point").as[T])
          case Obj(p) if p.isDefinedAt("open")  => Domain1D.OpenPoint(p("open").as[T])
          case unexpected => throw new Exception(s"Expected Str (Top/Bottom) or Obj (point/open) but got $unexpected")
        }
      )

// TODO: Apparently (discovered in coverage testing) this is not needed as the default transformers of tuples as arrays
//  does the job... unless there are more than 22 dimensions? :'(
//
//  trait JsonDomainLikeTupleOps[D <: NonEmptyTuple]:
//    def toValuesFromDomain(domainTuple: D): List[Value]
//    def fromValuesToDomain(values: List[Value]): D
//
//  import DomainLikeTupleOps.{OneDimDomain, MultiDimDomain}
//
//  /**
//    * Base case, for a one-dimensional domain (empty tail)
//    */
//  private given JsonDomainLikeOneDimOps[DV: DomainValueLike](using
//    From[Domain1D[DV]],
//    To[Domain1D[DV]]
//  ): JsonDomainLikeTupleOps[OneDimDomain[DV]] with
//    inline override def toValuesFromDomain(domainTuple: OneDimDomain[DV]): List[Value] =
//      List(writeJs(domainTuple.head))
//    inline override def fromValuesToDomain(values: List[Value]): OneDimDomain[DV] =
//      values.head.as[Domain1D[DV]]
//
//  /**
//    * Inductive case for a domain with two or more dimensions (non-empty tail)
//    */
//  private given JsonDomainLikeMultiDimOps[
//    DV: DomainValueLike: From: To,
//    DomainTail <: NonEmptyTuple
//  ](using
//    applyToTail: JsonDomainLikeTupleOps[DomainTail]
//  )(using
//    From[Domain1D[DV]],
//    To[Domain1D[DV]]
//  ): JsonDomainLikeTupleOps[Domain1D[DV] *: DomainTail] with
//    inline override def toValuesFromDomain(domainTuple: MultiDimDomain[DV, DomainTail]): List[Value] =
//      writeJs(domainTuple.head) :: applyToTail.toValuesFromDomain(domainTuple.tail)
//    inline override def fromValuesToDomain(values: List[Value]): MultiDimDomain[DV, DomainTail] =
//      values.head.as[Domain1D[DV]] *: applyToTail.fromValuesToDomain(values.tail)

//  given [D <: NonEmptyTuple: DomainLike](using
//    ops: JsonDomainLikeTupleOps[D]
//  ): FromTo[D] =
//    asValueArr.bimap[D](
//      domain => Arr.from(ops.toValuesFromDomain(domain)),
//      arr => ops.fromValuesToDomain(arr.value.toList)
//    )

  /*
   * Intervals encoded as objects
   */

  given [D <: NonEmptyTuple: DomainLike](using FromTo[D]): FromTo[Interval[D]] =
    asValueObj.bimap[Interval[D]](
      interval =>
        Obj(
          "start" -> writeJs(interval.start),
          "end" -> writeJs(interval.end)
        ),
      obj =>
        Interval[D](
          obj("start").as[D],
          obj("end").as[D]
        )
    )

  /*
   * Valid data encoded as objects
   */

  given [V, D <: NonEmptyTuple: DomainLike](using FromTo[V], FromTo[D]): FromTo[ValidData[V, D]] =
    asValueObj.bimap[ValidData[V, D]](
      data =>
        Obj(
          "value" -> writeJs(data.value),
          "interval" -> writeJs(data.interval)
        ),
      obj =>
        ValidData[V, D](
          obj("value").as[V],
          obj("interval").as[Interval[D]]
        )
    )

  /*
   * Diff actions encoded as objects
   */

  given [V, D <: NonEmptyTuple: DomainLike](using
    fromToValidData: FromTo[ValidData[V, D]],
    fromToDomain: FromTo[D]
  ): FromTo[DiffAction[V, D]] =
    asValueObj.bimap[DiffAction[V, D]](
      {
        case DiffAction.Create(validData: ValidData[V, D]) =>
          Obj("action" -> "Create", "validData" -> writeJs(validData))
        case DiffAction.Update(validData: ValidData[V, D]) =>
          Obj("action" -> "Update", "validData" -> writeJs(validData))
        case DiffAction.Delete(key) =>
          Obj("action" -> "Delete", "key" -> writeJs(key)(using fromToDomain))
      },
      obj =>
        obj("action").str match
          case "Create" => DiffAction.Create(obj("validData").as[ValidData[V, D]])
          case "Update" => DiffAction.Update(obj("validData").as[ValidData[V, D]])
          case "Delete" => DiffAction.Delete(obj("key").as[D])
    )

  /*
   * Immutable dimensional data encoded as arrays. These require explicit names because the generated names clash.
   */

  given given_FromTo_immutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]]
  ): FromTo[immutable.Data[V, D]] =
    asValueArr.bimap[immutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )

  /*
   * Mutable dimensional data encoded as arrays
   */

  given given_FromTo_mutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]]
  ): FromTo[mutable.Data[V, D]] =
    asValueArr.bimap[mutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )
