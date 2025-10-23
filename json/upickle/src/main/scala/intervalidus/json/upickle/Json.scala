package intervalidus.json.upickle

import ujson.{Arr, Obj, Str, Value}
import upickle.default.{Reader, ReadWriter, Writer, writeJs}
import intervalidus.*

import scala.language.implicitConversions

object Json:
  private val asValue: ReadWriter[Value] = summon
  private val asValueObj: ReadWriter[Obj] = summon
  private val asValueArr: ReadWriter[Arr] = summon

  extension [T](t: T)(using fromT: Writer[T]) def as[S](using toS: Reader[S]): S = fromT.transform(t, toS)

  /**
    * Domains encoded as strings/objects
    */
  given [T: DiscreteValue: Reader: Writer]: ReadWriter[Domain1D[T]] =
    asValue.bimap[Domain1D[T]](
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

  /**
    * Intervals encoded as objects
    */
  given [D <: NonEmptyTuple: DomainLike](using ReadWriter[D]): ReadWriter[Interval[D]] =
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

  /**
    * Valid data encoded as objects
    */
  given [V, D <: NonEmptyTuple: DomainLike](using ReadWriter[V], ReadWriter[D]): ReadWriter[ValidData[V, D]] =
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

  /**
    * Diff actions encoded as objects
    */
  given [V, D <: NonEmptyTuple: DomainLike](using
    ReadWriter[ValidData[V, D]],
    ReadWriter[D]
  ): ReadWriter[DiffAction[V, D]] =
    asValueObj.bimap[DiffAction[V, D]](
      {
        case DiffAction.Create(validData: ValidData[V, D]) =>
          Obj("action" -> "Create", "validData" -> writeJs(validData))
        case DiffAction.Update(validData: ValidData[V, D]) =>
          Obj("action" -> "Update", "validData" -> writeJs(validData))
        case DiffAction.Delete(key) =>
          Obj("action" -> "Delete", "key" -> writeJs(key: D))
      },
      obj =>
        obj("action").str match
          case "Create" => DiffAction.Create(obj("validData").as[ValidData[V, D]])
          case "Update" => DiffAction.Update(obj("validData").as[ValidData[V, D]])
          case "Delete" => DiffAction.Delete(obj("key").as[D])
    )

  /**
    * Immutable dimensional data encoded as arrays. These require explicit names because the generated names clash.
    */
  given given_ReadWriter_immutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    ReadWriter[ValidData[V, D]]
  ): ReadWriter[immutable.Data[V, D]] =
    asValueArr.bimap[immutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )

  /**
    * Mutable dimensional data encoded as arrays
    */
  given given_ReadWriter_mutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    ReadWriter[ValidData[V, D]]
  ): ReadWriter[mutable.Data[V, D]] =
    asValueArr.bimap[mutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )
