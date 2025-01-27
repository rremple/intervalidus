package intervalidus.json.upickle

import ujson.{Arr, Obj, Str, Value}
import upickle.default.*
import intervalidus.*

import scala.language.implicitConversions

object Json:
  private val asValueObj: ReadWriter[Obj] = ReadWriter.join(JsObjR, JsObjW)
  private val asValueArr: ReadWriter[Arr] = ReadWriter.join(JsArrR, JsArrW)

  extension (value: Value) def as[T: Reader]: T = value.transform(reader[T])

  /*
   * Domains encoded as strings/objects
   */

  given [T: DiscreteValue: Reader: Writer]: ReadWriter[Domain1D[T]] =
    ReadWriter
      .join(JsValueR, JsValueW)
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
          case unexpected => throw new Exception(s"Expected Str (Top/Bottom) or Obj (point) but got $unexpected")
        }
      )

  given [T1: DiscreteValue, T2: DiscreteValue](using
    ReadWriter[Domain1D[T1]],
    ReadWriter[Domain1D[T2]]
  ): ReadWriter[Domain2D[T1, T2]] =
    asValueObj.bimap[Domain2D[T1, T2]](
      domain =>
        Obj(
          "horizontalIndex" -> writeJs(domain.horizontalIndex),
          "verticalIndex" -> writeJs(domain.verticalIndex)
        ),
      obj =>
        Domain2D[T1, T2](
          obj("horizontalIndex").as[Domain1D[T1]],
          obj("verticalIndex").as[Domain1D[T2]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](using
    ReadWriter[Domain1D[T1]],
    ReadWriter[Domain1D[T2]],
    ReadWriter[Domain1D[T3]]
  ): ReadWriter[Domain3D[T1, T2, T3]] =
    asValueObj.bimap[Domain3D[T1, T2, T3]](
      domain =>
        Obj(
          "horizontalIndex" -> writeJs(domain.horizontalIndex),
          "verticalIndex" -> writeJs(domain.verticalIndex),
          "depthIndex" -> writeJs(domain.depthIndex)
        ),
      obj =>
        Domain3D[T1, T2, T3](
          obj("horizontalIndex").as[Domain1D[T1]],
          obj("verticalIndex").as[Domain1D[T2]],
          obj("depthIndex").as[Domain1D[T3]]
        )
    )

  /*
   * Intervals encoded as objects
   */

  given [T: DiscreteValue](using ReadWriter[Domain1D[T]]): ReadWriter[Interval1D[T]] =
    asValueObj.bimap[Interval1D[T]](
      interval =>
        Obj(
          "start" -> writeJs(interval.start),
          "end" -> writeJs(interval.end)
        ),
      obj =>
        Interval1D[T](
          obj("start").as[Domain1D[T]],
          obj("end").as[Domain1D[T]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue](using
    ReadWriter[Interval1D[T1]],
    ReadWriter[Interval1D[T2]]
  ): ReadWriter[Interval2D[T1, T2]] =
    asValueObj.bimap[Interval2D[T1, T2]](
      interval =>
        Obj(
          "horizontal" -> writeJs(interval.horizontal),
          "vertical" -> writeJs(interval.vertical)
        ),
      obj =>
        Interval2D[T1, T2](
          obj("horizontal").as[Interval1D[T1]],
          obj("vertical").as[Interval1D[T2]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](using
    ReadWriter[Interval1D[T1]],
    ReadWriter[Interval1D[T2]],
    ReadWriter[Interval1D[T3]]
  ): ReadWriter[Interval3D[T1, T2, T3]] =
    asValueObj.bimap[Interval3D[T1, T2, T3]](
      interval =>
        Obj(
          "horizontal" -> writeJs(interval.horizontal),
          "vertical" -> writeJs(interval.vertical),
          "depth" -> writeJs(interval.depth)
        ),
      obj =>
        Interval3D[T1, T2, T3](
          obj("horizontal").as[Interval1D[T1]],
          obj("vertical").as[Interval1D[T2]],
          obj("depth").as[Interval1D[T3]]
        )
    )

  /*
   * Valid data encoded as objects
   */

  given [V, R: DiscreteValue](using ReadWriter[V], ReadWriter[Interval1D[R]]): ReadWriter[ValidData1D[V, R]] =
    asValueObj.bimap[ValidData1D[V, R]](
      data =>
        Obj(
          "value" -> writeJs(data.value),
          "interval" -> writeJs(data.interval)
        ),
      obj =>
        ValidData1D[V, R](
          obj("value").as[V],
          obj("interval").as[Interval1D[R]]
        )
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[V],
    ReadWriter[Interval2D[R1, R2]]
  ): ReadWriter[ValidData2D[V, R1, R2]] =
    asValueObj.bimap[ValidData2D[V, R1, R2]](
      data =>
        Obj(
          "value" -> writeJs(data.value),
          "interval" -> writeJs(data.interval)
        ),
      obj =>
        ValidData2D[V, R1, R2](
          obj("value").as[V],
          obj("interval").as[Interval2D[R1, R2]]
        )
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[V],
    ReadWriter[Interval3D[R1, R2, R3]]
  ): ReadWriter[ValidData3D[V, R1, R2, R3]] =
    asValueObj.bimap[ValidData3D[V, R1, R2, R3]](
      data =>
        Obj(
          "value" -> writeJs(data.value),
          "interval" -> writeJs(data.interval)
        ),
      obj =>
        ValidData3D[V, R1, R2, R3](
          obj("value").as[V],
          obj("interval").as[Interval3D[R1, R2, R3]]
        )
    )

  /*
   * Diff actions
   */

  given [V, R: DiscreteValue](using
    ReadWriter[ValidData1D[V, R]],
    ReadWriter[Domain1D[R]]
  ): ReadWriter[DiffAction1D[V, R]] =
    asValueObj.bimap[DiffAction1D[V, R]](
      {
        case DiffAction1D.Create(validData) => Obj("action" -> "Create", "validData" -> writeJs(validData))
        case DiffAction1D.Update(validData) => Obj("action" -> "Update", "validData" -> writeJs(validData))
        case DiffAction1D.Delete(key)       => Obj("action" -> "Delete", "key" -> writeJs(key))
      },
      obj =>
        obj("action").str match
          case "Create" => DiffAction1D.Create(obj("validData").as[ValidData1D[V, R]])
          case "Update" => DiffAction1D.Update(obj("validData").as[ValidData1D[V, R]])
          case "Delete" => DiffAction1D.Delete(obj("key").as[Domain1D[R]])
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[ValidData2D[V, R1, R2]],
    ReadWriter[Domain2D[R1, R2]]
  ): ReadWriter[DiffAction2D[V, R1, R2]] =
    asValueObj.bimap[DiffAction2D[V, R1, R2]](
      {
        case DiffAction2D.Create(validData) => Obj("action" -> "Create", "validData" -> writeJs(validData))
        case DiffAction2D.Update(validData) => Obj("action" -> "Update", "validData" -> writeJs(validData))
        case DiffAction2D.Delete(key)       => Obj("action" -> "Delete", "key" -> writeJs(key))
      },
      obj =>
        obj("action").str match
          case "Create" => DiffAction2D.Create(obj("validData").as[ValidData2D[V, R1, R2]])
          case "Update" => DiffAction2D.Update(obj("validData").as[ValidData2D[V, R1, R2]])
          case "Delete" => DiffAction2D.Delete(obj("key").as[Domain2D[R1, R2]])
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[ValidData3D[V, R1, R2, R3]],
    ReadWriter[Domain3D[R1, R2, R3]]
  ): ReadWriter[DiffAction3D[V, R1, R2, R3]] =
    asValueObj.bimap[DiffAction3D[V, R1, R2, R3]](
      {
        case DiffAction3D.Create(validData) => Obj("action" -> "Create", "validData" -> writeJs(validData))
        case DiffAction3D.Update(validData) => Obj("action" -> "Update", "validData" -> writeJs(validData))
        case DiffAction3D.Delete(key)       => Obj("action" -> "Delete", "key" -> writeJs(key))
      },
      obj =>
        obj("action").str match
          case "Create" => DiffAction3D.Create(obj("validData").as[ValidData3D[V, R1, R2, R3]])
          case "Update" => DiffAction3D.Update(obj("validData").as[ValidData3D[V, R1, R2, R3]])
          case "Delete" => DiffAction3D.Delete(obj("key").as[Domain3D[R1, R2, R3]])
    )

  /*
   * Immutable dimensional data encoded as arrays. These require explicit names because the generated names clash.
   */

  given given_ReadWriter_immutable_DataIn1D[V, R: DiscreteValue](using
    ReadWriter[ValidData1D[V, R]]
  ): ReadWriter[immutable.DataIn1D[V, R]] =
    asValueArr.bimap[immutable.DataIn1D[V, R]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn1D[V, R](arr.value.map(_.as[ValidData1D[V, R]]))
    )

  given given_ReadWriter_immutable_DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[ValidData2D[V, R1, R2]]
  ): ReadWriter[immutable.DataIn2D[V, R1, R2]] =
    asValueArr.bimap[immutable.DataIn2D[V, R1, R2]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn2D[V, R1, R2](arr.value.map(_.as[ValidData2D[V, R1, R2]]))
    )

  given given_ReadWriter_immutable_DataIn3D[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[ValidData3D[V, R1, R2, R3]]
  ): ReadWriter[immutable.DataIn3D[V, R1, R2, R3]] =
    asValueArr.bimap[immutable.DataIn3D[V, R1, R2, R3]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn3D[V, R1, R2, R3](arr.value.map(_.as[ValidData3D[V, R1, R2, R3]]))
    )

  /*
   * Mutable dimensional data encoded as arrays
   */

  given given_ReadWriter_mutable_DataIn1D[V, R: DiscreteValue](using
    ReadWriter[ValidData1D[V, R]]
  ): ReadWriter[mutable.DataIn1D[V, R]] =
    asValueArr.bimap[mutable.DataIn1D[V, R]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn1D[V, R](arr.value.map(_.as[ValidData1D[V, R]]))
    )

  given given_ReadWriter_mutable_DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[ValidData2D[V, R1, R2]]
  ): ReadWriter[mutable.DataIn2D[V, R1, R2]] =
    asValueArr.bimap[mutable.DataIn2D[V, R1, R2]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn2D[V, R1, R2](arr.value.map(_.as[ValidData2D[V, R1, R2]]))
    )

  given given_ReadWriter_mutable_DataIn3D[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[ValidData3D[V, R1, R2, R3]]
  ): ReadWriter[mutable.DataIn3D[V, R1, R2, R3]] =
    asValueArr.bimap[mutable.DataIn3D[V, R1, R2, R3]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn3D[V, R1, R2, R3](arr.value.map(_.as[ValidData3D[V, R1, R2, R3]]))
    )
