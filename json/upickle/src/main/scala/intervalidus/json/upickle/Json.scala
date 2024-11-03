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

  given [T: DiscreteValue: Reader: Writer]: ReadWriter[DiscreteDomain1D[T]] =
    ReadWriter
      .join(JsValueR, JsValueW)
      .bimap[DiscreteDomain1D[T]](
        {
          case DiscreteDomain1D.Top      => Str("Top")
          case DiscreteDomain1D.Bottom   => Str("Bottom")
          case DiscreteDomain1D.Point(p) => Obj("point" -> writeJs[T](p))
        },
        {
          case Str("Top")    => DiscreteDomain1D.Top
          case Str("Bottom") => DiscreteDomain1D.Bottom
          case Obj(p)        => DiscreteDomain1D.Point(p("point").as[T])
          case unexpected    => throw new Exception(s"Expected Str (Top/Bottom) or Obj (point) but got $unexpected")
        }
      )

  given [T1: DiscreteValue, T2: DiscreteValue](using
    ReadWriter[DiscreteDomain1D[T1]],
    ReadWriter[DiscreteDomain1D[T2]]
  ): ReadWriter[DiscreteDomain2D[T1, T2]] =
    asValueObj.bimap[DiscreteDomain2D[T1, T2]](
      domain =>
        Obj(
          "horizontalIndex" -> writeJs(domain.horizontalIndex),
          "verticalIndex" -> writeJs(domain.verticalIndex)
        ),
      obj =>
        DiscreteDomain2D[T1, T2](
          obj("horizontalIndex").as[DiscreteDomain1D[T1]],
          obj("verticalIndex").as[DiscreteDomain1D[T2]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](using
    ReadWriter[DiscreteDomain1D[T1]],
    ReadWriter[DiscreteDomain1D[T2]],
    ReadWriter[DiscreteDomain1D[T3]]
  ): ReadWriter[DiscreteDomain3D[T1, T2, T3]] =
    asValueObj.bimap[DiscreteDomain3D[T1, T2, T3]](
      domain =>
        Obj(
          "horizontalIndex" -> writeJs(domain.horizontalIndex),
          "verticalIndex" -> writeJs(domain.verticalIndex),
          "depthIndex" -> writeJs(domain.depthIndex)
        ),
      obj =>
        DiscreteDomain3D[T1, T2, T3](
          obj("horizontalIndex").as[DiscreteDomain1D[T1]],
          obj("verticalIndex").as[DiscreteDomain1D[T2]],
          obj("depthIndex").as[DiscreteDomain1D[T3]]
        )
    )

  /*
   * Intervals encoded as objects
   */

  given [T: DiscreteValue](using ReadWriter[DiscreteDomain1D[T]]): ReadWriter[DiscreteInterval1D[T]] =
    asValueObj.bimap[DiscreteInterval1D[T]](
      interval =>
        Obj(
          "start" -> writeJs(interval.start),
          "end" -> writeJs(interval.end)
        ),
      obj =>
        DiscreteInterval1D[T](
          obj("start").as[DiscreteDomain1D[T]],
          obj("end").as[DiscreteDomain1D[T]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue](using
    ReadWriter[DiscreteInterval1D[T1]],
    ReadWriter[DiscreteInterval1D[T2]]
  ): ReadWriter[DiscreteInterval2D[T1, T2]] =
    asValueObj.bimap[DiscreteInterval2D[T1, T2]](
      interval =>
        Obj(
          "horizontal" -> writeJs(interval.horizontal),
          "vertical" -> writeJs(interval.vertical)
        ),
      obj =>
        DiscreteInterval2D[T1, T2](
          obj("horizontal").as[DiscreteInterval1D[T1]],
          obj("vertical").as[DiscreteInterval1D[T2]]
        )
    )

  given [T1: DiscreteValue, T2: DiscreteValue, T3: DiscreteValue](using
    ReadWriter[DiscreteInterval1D[T1]],
    ReadWriter[DiscreteInterval1D[T2]],
    ReadWriter[DiscreteInterval1D[T3]]
  ): ReadWriter[DiscreteInterval3D[T1, T2, T3]] =
    asValueObj.bimap[DiscreteInterval3D[T1, T2, T3]](
      interval =>
        Obj(
          "horizontal" -> writeJs(interval.horizontal),
          "vertical" -> writeJs(interval.vertical),
          "depth" -> writeJs(interval.depth)
        ),
      obj =>
        DiscreteInterval3D[T1, T2, T3](
          obj("horizontal").as[DiscreteInterval1D[T1]],
          obj("vertical").as[DiscreteInterval1D[T2]],
          obj("depth").as[DiscreteInterval1D[T3]]
        )
    )

  /*
   * Valid data encoded as objects
   */

  given [V, R: DiscreteValue](using ReadWriter[V], ReadWriter[DiscreteInterval1D[R]]): ReadWriter[ValidData1D[V, R]] =
    asValueObj.bimap[ValidData1D[V, R]](
      data =>
        Obj(
          "value" -> writeJs(data.value),
          "interval" -> writeJs(data.interval)
        ),
      obj =>
        ValidData1D[V, R](
          obj("value").as[V],
          obj("interval").as[DiscreteInterval1D[R]]
        )
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[V],
    ReadWriter[DiscreteInterval2D[R1, R2]]
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
          obj("interval").as[DiscreteInterval2D[R1, R2]]
        )
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[V],
    ReadWriter[DiscreteInterval3D[R1, R2, R3]]
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
          obj("interval").as[DiscreteInterval3D[R1, R2, R3]]
        )
    )

  /*
   * Diff actions
   */

  given [V, R: DiscreteValue](using
    ReadWriter[ValidData1D[V, R]],
    ReadWriter[DiscreteDomain1D[R]]
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
          case "Delete" => DiffAction1D.Delete(obj("key").as[DiscreteDomain1D[R]])
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue](using
    ReadWriter[ValidData2D[V, R1, R2]],
    ReadWriter[DiscreteDomain2D[R1, R2]]
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
          case "Delete" => DiffAction2D.Delete(obj("key").as[DiscreteDomain2D[R1, R2]])
    )

  given [V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    ReadWriter[ValidData3D[V, R1, R2, R3]],
    ReadWriter[DiscreteDomain3D[R1, R2, R3]]
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
          case "Delete" => DiffAction3D.Delete(obj("key").as[DiscreteDomain3D[R1, R2, R3]])
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
