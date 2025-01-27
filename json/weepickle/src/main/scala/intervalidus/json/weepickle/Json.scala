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

  given [T: DiscreteValue: From: To]: FromTo[Domain1D[T]] =
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

  given [T1: DiscreteValue, T2: DiscreteValue](using
    FromTo[Domain1D[T1]],
    FromTo[Domain1D[T2]]
  ): FromTo[Domain2D[T1, T2]] =
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
    FromTo[Domain1D[T1]],
    FromTo[Domain1D[T2]],
    FromTo[Domain1D[T3]]
  ): FromTo[Domain3D[T1, T2, T3]] =
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

  given [T: DiscreteValue](using FromTo[Domain1D[T]]): FromTo[Interval1D[T]] =
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
    FromTo[Interval1D[T1]],
    FromTo[Interval1D[T2]]
  ): FromTo[Interval2D[T1, T2]] =
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
    FromTo[Interval1D[T1]],
    FromTo[Interval1D[T2]],
    FromTo[Interval1D[T3]]
  ): FromTo[Interval3D[T1, T2, T3]] =
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

  given [V, R: DiscreteValue](using FromTo[V], FromTo[Interval1D[R]]): FromTo[ValidData1D[V, R]] =
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
    FromTo[V],
    FromTo[Interval2D[R1, R2]]
  ): FromTo[ValidData2D[V, R1, R2]] =
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
    FromTo[V],
    FromTo[Interval3D[R1, R2, R3]]
  ): FromTo[ValidData3D[V, R1, R2, R3]] =
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
   * Diff actions encoded as objects
   */

  given [V, R: DiscreteValue](using
    FromTo[ValidData1D[V, R]],
    FromTo[Domain1D[R]]
  ): FromTo[DiffAction1D[V, R]] =
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
    FromTo[ValidData2D[V, R1, R2]],
    FromTo[Domain2D[R1, R2]]
  ): FromTo[DiffAction2D[V, R1, R2]] =
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
    FromTo[ValidData3D[V, R1, R2, R3]],
    FromTo[Domain3D[R1, R2, R3]]
  ): FromTo[DiffAction3D[V, R1, R2, R3]] =
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

  given given_FromTo_immutable_DataIn1D[V, R: DiscreteValue](using
    FromTo[ValidData1D[V, R]]
  ): FromTo[immutable.DataIn1D[V, R]] =
    asValueArr.bimap[immutable.DataIn1D[V, R]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn1D[V, R](arr.value.map(_.as[ValidData1D[V, R]]))
    )

  given given_FromTo_immutable_DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](using
    FromTo[ValidData2D[V, R1, R2]]
  ): FromTo[immutable.DataIn2D[V, R1, R2]] =
    asValueArr.bimap[immutable.DataIn2D[V, R1, R2]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn2D[V, R1, R2](arr.value.map(_.as[ValidData2D[V, R1, R2]]))
    )

  given given_FromTo_immutable_DataIn3D[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    FromTo[ValidData3D[V, R1, R2, R3]]
  ): FromTo[immutable.DataIn3D[V, R1, R2, R3]] =
    asValueArr.bimap[immutable.DataIn3D[V, R1, R2, R3]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataIn3D[V, R1, R2, R3](arr.value.map(_.as[ValidData3D[V, R1, R2, R3]]))
    )

  /*
   * Mutable dimensional data encoded as arrays
   */

  given given_FromTo_mutable_DataIn1D[V, R: DiscreteValue](using
    FromTo[ValidData1D[V, R]]
  ): FromTo[mutable.DataIn1D[V, R]] =
    asValueArr.bimap[mutable.DataIn1D[V, R]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn1D[V, R](arr.value.map(_.as[ValidData1D[V, R]]))
    )

  given given_FromTo_mutable_DataIn2D[V, R1: DiscreteValue, R2: DiscreteValue](using
    FromTo[ValidData2D[V, R1, R2]]
  ): FromTo[mutable.DataIn2D[V, R1, R2]] =
    asValueArr.bimap[mutable.DataIn2D[V, R1, R2]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn2D[V, R1, R2](arr.value.map(_.as[ValidData2D[V, R1, R2]]))
    )

  given given_FromTo_mutable_DataIn3D[V, R1: DiscreteValue, R2: DiscreteValue, R3: DiscreteValue](using
    FromTo[ValidData3D[V, R1, R2, R3]]
  ): FromTo[mutable.DataIn3D[V, R1, R2, R3]] =
    asValueArr.bimap[mutable.DataIn3D[V, R1, R2, R3]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataIn3D[V, R1, R2, R3](arr.value.map(_.as[ValidData3D[V, R1, R2, R3]]))
    )
