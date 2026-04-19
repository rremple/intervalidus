package intervalidus.json.weepickle

import com.rallyhealth.weejson.v1.{Arr, Obj, Str, Value}
import com.rallyhealth.weepickle.v1.WeePickle.{From, FromTo, To}
import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomainValue, VersionMetadata, Versioned}
import intervalidus.VariableBase.Time

/**
  * Common definitions for encoding and decoding Intervalidus structures as JSON.
  */
object Json:
  private val asValue: FromTo[Value] = summon
  private val asValueObj: FromTo[Obj] = summon
  private val asValueArr: FromTo[Arr] = summon

  extension [T](t: T)(using fromT: From[T]) def as[S](using toS: To[S]): S = fromT.transform(t, toS)

  private def writeJs[T: From](t: T): Value = t.as[Value]

  /**
    * Domains encoded as strings/objects
    */
  given [T: DomainValueLike: From: To]: FromTo[Domain1D[T]] =
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
        case unexpected => throw Exception(s"Expected Str (Top/Bottom) or Obj (point/open) but got $unexpected")
      }
    )

  /**
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

  /**
    * Interval shapes encoded as arrays
    */
  given [D <: NonEmptyTuple: DomainLike](using
    FromTo[Interval[D]],
    CoreConfig[D]
  ): FromTo[IntervalShape[D]] =
    asValueArr.bimap[IntervalShape[D]](
      dimensional => Arr.from(dimensional.allIntervals.map(writeJs)),
      arr => IntervalShape.withoutChecks[D](arr.value.map(_.as[Interval[D]]))
    )

  /**
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

  /**
    * Diff actions encoded as objects
    */
  given [V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]],
    FromTo[D]
  ): FromTo[DiffAction[V, D]] =
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
    * Immutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */

  given given_FromTo_immutable_Variable[V](using
    FromTo[ValidData[V, Time]],
    CoreConfig[Time]
  ): FromTo[immutable.Variable[V]] =
    asValueArr.bimap[immutable.Variable[V]](
      dimensional => Arr.from(dimensional.history.getAll.map(writeJs)),
      arr => immutable.Variable.fromHistory(arr.value.map(_.as[ValidData[V, Time]]))
    )

  given given_FromTo_immutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]],
    CoreConfig[D]
  ): FromTo[immutable.Data[V, D]] =
    asValueArr.bimap[immutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )

  given given_FromTo_immutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    FromTo[ValidData[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): FromTo[immutable.DataVersioned[V, D]] =
    asValueObj.bimap[immutable.DataVersioned[V, D]](
      data =>
        Obj(
          "initialVersion" -> writeJs(data.getVersionTimestamps.keySet.minOption.getOrElse(0)),
          "currentVersion" -> writeJs(Some(data.getCurrentVersion)),
          "versionTimestamps" -> writeJs(data.getVersionTimestamps.toSeq),
          "data" -> writeJs(data.getVersionedData.getAll)
        ),
      obj =>
        new immutable.DataVersioned[V, D](
          obj("data").as[Iterable[ValidData[V, Versioned[D]]]],
          obj("initialVersion").as[VersionDomainValue],
          scala.collection.mutable.Map.from(obj("versionTimestamps").as[Seq[(VersionDomainValue, VersionMetadata)]]),
          obj("currentVersion").as[Option[VersionDomainValue]]
        )
    )

  given given_FromTo_immutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[Set[V], D]],
    CoreConfig[D]
  ): FromTo[immutable.DataMulti[V, D]] =
    asValueArr.bimap[immutable.DataMulti[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataMulti[V, D](arr.value.map(_.as[ValidData[Set[V], D]]))
    )

  given given_FromTo_immutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]],
    CoreConfig[D]
  ): FromTo[immutable.DataMonoid[V, D]] =
    asValueArr.bimap[immutable.DataMonoid[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => immutable.DataMonoid[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )

  /**
    * Mutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */

  given given_FromTo_mutable_Variable[V](using
    FromTo[ValidData[V, Time]],
    CoreConfig[Time]
  ): FromTo[mutable.Variable[V]] =
    asValueArr.bimap[mutable.Variable[V]](
      dimensional => Arr.from(dimensional.history.getAll.map(writeJs)),
      arr => mutable.Variable.fromHistory(arr.value.map(_.as[ValidData[V, Time]]))
    )

  given given_FromTo_mutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]],
    CoreConfig[D]
  ): FromTo[mutable.Data[V, D]] =
    asValueArr.bimap[mutable.Data[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.Data[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )

  given given_FromTo_mutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    FromTo[ValidData[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): FromTo[mutable.DataVersioned[V, D]] =
    asValueObj.bimap[mutable.DataVersioned[V, D]](
      data =>
        Obj(
          "initialVersion" -> writeJs(data.getVersionTimestamps.keySet.minOption.getOrElse(0)),
          "currentVersion" -> writeJs(Some(data.getCurrentVersion)),
          "versionTimestamps" -> writeJs(data.getVersionTimestamps.toSeq),
          "data" -> writeJs(data.getVersionedData.getAll)
        ),
      obj =>
        new mutable.DataVersioned[V, D](
          obj("data").as[Iterable[ValidData[V, Versioned[D]]]],
          obj("initialVersion").as[VersionDomainValue],
          scala.collection.mutable.Map.from(obj("versionTimestamps").as[Seq[(VersionDomainValue, VersionMetadata)]]),
          obj("currentVersion").as[Option[VersionDomainValue]]
        )
    )

  given given_FromTo_mutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[Set[V], D]],
    CoreConfig[D]
  ): FromTo[mutable.DataMulti[V, D]] =
    asValueArr.bimap[mutable.DataMulti[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataMulti[V, D](arr.value.map(_.as[ValidData[Set[V], D]]))
    )

  given given_FromTo_mutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    FromTo[ValidData[V, D]],
    CoreConfig[D]
  ): FromTo[mutable.DataMonoid[V, D]] =
    asValueArr.bimap[mutable.DataMonoid[V, D]](
      dimensional => Arr.from(dimensional.getAll.map(writeJs)),
      arr => mutable.DataMonoid[V, D](arr.value.map(_.as[ValidData[V, D]]))
    )
