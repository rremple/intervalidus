package intervalidus.json.circe

import io.circe.*
import io.circe.syntax.*
import io.circe.Json.{obj, fromString}
import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomainValue, VersionMetadata, Versioned}
import intervalidus.VariableBase.Time

/**
  * Common definitions for encoding and decoding Intervalidus structures as JSON.
  */
object Json:

  /**
    * Domains encoded as strings/objects
    */
  given [T: DiscreteValue: Decoder](using encoderT: Encoder[T]): Codec[Domain1D[T]] = Codec.from(
    Decoder.instance: cursor =>
      def asClosedPoint = cursor.get[T]("point").map(Domain1D.Point(_))
      def asOpenPoint = cursor.get[T]("open").map(Domain1D.OpenPoint(_))
      def asUnbound = cursor
        .as[String]
        .flatMap:
          case "Top"    => Right(Domain1D.Top)
          case "Bottom" => Right(Domain1D.Bottom)
          case unknown  => Left(DecodingFailure(s"Unknown Domain1D: $unknown", cursor.history))
      asClosedPoint.orElse(asOpenPoint).orElse(asUnbound)
    ,
    Encoder.instance:
      case Domain1D.Point(p)     => obj("point" -> p.asJson(using encoderT))
      case Domain1D.OpenPoint(p) => obj("open" -> p.asJson(using encoderT))
      case Domain1D.Top          => fromString("Top")
      case Domain1D.Bottom       => fromString("Bottom")
  )

  /**
    * Intervals encoded as objects
    */
  given [D <: NonEmptyTuple: DomainLike: Encoder: Decoder]: Codec[Interval[D]] = Codec.from(
    Decoder.instance: cursor =>
      for
        start <- cursor.get[D]("start")
        end <- cursor.get[D]("end")
      yield Interval[D](start, end),
    Encoder.instance: interval =>
      obj(
        "start" -> interval.start.asJson,
        "end" -> interval.end.asJson
      )
  )

  /**
    * Interval shapes encoded as arrays
    */
  given [D <: NonEmptyTuple: DomainLike](using
    Codec[Interval[D]],
    CoreConfig[D]
  ): Codec[IntervalShape[D]] = Codec.from(
    Decoder[Vector[Interval[D]]].map(IntervalShape.withoutChecks[D]),
    Encoder[Vector[Interval[D]]].contramap(_.allIntervals.toVector)
  )

  /**
    * Valid data encoded as objects
    */
  given [V: Encoder: Decoder, D <: NonEmptyTuple: DomainLike](using
    Codec[Interval[D]]
  ): Codec[ValidData[V, D]] = Codec.from(
    Decoder.instance: cursor =>
      for
        value <- cursor.get[V]("value")
        interval <- cursor.get[Interval[D]]("interval")
      yield ValidData[V, D](value, interval),
    Encoder.instance: data =>
      obj(
        "value" -> data.value.asJson,
        "interval" -> data.interval.asJson
      )
  )

  /**
    * Diff actions encoded as objects
    */
  given [V, D <: NonEmptyTuple: DomainLike: Decoder](using
    codecV: Codec[ValidData[V, D]],
    encoderD: Encoder[D]
  ): Codec[DiffAction[V, D]] = Codec.from(
    Decoder.instance: cursor =>
      cursor
        .get[String]("action")
        .flatMap:
          case "Create" => cursor.get[ValidData[V, D]]("validData").map(DiffAction.Create(_))
          case "Update" => cursor.get[ValidData[V, D]]("validData").map(DiffAction.Update(_))
          case "Delete" => cursor.get[D]("key").map(DiffAction.Delete(_))
          case unknown  => Left(DecodingFailure(s"Unknown DiffAction: $unknown", cursor.history)),
    Encoder.instance:
      case DiffAction.Create(validData: ValidData[V, D]) =>
        obj("action" -> fromString("Create"), "validData" -> validData.asJson(using codecV))
      case DiffAction.Update(validData: ValidData[V, D]) =>
        obj("action" -> fromString("Update"), "validData" -> validData.asJson(using codecV))
      case DiffAction.Delete(key) =>
        obj("action" -> fromString("Delete"), "key" -> key.asJson(using encoderD))
  )

  /**
    * Immutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */

  given given_Codec_immutable_Variable[V](using
    Codec[ValidData[V, Time]],
    CoreConfig[Time]
  ): Codec[immutable.Variable[V]] = Codec.from(
    Decoder[Vector[ValidData[V, Time]]].map(immutable.Variable.fromHistory),
    Encoder[Vector[ValidData[V, Time]]].contramap(_.history.getAll.toVector)
  )

  given given_Codec_immutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[immutable.Data[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => immutable.Data[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_immutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    Encoder[ValidData[V, Versioned[D]]],
    Decoder[mutable.Data[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): Codec[immutable.DataVersioned[V, D]] = Codec.from(
    Decoder.instance: cursor =>
      for
        data <- cursor.get[mutable.Data[V, Versioned[D]]]("data")
        initialVersion <- cursor.get[VersionDomainValue]("initialVersion")
        versionTimestamps <- cursor.get[Seq[(VersionDomainValue, VersionMetadata)]]("versionTimestamps")
        currentVersion <- cursor.get[Option[VersionDomainValue]]("currentVersion")
      yield immutable.DataVersioned.asDataVersioned[V, D](data)(
        initialVersion,
        scala.collection.mutable.Map.from(versionTimestamps),
        currentVersion
      ),
    Encoder.instance: data =>
      obj(
        "initialVersion" -> data.getVersionTimestamps.keySet.minOption.getOrElse(0).asJson,
        "currentVersion" -> Some(data.getCurrentVersion).asJson,
        "versionTimestamps" -> data.getVersionTimestamps.toSeq.asJson,
        "data" -> data.getVersionedData.getAll.asJson
      )
  )

  given given_Codec_immutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[Set[V], D]],
    CoreConfig[D]
  ): Codec[immutable.DataMulti[V, D]] = Codec.from(
    Decoder[Vector[ValidData[Set[V], D]]].map(items => immutable.DataMulti[V, D](items)),
    Encoder[Vector[ValidData[Set[V], D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_immutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[immutable.DataMonoid[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => immutable.DataMonoid[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_immutable_DataAffine[V, D <: NonEmptyTuple: DomainAffineLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[immutable.DataAffine[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => immutable.DataAffine[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  /**
    * Mutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */

  given given_Codec_mutable_Variable[V](using
    Codec[ValidData[V, Time]],
    CoreConfig[Time]
  ): Codec[mutable.Variable[V]] = Codec.from(
    Decoder[Vector[ValidData[V, Time]]].map(mutable.Variable.fromHistory),
    Encoder[Vector[ValidData[V, Time]]].contramap(_.history.getAll.toVector)
  )

  given given_Codec_mutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[mutable.Data[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => mutable.Data[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_mutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    Encoder[ValidData[V, Versioned[D]]],
    Decoder[mutable.Data[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): Codec[mutable.DataVersioned[V, D]] = Codec.from(
    Decoder.instance: cursor =>
      for
        data <- cursor.get[mutable.Data[V, Versioned[D]]]("data")
        initialVersion <- cursor.get[VersionDomainValue]("initialVersion")
        versionTimestamps <- cursor.get[Seq[(VersionDomainValue, VersionMetadata)]]("versionTimestamps")
        currentVersion <- cursor.get[Option[VersionDomainValue]]("currentVersion")
      yield mutable.DataVersioned.asDataVersioned[V, D](data)(
        initialVersion,
        scala.collection.mutable.Map.from(versionTimestamps),
        currentVersion
      ),
    Encoder.instance: data =>
      obj(
        "initialVersion" -> data.getVersionTimestamps.keySet.minOption.getOrElse(0).asJson,
        "currentVersion" -> Some(data.getCurrentVersion).asJson,
        "versionTimestamps" -> data.getVersionTimestamps.toSeq.asJson,
        "data" -> data.getVersionedData.getAll.asJson
      )
  )

  given given_Codec_mutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[Set[V], D]],
    CoreConfig[D]
  ): Codec[mutable.DataMulti[V, D]] = Codec.from(
    Decoder[Vector[ValidData[Set[V], D]]].map(items => mutable.DataMulti[V, D](items)),
    Encoder[Vector[ValidData[Set[V], D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_mutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[mutable.DataMonoid[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => mutable.DataMonoid[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Codec_mutable_DataAffine[V, D <: NonEmptyTuple: DomainAffineLike](using
    Codec[ValidData[V, D]],
    CoreConfig[D]
  ): Codec[mutable.DataAffine[V, D]] = Codec.from(
    Decoder[Vector[ValidData[V, D]]].map(items => mutable.DataAffine[V, D](items)),
    Encoder[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )
