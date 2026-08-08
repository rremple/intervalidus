package intervalidus.json.play

import intervalidus.*
import intervalidus.DimensionalVersionedBase.{VersionDomainValue, VersionMetadata, Versioned}
import intervalidus.VariableBase.Time
import play.api.libs.json.*
import play.api.libs.json.Json.{obj, toJson}

/**
  * Common definitions for encoding and decoding Intervalidus structures as JSON.
  */
object Json:

  /**
    * Domains encoded as strings/objects
    */
  given [T: DiscreteValue: Reads: Writes]: Format[Domain1D[T]] = Format(
    Reads: json =>
      def asClosedPoint = (json \ "point").validate[T].map(Domain1D.Point(_))
      def asOpenPoint = (json \ "open").validate[T].map(Domain1D.OpenPoint(_))
      def asUnbound = json
        .validate[String]
        .flatMap:
          case "Top"    => JsSuccess(Domain1D.Top)
          case "Bottom" => JsSuccess(Domain1D.Bottom)
          case unknown  => JsError(s"Unknown Domain1D: $unknown")
      asClosedPoint.orElse(asOpenPoint).orElse(asUnbound)
    ,
    Writes:
      case Domain1D.Point(p)     => obj("point" -> toJson[T](p))
      case Domain1D.OpenPoint(p) => obj("open" -> toJson[T](p))
      case Domain1D.Top          => JsString("Top")
      case Domain1D.Bottom       => JsString("Bottom")
  )

  /**
    * Intervals encoded as objects
    */
  given [D <: NonEmptyTuple: DomainLike: Reads: Writes]: Format[Interval[D]] = Format(
    Reads: json =>
      for
        start <- (json \ "start").validate[D]
        end <- (json \ "end").validate[D]
      yield Interval[D](start, end),
    Writes: interval =>
      obj(
        "start" -> toJson(interval.start),
        "end" -> toJson(interval.end)
      )
  )

  /**
    * Interval shapes encoded as arrays
    */
  given [D <: NonEmptyTuple: DomainLike](using
    Format[Interval[D]],
    CoreConfig[D]
  ): Format[IntervalShape[D]] = Format(
    Reads.of[Vector[Interval[D]]].map(IntervalShape.withoutChecks[D]),
    Writes.of[Vector[Interval[D]]].contramap(_.allIntervals.toVector)
  )

  /**
    * Valid data encoded as objects
    */
  given [V: Reads: Writes, D <: NonEmptyTuple: DomainLike](using
    Format[Interval[D]]
  ): Format[ValidData[V, D]] = Format(
    Reads: json =>
      for
        value <- (json \ "value").validate[V]
        interval <- (json \ "interval").validate[Interval[D]]
      yield ValidData[V, D](value, interval),
    Writes: data =>
      obj(
        "value" -> toJson(data.value),
        "interval" -> toJson(data.interval)
      )
  )

  /**
    * Diff actions encoded as objects
    */
  given [V, D <: NonEmptyTuple: DomainLike: Reads: Writes](using
    Format[ValidData[V, D]]
  ): Format[DiffAction[V, D]] = Format(
    Reads: json =>
      (json \ "action")
        .validate[String]
        .flatMap:
          case "Create" => (json \ "validData").validate[ValidData[V, D]].map(DiffAction.Create(_))
          case "Update" => (json \ "validData").validate[ValidData[V, D]].map(DiffAction.Update(_))
          case "Delete" => (json \ "key").validate[D].map(DiffAction.Delete(_))
          case unknown  => JsError(s"Unknown DiffAction: $unknown"),
    Writes:
      case DiffAction.Create(validData: ValidData[V, D]) =>
        obj("action" -> "Create", "validData" -> toJson(validData))
      case DiffAction.Update(validData: ValidData[V, D]) =>
        obj("action" -> "Update", "validData" -> toJson(validData))
      case DiffAction.Delete(key) =>
        obj("action" -> "Delete", "key" -> toJson[D](key))
  )

  /**
    * Immutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */
  given given_Format_immutable_Variable[V](using
    Format[ValidData[V, Time]],
    CoreConfig[Time]
  ): Format[immutable.Variable[V]] = Format(
    Reads.of[Vector[ValidData[V, Time]]].map(immutable.Variable.fromHistory),
    Writes.of[Vector[ValidData[V, Time]]].contramap(_.history.getAll.toVector)
  )

  given given_Format_immutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[immutable.Data[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => immutable.Data[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Format_immutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    Writes[ValidData[V, Versioned[D]]],
    Reads[mutable.Data[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): Format[immutable.DataVersioned[V, D]] = Format(
    Reads: json =>
      for
        data <- (json \ "data").validate[mutable.Data[V, Versioned[D]]]
        initialVersion <- (json \ "initialVersion").validate[VersionDomainValue]
        versionTimestamps <- (json \ "versionTimestamps").validate[Seq[(VersionDomainValue, VersionMetadata)]]
        currentVersion <- (json \ "currentVersion").validateOpt[VersionDomainValue]
      yield immutable.DataVersioned.asDataVersioned[V, D](data)(
        initialVersion,
        scala.collection.mutable.Map.from(versionTimestamps),
        currentVersion
      ),
    Writes: data =>
      obj(
        "initialVersion" -> data.getVersionTimestamps.keySet.minOption.getOrElse(0),
        "currentVersion" -> data.getCurrentVersion,
        "versionTimestamps" -> toJson(data.getVersionTimestamps.toSeq),
        "data" -> toJson(data.getVersionedData.getAll)
      )
  )

  given given_Format_immutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[Set[V], D]],
    CoreConfig[D]
  ): Format[immutable.DataMulti[V, D]] = Format(
    Reads.of[Vector[ValidData[Set[V], D]]].map(items => immutable.DataMulti[V, D](items)),
    Writes.of[Vector[ValidData[Set[V], D]]].contramap(_.getAll.toVector)
  )

  given given_Format_immutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[immutable.DataMonoid[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => immutable.DataMonoid[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Format_immutable_DataAffine[V, D <: NonEmptyTuple: DomainAffineLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[immutable.DataAffine[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => immutable.DataAffine[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  /**
    * Mutable variables and dimensional data encoded as objects and arrays. These require explicit names because the
    * generated names clash.
    */
  given given_Format_mutable_Variable[V](using
    Format[ValidData[V, Time]],
    CoreConfig[Time]
  ): Format[mutable.Variable[V]] = Format(
    Reads.of[Vector[ValidData[V, Time]]].map(mutable.Variable.fromHistory),
    Writes.of[Vector[ValidData[V, Time]]].contramap(_.history.getAll.toVector)
  )

  given given_Format_mutable_Data[V, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[mutable.Data[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => mutable.Data[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given given_Format_mutable_DataVersioned[V, D <: NonEmptyTuple: DomainLike](using
    DomainLike[Versioned[D]],
    Writes[ValidData[V, Versioned[D]]],
    Reads[mutable.Data[V, Versioned[D]]],
    CoreConfig[Versioned[D]]
  ): Format[mutable.DataVersioned[V, D]] = Format(
    Reads: json =>
      for
        data <- (json \ "data").validate[mutable.Data[V, Versioned[D]]]
        initialVersion <- (json \ "initialVersion").validate[VersionDomainValue]
        versionTimestamps <- (json \ "versionTimestamps").validate[Seq[(VersionDomainValue, VersionMetadata)]]
        currentVersion <- (json \ "currentVersion").validateOpt[VersionDomainValue]
      yield mutable.DataVersioned.asDataVersioned[V, D](data)(
        initialVersion,
        scala.collection.mutable.Map.from(versionTimestamps),
        currentVersion
      ),
    Writes: data =>
      obj(
        "initialVersion" -> data.getVersionTimestamps.keySet.minOption.getOrElse(0),
        "currentVersion" -> data.getCurrentVersion,
        "versionTimestamps" -> toJson(data.getVersionTimestamps.toSeq),
        "data" -> toJson(data.getVersionedData.getAll)
      )
  )

  given given_Format_mutable_DataMulti[V, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[Set[V], D]],
    CoreConfig[D]
  ): Format[mutable.DataMulti[V, D]] = Format(
    Reads.of[Vector[ValidData[Set[V], D]]].map(items => mutable.DataMulti[V, D](items)),
    Writes.of[Vector[ValidData[Set[V], D]]].contramap(_.getAll.toVector)
  )

  given given_Format_mutable_DataMonoid[V: Monoid, D <: NonEmptyTuple: DomainLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[mutable.DataMonoid[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => mutable.DataMonoid[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )

  given unbroken_given_Format_mutable_DataAffine[V, D <: NonEmptyTuple: DomainAffineLike](using
    Format[ValidData[V, D]],
    CoreConfig[D]
  ): Format[mutable.DataAffine[V, D]] = Format(
    Reads.of[Vector[ValidData[V, D]]].map(items => mutable.DataAffine[V, D](items)),
    Writes.of[Vector[ValidData[V, D]]].contramap(_.getAll.toVector)
  )
