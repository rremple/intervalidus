package intervalidus.collection

/**
  * Data in some boxed coordinate space - if a box is split, the original box is set as the parent.
  *
  * @tparam A
  *   payload type
  * @tparam C
  *   F-bounded coordinate type
  * @tparam B
  *   F-bounded box type (depends on the coordinate type)
  * @tparam Self
  *   F-bounded self type
  */
trait BoxedPayloadLike[A, C <: CoordinateLike[C], B <: BoxLike[C, B], Self <: BoxedPayloadLike[A, C, B, Self]]:
  /**
    * Container of the payload in a coordinate space.
    */
  def box: B

  /**
    * Value stored in box.
    */
  def payload: A

  /**
    * If the box is split because it intersects multiple leaf boundaries, the box will just be the intersections and the
    * parentBox is set to the original box before the split. (Subsequent splits will carry the same original parent
    * value.) It the box is never split, the parentBox will be None.
    */
  def parentBox: Option[B]

  /**
    * This boxed payload, but in a different box.
    */
  def withBox(newBox: B): Self

  /**
    * This boxed payload, but in a different parent box.
    */
  def withParentBox(newParentBox: Option[B]): Self

  /**
    * If this boxed payload had previously been split, recovers the original parent definition before the split. If it
    * has never been split, returns None.
    */
  def asParent: Option[Self] = parentBox.map(b => withBox(b).withParentBox(None))

  override def toString: String = s"$box -> $payload${parentBox.map(p => s" (from $p)").getOrElse("")}"

/**
  * @inheritdoc
  * @tparam A
  *   payload type
  */
case class BoxedPayload1D[A](box: Box1D, payload: A, parentBox: Option[Box1D] = None)
  extends BoxedPayloadLike[A, Coordinate1D, Box1D, BoxedPayload1D[A]]:
  override def withBox(newBox: Box1D): BoxedPayload1D[A] = copy(box = newBox)
  override def withParentBox(newParentBox: Option[Box1D]): BoxedPayload1D[A] = copy(parentBox = newParentBox)

/**
  * @inheritdoc
  * @tparam A
  *   payload type
  */
case class BoxedPayload2D[A](box: Box2D, payload: A, parentBox: Option[Box2D] = None)
  extends BoxedPayloadLike[A, Coordinate2D, Box2D, BoxedPayload2D[A]]:
  override def withBox(newBox: Box2D): BoxedPayload2D[A] = copy(box = newBox)
  override def withParentBox(newParentBox: Option[Box2D]): BoxedPayload2D[A] = copy(parentBox = newParentBox)

/**
  * @inheritdoc
  * @tparam A
  *   payload type
  */
case class BoxedPayload3D[A](box: Box3D, payload: A, parentBox: Option[Box3D] = None)
  extends BoxedPayloadLike[A, Coordinate3D, Box3D, BoxedPayload3D[A]]:
  override def withBox(newBox: Box3D): BoxedPayload3D[A] = copy(box = newBox)
  override def withParentBox(newParentBox: Option[Box3D]): BoxedPayload3D[A] = copy(parentBox = newParentBox)

/**
  * Common operations on boxed payloads of any dimension.
  */
object BoxedPayload:
  /**
    * Removed duplicates where boxes are split.
    *
    * @param data
    *   boxed data that may include duplicates because of box splits
    * @tparam A
    *   payload type
    * @tparam C
    *   F-bounded coordinate type
    * @tparam B
    *   F-bounded box type (depends on the coordinate type)
    * @tparam P
    *   F-bounded boxed payload type
    * @return
    *   boxed data with duplicates removed
    */
  def deduplicate[A, C <: CoordinateLike[C], B <: BoxLike[C, B], P <: BoxedPayloadLike[A, C, B, P]](
    data: Iterable[P]
  ): Iterable[P] =
    // partition so we only take the performance hit on data with split boxes
    val (split, unsplit) = data.partition(_.parentBox.isDefined)
    val correctedWithDups = split.flatMap(_.asParent)
    unsplit ++ correctedWithDups.toSet
