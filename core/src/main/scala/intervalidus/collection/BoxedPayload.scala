package intervalidus.collection

/**
  * Data in some boxed coordinate space - if a box is split, the original box is set as the parent.
  *
  * @param box
  *   Container of the payload in a coordinate space.
  * @param payload
  *   Value stored in box.
  * @param parentBox
  *   If the box is split because it intersects multiple leaf boundaries, the box will just be the intersections and the
  *   parentBox is set to the original box before the split. (Subsequent splits will carry the same original parent
  *   value.) It the box is never split, the parentBox will be None.
  * @tparam A
  *   payload type
  */
case class BoxedPayload[A](box: Box, payload: A, parentBox: Option[Box] = None):
  /**
    * This boxed payload, but in a different box.
    */
  def withBox(newBox: Box): BoxedPayload[A] = copy(box = newBox)

  /**
    * This boxed payload, but in a different parent box.
    */
  def withParentBox(newParentBox: Option[Box]): BoxedPayload[A] = copy(parentBox = newParentBox)

  /**
    * If this boxed payload had previously been split, recovers the original parent definition before the split. If it
    * has never been split, returns None.
    */
  def asParent: Option[BoxedPayload[A]] = parentBox.map(b => withBox(b).withParentBox(None))

  override def toString: String = s"$box -> $payload${parentBox.map(p => s" (from $p)").getOrElse("")}"

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
    * @return
    *   boxed data with duplicates removed
    */
  def deduplicate[A](
    data: Iterable[BoxedPayload[A]]
  ): Iterable[BoxedPayload[A]] =
    // partition so we only take the performance hit on data with split boxes
    val (split, unsplit) = data.partition(_.parentBox.isDefined)
    val correctedWithDups = split.flatMap(_.asParent)
    unsplit ++ correctedWithDups.toSet
