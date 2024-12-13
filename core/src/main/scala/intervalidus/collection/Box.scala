package intervalidus.collection

import scala.annotation.tailrec

/**
  * Represents a box in multidimensional double space. Could be a bounding box that defines the region of a tree node,
  * or could be a valid region associated with data.
  *
  * @param minPoint
  *   Coordinate of the minimum corner of the box(left / lower / back / etc., depending on dimension)
  * @param maxPoint
  *   Coordinate of the maximum corner of the box (right/upper/front/etc., depending on dimension)
  */
case class Box(minPoint: Coordinate, maxPoint: Coordinate):
  private type MM = (Double, Double)
  protected def minMax: Vector[MM] = minPoint.zip(maxPoint)
  private def minMaxForall(other: Box)(f: ((MM, MM)) => Boolean): Boolean =
    minMax.zip(other.minMax).forall(f)

  /**
    * The center of the box -- the point halfway between the mid and max point.
    */
  val midPoint: Coordinate = minPoint mid maxPoint

  /**
    * Does this box fully contain the other box?
    *
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  def contains(other: Box): Boolean = minMaxForall(other):
    case ((thisMin, thisMax), (otherMin, otherMax)) =>
      otherMin >= thisMin && otherMax <= thisMax

  /**
    * Does this box intersect the other box? (Touching or overlapping.)
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching.
    * @param other
    *   box to test
    * @return
    *   true or false
    */
  def intersects(other: Box): Boolean = minMaxForall(other):
    case ((thisMin, thisMax), (otherMin, otherMax)) =>
      otherMin <= thisMax && otherMax >= thisMin

  /**
    * The intersection of this and the other box.
    *
    * @note
    *   Even a single point in common is considered an intersection, e.g., two boxes touching. So the resulting
    *   intersection may have min == max in some or all dimensions.
    * @param other
    *   box to test
    * @return
    *   the intersection if this intersects the other, otherwise None.
    */
  def intersection(other: Box): Option[Box] =
    if intersects(other)
    then Some(Box(minPoint.projectAfter(other.minPoint), maxPoint.projectBefore(other.maxPoint)))
    else None

  /**
    * Constructs a boxes based on binary splits in each dimension. For n dimensions, there will be 2 to the power of n
    * boxes created, which generalizes subtrees for B-tree (2) in one dimension, quadtrees (4) in two dimension, octrees
    * (8) in three dimensions, etc.
    *
    * @return
    *   a vector of boxes that can be used for subtree boundaries
    */
  def binarySplit: Vector[Box] = {
    @tailrec
    def helper(
      minMidMax: Vector[(Double, Double, Double)],
      protoBoxes: Vector[(Vector[Double], Vector[Double])] = Vector.empty
    ): Vector[Box] = minMidMax.headOption match
      case Some((min, mid, max)) =>
        def growProtoBox(b: (Vector[Double], Vector[Double]), appendMin: Double, appendMax: Double) = (
          b._1.appended(appendMin),
          b._2.appended(appendMax)
        )
        val newProtoBoxes =
          if protoBoxes.isEmpty then Vector((Vector(min), Vector(mid)), (Vector(mid), Vector(max)))
          else protoBoxes.map(growProtoBox(_, min, mid)) ++ protoBoxes.map(growProtoBox(_, mid, max))
        helper(minMidMax.tail, newProtoBoxes)

      case None => protoBoxes.map(c => Box(Coordinate(c._1), Coordinate(c._2)))

    val minMidMax = midPoint
      .zip(minMax)
      .map:
        case (mid, (min, max)) => (min, mid, max)
    helper(minMidMax)
  }

  /**
    * Construct a new boxed payload from this box
    *
    * @param payload
    *   the payload
    * @tparam A
    *   the payload type
    * @return
    *   a new boxed payload
    */
  infix def ->[A](payload: A): BoxedPayload[A] = BoxedPayload(this, payload)

  override def toString: String = s"[${minPoint.asString}..${maxPoint.asString}]"
