package intervalidus.tinyrule

import java.time.LocalDate
import java.util.UUID
import scala.compiletime.{erasedValue, summonAll}
import scala.deriving.Mirror
import scala.util.Try

enum FactMergeStyle:
  case KeepingAll, WhenAbsent, AsReplacement

/**
  * A fact has an id and a set of attributes. It is flat, without any nested structure, isomorphic with a similarly flat
  * case class with parameters that only use supported attribute value types (which are: Boolean, String, Int, Double,
  * and LocalDate) or sets/options of those same value types. Based on this, Facts can be created from case classes (via
  * `from`), and vice versa (via `to`).
  *
  * @note
  *   Facts are allowed to have multiple attributes with the same name and different values, which can be used to
  *   represent a set of values for a single name. However, it may be confusing, so it should be used sparingly.
  *
  * @param id
  *   id of the fact, used when merging sets of facts
  * @param attributes
  *   attributes of the fact
  */
case class Fact(id: String, attributes: Set[Attribute[?]]):
  def withId: (String, Fact) = id -> this

  override def toString: String =
    attributes.toList
      .map(a => s" - ${a.name} -> ${a.value}")
      .mkString(s"Fact $id, attributes:\n", ",\n", "\n")

  /**
    * Add an attribute to this fact.
    *
    * @param attribute
    *   to add
    * @return
    *   a new fact with the same id and updated attributes
    */
  def +(attribute: Attribute[?]): Fact = copy(attributes = attributes + attribute)

  /**
    * Remove an attribute from this fact.
    *
    * @param attributeName
    *   name of the attribute to remove
    * @return
    *   a new fact with the same id and updated attributes
    */
  def -(attributeName: String): Fact = copy(attributes = attributes.filterNot(_.name == attributeName))

  /**
    * Merges the attributes of another fact into this one using the specified merge style. (The id of the other fact is
    * ignored.)
    *
    * @param that
    *   fact to merge
    * @param style
    *   Merge style if both this and that have the same attribute with different values:
    *   1. `KeepingAll` (default): both attributes are kept.
    *   1. `WhenAbsent`: keep only this one.
    *   1. `AsReplacements`: replace this one with that one.
    * @return
    *   a new merged fact with the id of this fact
    */
  def merge(that: Fact, style: FactMergeStyle = FactMergeStyle.KeepingAll): Fact = style match
    case FactMergeStyle.KeepingAll =>
      Fact(id, attributes ++ that.attributes)

    case FactMergeStyle.WhenAbsent =>
      val theseNames = attributes.map(_.name)
      val newAttributes = that.attributes.filterNot(a => theseNames.contains(a.name))
      Fact(id, attributes ++ newAttributes)

    case FactMergeStyle.AsReplacement =>
      val thoseNames = that.attributes.map(_.name)
      val keepAttributes = attributes.filterNot(a => thoseNames.contains(a.name))
      Fact(id, keepAttributes ++ that.attributes)

  /**
    * Recursively get all the functions for mapping a set of attribute values to each target element type.
    * @tparam T
    *   The tuple of target types, staring with the product mirror's MirroredElemTypes
    * @return
    *   A function for mapping all sets of attributes to all target types.
    */
  private inline def getProductElemValueOps[T <: Tuple]: List[Set[Any] => Any] =
    inline erasedValue[T] match
      case _: EmptyTuple        => Nil
      case _: (Set[_] *: ts)    => identity :: getProductElemValueOps[ts]
      case _: (Option[_] *: ts) => (_.headOption) :: getProductElemValueOps[ts]
      case _: (_ *: ts)         => (_.head) :: getProductElemValueOps[ts]

  /**
    * Convert this fact to a case class where the case class element names align with the fact attribute names. If there
    * are more attributes than elements, they are ignored. If there are more elements than attributes, an exception is
    * thrown. If multiple attributes have the same name and the corresponding element is a Set, all values are mapped,
    * but if the corresponding element isn't a Set, one of the values (arbitrarily selected) is mapped. If attributes
    * names line up but types don't, construction will likely fail.
    * @param mirror
    *   the [[Mirror.ProductOf]] mirror of the case class, which should be given automatically
    * @tparam P
    *   the case class type (a subtype of [[Product]])
    */
  inline def to[P <: Product](using mirror: Mirror.ProductOf[P]): P =
    val attributesByName = attributes.groupBy(_.name)

    // Get the attribute values as a tuple corresponding to the product label order. Each element of the product
    // label tuple will be a ValueOf[String] (unchecked because type param is erased).
    // The setOps.head will tell us how to map a set of values to the element of the product.
    def elementsFromAttributes(labels: Tuple, setOps: List[Set[Any] => Any]): Tuple = labels match
      case EmptyTuple => EmptyTuple
      case (elementLabel: ValueOf[String] @unchecked) *: remainingLabels =>
        val attributeValueSet = attributesByName.getOrElse(elementLabel.value, Set.empty).map(_.value)
        val elementValue = Try(setOps.head.apply(attributeValueSet))
          .getOrElse(throw new Exception(s"No attribute for ${elementLabel.value}"))
        elementValue *: elementsFromAttributes(remainingLabels, setOps.tail)
      case other => throw new Exception(s"Unexpected case: $other")

    val productElemLabels = summonAll[Tuple.Map[mirror.MirroredElemLabels, ValueOf]]
    val productElemValueSetOps = getProductElemValueOps[mirror.MirroredElemTypes]
    mirror.fromProduct(elementsFromAttributes(productElemLabels, productElemValueSetOps))

object Fact:
  /**
    * Generates attributes that represent the source element.
    * @param elementName
    *   The name of the element
    * @param elementValue
    *   The value (or values) associated with the element
    * @tparam T
    *   The element value type
    * @return
    *   A collection of attributes representing the provided element.
    */
  private inline def attributesFromOneElement[T](elementName: String, elementValue: T): Iterable[Attribute[?]] =
    inline elementValue match
      case a: Boolean           => Seq(BooleanAttribute(elementName, a))
      case a: Int               => Seq(IntAttribute(elementName, a))
      case a: Double            => Seq(DoubleAttribute(elementName, a))
      case a: String            => Seq(StringAttribute(elementName, a))
      case a: LocalDate         => Seq(DateAttribute(elementName, a))
      case a: Option[Boolean]   => a.map(BooleanAttribute(elementName, _))
      case a: Option[Int]       => a.map(IntAttribute(elementName, _))
      case a: Option[Double]    => a.map(DoubleAttribute(elementName, _))
      case a: Option[String]    => a.map(StringAttribute(elementName, _))
      case a: Option[LocalDate] => a.map(DateAttribute(elementName, _))
      case a: Set[Boolean]      => a.map(BooleanAttribute(elementName, _))
      case a: Set[Int]          => a.map(IntAttribute(elementName, _))
      case a: Set[Double]       => a.map(DoubleAttribute(elementName, _))
      case a: Set[String]       => a.map(StringAttribute(elementName, _))
      case a: Set[LocalDate]    => a.map(DateAttribute(elementName, _))
      case a                    => Seq(StringAttribute(elementName, a.toString)) // fallback to String

  /**
    * Recursively deconstructs the product elements as a set of attributes. Unfortunately deconstructing
    * Tuple.fromProductTyped has some kind of conflict with being inline, but without inline it looses type parameter
    * information (important for Set and Option types). So this uses separate lists of attribute names and values
    * (untyped), and reconstructs the element value types from the Tupled type directly (which starts as the product
    * mirror's MirroredElemTypes).
    *
    * @param elementNames
    *   names of the product's elements
    * @param elementValues
    *   values of the product's elements - untyped
    * @param accumulatedAttributes
    *   accumulator for recursive results (this is tailrec, but that annotation is not allowed on inline)
    * @tparam T
    *   the tuple type for the element components passed in as lists
    * @return
    *   a set of attributes for the elements
    */
  private inline def attributesFromElements[T <: Tuple](
    elementNames: List[String],
    elementValues: List[Any],
    accumulatedAttributes: Set[Attribute[?]] = Set.empty
  ): Set[Attribute[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple                         => accumulatedAttributes
      case _: (elementType *: elementsTailTypes) =>
        // recover the type and recurse
        val typedElement = elementValues.head.asInstanceOf[elementType]
        attributesFromElements[elementsTailTypes](
          elementNames.tail,
          elementValues.tail,
          accumulatedAttributes ++ attributesFromOneElement(elementNames.head, typedElement)
        )

  /**
    * Construct from a case class where the name of the fact will be the case class name and the attributes will be the
    * case class elements.
    *
    * @param p
    *   the case class
    * @param mirror
    *   the [[Mirror.ProductOf]] mirror of the case class, which should be given automatically
    * @tparam P
    *   the case class type (a subtype of [[Product]])
    * @return
    *   a new fact
    */
  inline def from[P <: Product](p: P)(using mirror: Mirror.ProductOf[P]): Fact =
    Fact(
      s"${p.productPrefix}-${UUID.randomUUID()}",
      attributesFromElements[mirror.MirroredElemTypes](
        p.productElementNames.toList,
        p.productIterator.toList // untyped, where types are recovered recursively from MirroredElemTypes
      )
    )

  /**
    * Construct from parameters.
    *
    * @param id
    *   name of the fact
    * @param attributes
    *   attributes of the fact
    * @return
    *   a new fact
    */
  def apply(id: String, attributes: Attribute[?]*): Fact = Fact(id, attributes.toSet)

  /**
    * Merge two sets of facts (by id).
    */
  def mergeAll(these: Set[Fact], those: Set[Fact], style: FactMergeStyle = FactMergeStyle.KeepingAll): Set[Fact] =
    val theseById = these.map(_.withId).toMap
    val thoseById = those.map(_.withId).toMap
    (theseById.keySet ++ thoseById.keySet).map: id =>
      theseById.get(id) match
        case Some(thisFact) =>
          thoseById.get(id) match
            case Some(thatFact) => thisFact.merge(thatFact, style)
            case None           => thisFact
        case None => thoseById(id)
