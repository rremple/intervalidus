package intervalidus.tinyrule

import java.util.UUID
import scala.compiletime.{constValueTuple, erasedValue, summonFrom}

import scala.deriving.Mirror

enum FactMergeStyle:
  case KeepingAll, WhenAbsent, AsReplacement

/**
  * A fact has an id and a set of attributes. It is flat, without any nested structure, isomorphic with a similarly flat
  * case class with elements that only use supported attribute value types (which are: Boolean, String, Int, Double, and
  * LocalDate) or sets/options of those same value types. Based on this, Facts can be created from case classes (via
  * `from`), and vice versa (via `to`).
  *
  * @note
  *   Facts are allowed to have multiple attributes with the same name and different values, which can be used to
  *   represent a set of values for a single name.
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
  def +[T: AttributeValueLike](attribute: Attribute[T]): Fact = copy(attributes = attributes + attribute)

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
    * Recursively gets all the functions for extracting target element values from each set of attribute values. Each
    * function throws an exception if the number of attribute values is more or less than what is expected in the target
    * type.
    * @tparam T
    *   tuple of target types, staring with the product mirror's `MirroredElemTypes`
    * @return
    *   list of functions from a label name (for error reporting) and a set of attribute values to the corresponding
    *   product element value, in the same order as the target elements
    */
  private inline def attributeValuesToElementValues[T <: Tuple]: List[(String, Set[Any]) => Any] =
    def setAsSet(name: String, values: Set[Any]): Set[Any] =
      values // no checks needed
    def setAsOption(name: String, values: Set[Any]): Option[Any] =
      if values.size > 1 then throw new Exception(s"Multiple attributes for $name")
      else values.headOption
    def setAsSingleValue(name: String, values: Set[Any]): Any =
      setAsOption(name, values).getOrElse(throw new Exception(s"No attribute for $name"))

    inline erasedValue[T] match
      case _: EmptyTuple        => Nil
      case _: (Set[?] *: ts)    => setAsSet :: attributeValuesToElementValues[ts]
      case _: (Option[?] *: ts) => setAsOption :: attributeValuesToElementValues[ts]
      case _: (_ *: ts)         => setAsSingleValue :: attributeValuesToElementValues[ts]

  /**
    * Convert this fact to a case class where the case class element names align with the fact attribute names. If there
    * are more attributes than elements, they are ignored. If there are more elements than attributes, an exception is
    * thrown. If multiple attributes have the same name and the corresponding element is a Set, all values are mapped,
    * but if the corresponding element isn't a Set, one of the values (arbitrarily selected) is mapped. If attributes
    * names line up but types don't, construction will likely fail.
    * @param mirror
    *   the `Mirror.ProductOf` mirror of the case class, which should be given automatically
    * @tparam P
    *   the case class type (a subtype of [[Product]])
    */
  inline def to[P <: Product](using mirror: Mirror.ProductOf[P]): P =
    val attributeValuesByName = attributes.groupBy(_.name).view.mapValues(_.map(_.value))

    /**
      * Get the attribute values as a tuple of product element values.
      *
      * @param labels
      *   tuple of product element labels
      * @param fromAttributeValues
      *   list of functions from a label name (for error reporting) and a set of attribute values to the corresponding
      *   product element value, in the same order as the elements
      * @return
      *   a tuple of values that can be used to construct the product (i.e., case class) using its mirror
      */
    def elementsFromAttributes[T <: Tuple](
      labels: T,
      fromAttributeValues: List[(String, Set[Any]) => Any]
    ): Tuple = labels match
      case EmptyTuple => EmptyTuple
      case (label: String) *: labelsTail =>
        val attributeValues = attributeValuesByName.getOrElse(label, Set.empty)
        val elementValue = fromAttributeValues.head.apply(label, attributeValues)
        elementValue *: elementsFromAttributes(labelsTail, fromAttributeValues.tail)
      case other => throw new Exception(s"Unexpected case: $other")

    val productElemLabels = constValueTuple[mirror.MirroredElemLabels]
    val productElemValuesFromAttributeValues = attributeValuesToElementValues[mirror.MirroredElemTypes]
    mirror.fromProduct(elementsFromAttributes(productElemLabels, productElemValuesFromAttributeValues))

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
    summonFrom:
      case given AttributeValueLike[T] => Seq(Attribute(elementName, elementValue))
      case _ =>
        inline elementValue match
          case a: Option[?] => a.toSeq.flatMap(attributesFromOneElement(elementName, _))
          case a: Set[?]    => a.toSeq.flatMap(attributesFromOneElement(elementName, _))
          case a            => Seq(Attribute(elementName, a.toString)) // fallback to String

  /**
    * Recursively deconstructs the product elements as a set of attributes. Unfortunately, deconstructing
    * Tuple.fromProductTyped has some kind of conflict with being inline, but without inline it loses type parameter
    * information (important for Set and Option types). So this uses separate lists of attribute names and values
    * (untyped), and reconstructs the element value types from the Tupled type directly (that start as the product
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
    *   the `Mirror.ProductOf` mirror of the case class, which should be given automatically
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
    val theseFactsById = these.map(_.withId).toMap
    val thoseFactsById = those.map(_.withId).toMap
    (theseFactsById.keySet ++ thoseFactsById.keySet).map: id =>
      theseFactsById.get(id) match
        case None => thoseFactsById(id)
        case Some(thisFact) =>
          thoseFactsById.get(id) match
            case None           => thisFact
            case Some(thatFact) => thisFact.merge(thatFact, style)
