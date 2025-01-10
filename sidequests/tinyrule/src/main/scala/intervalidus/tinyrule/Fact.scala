package intervalidus.tinyrule

import java.time.LocalDate
import java.util.UUID
import scala.annotation.tailrec
import scala.compiletime.summonAll
import scala.deriving.Mirror
import scala.reflect.ClassTag

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
    val attributeValuesByName = attributes.groupBy(_.name).view.mapValues(_.map(_.value))
    val productElemLabels = summonAll[Tuple.Map[mirror.MirroredElemLabels, ValueOf]]
    // kind of a hack...
    val productElemTypeNames = Macro.summonTypeNames[mirror.MirroredElemTypes]
    // for stringy type comparisons
    import Fact.{set, option}
    val indexedNames = productElemTypeNames.zipWithIndex
    val targetIsSet = indexedNames.map((elemType, index) => index -> elemType.startsWith(set)).toMap
    val targetIsOption = indexedNames.map((elemType, index) => index -> elemType.startsWith(option)).toMap

    // Get the attribute values as a tuple corresponding to the product label order. Each element of the product
    // label tuple will be a ValueOf[String] (unchecked because type param is erased).
    def attributeValueTuple(labelTuple: Tuple, index: Int = 0): Tuple = labelTuple match
      case EmptyTuple => EmptyTuple
      case (firstLabel: ValueOf[String] @unchecked) *: remainingLabels =>
        val attributeValueSet = attributeValuesByName.getOrElse(firstLabel.value, Set.empty)
        val attributeValue =
          if targetIsSet(index) then attributeValueSet
          else if targetIsOption(index) then attributeValueSet.headOption
          else attributeValueSet.headOption.getOrElse(throw new Exception(s"No attribute for ${firstLabel.value}"))
        attributeValue *: attributeValueTuple(remainingLabels, index + 1)
      case other => throw new Exception(s"Unexpected case: $other")

    mirror.fromProduct(attributeValueTuple(productElemLabels))

object Fact:
  // for stringy type comparisons
  private val set = "scala.collection.immutable.Set"
  private val option = "scala.Option"

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
    // kind of a hack, but won't match on Set as a type, and type parameters are erased anyway...
    val productElemTypeNames = Macro.summonTypeNames[mirror.MirroredElemTypes]
    // for stringy type comparisons
    val bool = "scala.Boolean"
    val int = "scala.Int"
    val double = "scala.Double"
    val string = "scala.Predef.String"
    val date = "java.time.LocalDate"

    val attributes: Iterator[Attribute[?]] = p.productElementNames
      .zip(Tuple.fromProductTyped(p).toList)
      .zip(productElemTypeNames)
      .flatMap:
        case ((name, a: Boolean), _)                                                => Seq(BooleanAttribute(name, a))
        case ((name, a: Int), _)                                                    => Seq(IntAttribute(name, a))
        case ((name, a: Double), _)                                                 => Seq(DoubleAttribute(name, a))
        case ((name, a: String), _)                                                 => Seq(StringAttribute(name, a))
        case ((name, a: LocalDate), _)                                              => Seq(DateAttribute(name, a))
        case ((name, a: Option[Boolean] @unchecked), t) if t == s"$option[$bool]"   => a.map(BooleanAttribute(name, _))
        case ((name, a: Option[Int] @unchecked), t) if t == s"$option[$int]"        => a.map(IntAttribute(name, _))
        case ((name, a: Option[Double] @unchecked), t) if t == s"$option[$double]"  => a.map(DoubleAttribute(name, _))
        case ((name, a: Option[String] @unchecked), t) if t == s"$option[$string]"  => a.map(StringAttribute(name, _))
        case ((name, a: Option[LocalDate] @unchecked), t) if t == s"$option[$date]" => a.map(DateAttribute(name, _))
        case ((name, a: Set[Boolean] @unchecked), t) if t == s"$set[$bool]"         => a.map(BooleanAttribute(name, _))
        case ((name, a: Set[Int] @unchecked), t) if t == s"$set[$int]"              => a.map(IntAttribute(name, _))
        case ((name, a: Set[Double] @unchecked), t) if t == s"$set[$double]"        => a.map(DoubleAttribute(name, _))
        case ((name, a: Set[String] @unchecked), t) if t == s"$set[$string]"        => a.map(StringAttribute(name, _))
        case ((name, a: Set[LocalDate] @unchecked), t) if t == s"$set[$date]"       => a.map(DateAttribute(name, _))
        case ((name, a), typeString) => Set(StringAttribute(name, a.toString)) // fallback to String
    Fact(s"${p.productPrefix}-${UUID.randomUUID()}", attributes.toSet)

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
