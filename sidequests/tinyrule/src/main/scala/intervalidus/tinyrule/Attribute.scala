package intervalidus.tinyrule

import java.time.LocalDate

/**
  * An attribute has a name and a value of some specific type.
  *
  * @tparam T
  *   the type of the attribute value - supported types are Boolean, Int, Double, String, and LocalDate.
  */
sealed trait Attribute[T]:
  def name: String
  def value: T

object Attribute:
  /**
    * Construct an attribute given a name and value. Inline, so it can return the specific attribute type for the value
    * type.
    *
    * @param name
    *   attribute name
    * @param anyValue
    *   attribute value with a type known at compile time. Unsupported types are converted to Strings.
    * @return
    *   an attribute corresponding to the value type, if supported, and a StringAttribute with a stringified value
    *   otherwise
    */
  transparent inline def apply(name: String, anyValue: Any): Any = inline anyValue match
    case v: Boolean   => BooleanAttribute(name, v)
    case v: Int       => IntAttribute(name, v)
    case v: Double    => DoubleAttribute(name, v)
    case v: String    => StringAttribute(name, v)
    case v: LocalDate => DateAttribute(name, v)
    case v            => StringAttribute(name, v.toString)

case class BooleanAttribute(name: String, value: Boolean) extends Attribute[Boolean]

case class IntAttribute(name: String, value: Int) extends Attribute[Int]

case class DoubleAttribute(name: String, value: Double) extends Attribute[Double]

case class StringAttribute(name: String, value: String) extends Attribute[String]

case class DateAttribute(name: String, value: LocalDate) extends Attribute[LocalDate]
