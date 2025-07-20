package intervalidus.tinyrule

/**
  * An attribute has a name and a value of some specific type.
  *
  * @tparam T
  *   the type of the attribute value - must be [[AttributeValueLike]].
  */
case class Attribute[T: AttributeValueLike](name: String, value: T)

object Attribute:
  extension (name: String) infix def is[T: AttributeValueLike](value: T): Attribute[T] = Attribute(name, value)
