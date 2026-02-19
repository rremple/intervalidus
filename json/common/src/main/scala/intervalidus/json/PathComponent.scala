package intervalidus.json

/**
  * Used in [[FilterPath]] to express path constraints as components. These can either be
  *   - object path components, which match a specific element of an object by key, or
  *   - array path components, which match array elements by an index slice, where that slice can include a lower bound
  *     (from), an upper bound (until), both, or neither.
  */
enum PathComponent:
  case ObjectPathComponent(key: String)
  case ArrayPathComponent(from: Option[Int], until: Option[Int])

  // or aOpt.fold(bOpt.fold(None)(_ => bOpt))(a => bOpt.fold(aOpt)(b => Some(op(a, b))))
  private def combine[T](aOpt: Option[T], bOpt: Option[T])(op: (T, T) => T): Option[T] = (aOpt, bOpt) match
    case (None, None)       => None
    case (None, _)          => bOpt
    case (_, None)          => aOpt
    case (Some(a), Some(b)) => Some(op(a, b))

  infix def matches(that: PathComponent): Boolean = (this, that) match
    // when object keys are equal
    case (ObjectPathComponent(thisKey), ObjectPathComponent(thatKey)) => thatKey == thisKey

    // when array slice intersections are non-empty
    case (ArrayPathComponent(thisFrom, thisUntil), ArrayPathComponent(thatFrom, thatUntil)) =>
      combine(thisFrom, thatFrom)(_ max _).zip(combine(thisUntil, thatUntil)(_ min _)).forall(_ < _)

    case _ => false

  override def toString: String = this match
    case ObjectPathComponent(key) =>
      key.replace("~1", "/").replace("~0", "~")
    case ArrayPathComponent(from, until) =>
      (from, until) match
        case (None, None)                     => "[*]" // unconstrained
        case (Some(f), Some(u)) if u == f + 1 => s"[$f]" // single index constraint
        case _                                => s"[${from.getOrElse("0")}:${until.getOrElse("")}]" // slice constraint

/**
  * Common definitions for path constraints as components.
  */
object PathComponent:
  /**
    * Parse a JSON Pointer/Path-like string as a PathComponent. Throws if the string does not conform. See
    * [[FilterPath.parse]] for more information.
    *
    * @param s
    *   the JSON Pointer/Path-like string, referencing an individual object attribute and/or array element/slice.
    * @return
    *   a PathComponent for the string
    */
  def parse(s: String): Seq[PathComponent] =

    def parseArrayRef(s: String): PathComponent =
      val arraySpec = s.takeWhile(_ != ']')
      if arraySpec == "*" then toArrayAll
      else if !arraySpec.contains(":") then toArrayIndex(arraySpec.toInt)
      else
        def from(f: String): Int = if f.isEmpty then 0 else f.toInt
        arraySpec.split(':').toList match
          case f :: u :: _ => toArraySlice(from(f), u.toInt)
          case f :: _      => toArraySlice(from(f))
          case Nil         => toArraySlice() // treat "[:]" the same as "[*]"

    def parseObjectRef(s: String): Seq[PathComponent] =
      s.split('[').toList match
        case objPart :: arrayPart :: _ => Seq(toObject(objPart), parseArrayRef(arrayPart))
        case _                         => Seq(toObject(s))

    if s.startsWith("[") then Seq(parseArrayRef(s.drop(1))) // e.g., ".../[1:4]"
    else if s.toIntOption.isDefined then Seq(parseArrayRef(s)) // e.g., ".../1"
    else parseObjectRef(s) // e.g. ".../tags" or ".../tags[1:4]"

  def toObject(key: String): PathComponent = ObjectPathComponent(key.replace("~", "~0").replace("/", "~1"))

  def toArrayAll: PathComponent = ArrayPathComponent(None, None)

  def toArrayIndex(index: Int): PathComponent = ArrayPathComponent(Some(index), Some(index + 1))

  def toArraySlice(from: Int = 0, until: Int = -1): PathComponent = ArrayPathComponent(
    from = if from == 0 then None else Some(from),
    until = if until == -1 then None else Some(until)
  )
