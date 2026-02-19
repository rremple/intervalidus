package intervalidus.json

/**
  * Specifies the path to subdocuments over which we want to fold. Similar to uPickle's `TraceVisitor.HasPath`, but
  * optimized for frequent evaluation/comparison.
  */
trait FilterPath:
  // ordered leaf to root
  def path: List[PathComponent]

  // Component "matching" applies path component constraints. It is not strict equality.
  def comparePathTo[T](that: FilterPath)(whenPrefix: => T, whenMatches: => T, otherwise: => T): T =
    val thisSize = this.path.size
    val thatSize = that.path.size
    def thatPrefixComponents = that.path.drop(thatSize - thisSize)

    if thisSize > thatSize then otherwise // this is longer, so this and that are not comparable
    else if this.path.zip(thatPrefixComponents).forall(_ matches _)
    then // prefix matches
      if thisSize < thatSize
      then whenPrefix // this is a logical prefix of that
      else whenMatches // this and that match completely
    else otherwise // the prefix does not match, so this and that are not comparable

  override def toString: String = path.reverse.mkString("/")

  // keep chaining

  /**
    * Filter for an object attribute.
    * @param s
    *   attribute name.
    * @return
    *   filter path to the object attribute.
    */
  def apply(s: String): FilterPath = SimpleFilterPath(PathComponent.toObject(s) :: path)

  /**
    * Filter for an array element.
    * @param i
    *   index of the array element.
    * @return
    *   filter path to the array element.
    */
  def apply(i: Int): FilterPath = SimpleFilterPath(PathComponent.toArrayIndex(i) :: path)

  /**
    * Filter for all array elements referenced by an object attribute.
    * @param s
    *   object attribute name for the array.
    * @return
    *   filter path to the array elements.
    */
  def all(s: String): FilterPath = SimpleFilterPath(PathComponent.toArrayAll :: PathComponent.toObject(s) :: path)

  /**
    * Filter for a slice of array elements referenced by an object attribute.
    * @param s
    *   object attribute name for the array.
    * @param from
    *   the lowest index to include from the array.
    * @param until
    *   the lowest index to EXCLUDE from the array.
    * @return
    *   filter path to the slice of the array elements.
    */
  def slice(s: String, from: Int = 0, until: Int = -1): FilterPath =
    SimpleFilterPath(PathComponent.toArraySlice(from, until) :: PathComponent.toObject(s) :: path)

case class SimpleFilterPath(path: List[PathComponent]) extends FilterPath

/**
  * Common definitions for paths to subdocuments over which we want to fold.
  */
object FilterPath:
  /**
    * Parse a JSON Pointer/Path-like string as a FilterPath. Throws if the string does not conform.
    *
    * Accepts both the primary form (where every component, including bracketed array components, are separated by
    * slashes) and the alternate form (where bracketed array components can immediately follow the object attribute name
    * for the array). For example, if the primary form of a particular path is
    * "provider/locations/[0]/officeHours/times/[*]" then the alternate form is
    * "provider/locations[0]/officeHours/times[*]", which will parse to the same filter path.
    *
    * Another equivalent form is when array indices are separated by slashes,
    * e.g.,"provider/locations/0/officeHours/times[*]"
    *
    * @param s
    *   the JSON Pointer/Path-like string, where each path element is separated by '/'.
    * @return
    *   a FilterPath for the string
    */
  def parse(s: String): FilterPath =
    SimpleFilterPath(s.split('/').toList.flatMap(PathComponent.parse).reverse)

  val RootPath: FilterPath = SimpleFilterPath(List())

  // start chaining

  /**
    * Filter for a top-level object attribute.
    * @param s
    *   attribute name.
    * @return
    *   filter path to the object attribute.
    */
  def apply(s: String): FilterPath = RootPath(s)

  /**
    * Filter for a top-level array element.
    * @param i
    *   index of the array element.
    * @return
    *   filter path to the array element.
    */
  def apply(i: Int): FilterPath = RootPath(i)

  /**
    * Filter for all array elements referenced by a top-level object attribute.
    * @param s
    *   object attribute name for the array.
    * @return
    *   filter path to the array elements.
    */
  def all(s: String): FilterPath = RootPath.all(s)

  /**
    * Filter for a slice of array elements referenced by a top-level object attribute.
    * @param s
    *   object attribute name for the array.
    * @param from
    *   the lowest index to include from the array.
    * @param until
    *   the lowest index to EXCLUDE from the array.
    * @return
    *   filter path to the slice of the array elements.
    */
  def slice(s: String, from: Int = 0, until: Int = -1): FilterPath = RootPath.slice(s, from, until)
