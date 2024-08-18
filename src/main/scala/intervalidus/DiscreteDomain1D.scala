package intervalidus

/**
  * Domain used in defining and operating on a discrete interval. It describes specific discrete data points as well as
  * the special "Bottom" or "Top" cases which conceptually lie below and above the finite range of data points
  * (logically below and above minValue and maxValue respectively). This also gives us a way to accommodate having a
  * predecessor or successor on a boundary, i.e., maxValue.successor == Top and minValue.predecessor == Bottom. Note
  * this allows predecessor and successor to be closed, where Top.successor == Top.predecessor == Top, and
  * Bottom.predecessor == Bottom.successor == Bottom.
  *
  * @tparam T
  *   expected to be a discrete value (i.e., DiscreteValue[T] should be given). But because T must be covariant in the
  *   enum definition, it is impossible to express this dependency directly through context bounds. Therefore this
  *   dependency gets expressed in the definition of Point and in each method of the enum instead.
  */
enum DiscreteDomain1D[+T] extends DimensionalBase.DomainLike:
  /**
    * Smaller than smallest data point (like -∞)
    */
  case Bottom

  /**
    * A single data point in the finite range of this domain
    */
  case Point[P: DiscreteValue](p: P) extends DiscreteDomain1D[P]

  /**
    * Larger than largest data point (like +∞)
    */
  case Top

  override def toString: String = this match
    case Bottom   => "-∞"
    case Point(t) => t.toString
    case Top      => "+∞"

  /**
    * Successor of this, where Bottom and Top are their own successors, and the successor of maxValue is Top.
    *
    * @tparam S
    *   discrete value supertype of T (usually the same as T) used in the return type.
    * @return
    *   successor of this
    */
  def successor[S >: T: DiscreteValue]: DiscreteDomain1D[S] = this match
    case Point(p)    => p.successor.map(Point(_)).getOrElse(Top)
    case topOrBottom => topOrBottom

  /**
    * Predecessor of this, where Bottom and Top are their own predecessors, and the predecessor of minValue is Bottom.
    *
    * @tparam S
    *   discrete value supertype of T (usually the same as T) used in the return type.
    * @return
    *   successor of this
    */
  def predecessor[S >: T: DiscreteValue]: DiscreteDomain1D[S] = this match
    case Point(p)    => p.predecessor.map(Point(_)).getOrElse(Bottom)
    case topOrBottom => topOrBottom

  /**
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @tparam S
    *   discrete value supertype of T (usually the same as T) used in the return type.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  infix def belongsTo[S >: T: DiscreteValue](interval: DiscreteInterval1D[S]): Boolean = interval contains this

  // equivalent symbolic method names

  /**
    * Same as [[belongsTo]]
    *
    * Tests if this belongs to an interval.
    *
    * @param interval
    *   interval to test.
    * @tparam S
    *   discrete value supertype of T (usually the same as T) used in the return type.
    * @return
    *   true if this belongs to the specified interval, false otherwise.
    */
  def ∈[S >: T: DiscreteValue](interval: DiscreteInterval1D[S]): Boolean = this belongsTo interval

object DiscreteDomain1D:
  /**
    * This ordering sorts Bottoms and Tops correctly and leverages the discrete value ordering for the data points in
    * between.
    *
    * Note that, because T is covariant in the enum definition, this ordering will not get summoned automatically for
    * specific instance values of the enum. For example, Point(3).predecessor equiv Point(2) will summon the ordering
    * since Point(3).predecessor has the type IntervalBoundary[Int], but Point(2) equiv Point(3).predecessor will not
    * summon the ordering since Point(2) returns the type IntervalBoundary.Point[Int].
    *
    * One workaround is to cast as supertype, e.g., (Point(2): DiscreteIntervalBoundary[Int]) equiv Point(3).predecessor
    */
  given [T](using
    orderedValues: DiscreteValue[T]
  ): Ordering[DiscreteDomain1D[T]] =
    case (Bottom, Bottom)     => 0
    case (Bottom, _)          => -1
    case (Point(_), Bottom)   => 1
    case (Point(x), Point(y)) => orderedValues.compare(x, y)
    case (Point(_), Top)      => -1
    case (Top, Top)           => 0
    case (Top, _)             => 1

  /**
    * This allows a client to use discrete values in methods requiring a discrete domain element by implicitly
    * converting them to a Point. For example, a client can write dataIn1D.getAt(Point(1)) or dataIn1D.getAt(1). It is
    * nice to not have to wrap all discrete values as Points all the time, and cleaner to have one implicit conversion
    * rather than a multitude of overloaded methods (which are especially problematic when combined with default
    * parameters).
    */
  given [T](using DiscreteValue[T]): Conversion[T, DiscreteDomain1D[T]] = Point(_)
