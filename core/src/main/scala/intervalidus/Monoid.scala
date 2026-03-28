package intervalidus

/**
  * Type class for a monoid. A monoid is a semigroup with an identity element. See
  * [[https://en.wikipedia.org/wiki/Monoid]]
  */
trait Monoid[V] extends Semigroup[V]:
  /**
    * Identity element, where for any v in V, combine(v, identity) == combine(identity, v) == v.
    */
  def identity: V

object Monoid:
  given Monoid[Unit] with
    override def identity: Unit = ()

    override def combine(lhs: Unit, rhs: Unit): Unit = ()

  given Monoid[Int] with
    override def identity: Int = 0

    override def combine(lhs: Int, rhs: Int): Int = lhs + rhs

  given Monoid[Long] with
    override def identity: Long = 0L

    override def combine(lhs: Long, rhs: Long): Long = lhs + rhs

  given Monoid[Double] with
    override def identity: Double = 0.0

    override def combine(lhs: Double, rhs: Double): Double = lhs + rhs

  /*
   * Unitize a semigroup as an optional monoid
   */
  given [T](using s: Semigroup[T]): Monoid[Option[T]] with

    override def identity: Option[T] = None

    override def combine(lhs: Option[T], rhs: Option[T]): Option[T] = (lhs, rhs) match
      case (None, None)              => None
      case (leftOnly, None)          => leftOnly
      case (None, rightOnly)         => rightOnly
      case (Some(left), Some(right)) => Some(s.combine(left, right))

// This is an example of something that would typecheck as a Semigroup, but because it is not associative, it isn't
// one (it is technically a "Magma"). The following shows its lack of associativity:
//  ( (Rock +  Paper) + Scissors  ) => ( Paper + Scissors ) => Scissors, but
//  (  Rock + (Paper  + Scissors) ) => ( Rock  + Scissors ) => Rock.
//
//  enum Rps:
//    case Rock, Paper, Scissors
//
//  given Semigroup[Rps] with
//    import Rps.*
//    def combine(lhs: Rps, rhs: Rps): Rps = (lhs, rhs) match
//      case (Rock, Scissors)  | (Scissors, Rock)  => Rock     // rock  crushes  scissors
//      case (Paper, Rock)     | (Rock, Paper)     => Paper    // paper  covers  rock
//      case (Scissors, Paper) | (Paper, Scissors) => Scissors // scissors cuts  paper
//      case _                                     => lhs      // tie
