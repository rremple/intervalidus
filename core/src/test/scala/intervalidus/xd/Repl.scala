package intervalidus.xd

import intervalidus.{DiscreteDomain1D, DiscreteDomainLike, DiscreteValue}
import intervalidus.DiscreteDomain1D.given

import java.time.LocalDate
import scala.annotation.tailrec
import scala.language.implicitConversions

object Repl:
  enum DiscreteDomainXd:
    case Z
    case D[H: DiscreteValue, T <: DiscreteDomainXd](head: DiscreteDomain1D[H], tail: T)

    val x: Tuple2[Int, Int] = (1, 2)

    override def toString: String = DiscreteDomainXd.reduce(this, _.toString, _.mkString("(", ",", ")"))

  object DiscreteDomainXd:
    @tailrec
    private def reduce[T, R](
      xd: DiscreteDomainXd,
      mapF: DiscreteDomain1D[_] => T,
      reduceF: Vector[T] => R,
      results: Vector[T] = Vector.empty
    ): R = xd match
      case D(head, tail) => reduce(tail, mapF, reduceF, results.appended(mapF(head)))
      case Z             => reduceF(results)

    extension [A: DiscreteValue](a: A) def *:[T <: DiscreteDomainXd](t: T): D[A, T] = D(a, t)

//    /**
//      * Type class instance for multi-dimensional discrete domains.
//      */
//    given [T: DiscreteValue]: DiscreteDomain[DiscreteDomainXd] with
//      extension (domain: DiscreteDomainXd)
//        override def isUnbounded: Boolean = reduce(domain, x => x.isUnbounded, _.forall(identity))
//
//        override def toCodeLikeString: String =
//          reduce(domain, _.toCodeLikeString, _.mkString("", " *: ", " *: DiscreteDomainXd.Z"))

//          def codeFor(value: T): String = value match
//            case d: LocalDate => s"LocalDate.of(${d.getYear},${d.getMonthValue},${d.getDayOfMonth})"
//            case _ => value.toString
//
//          domain match
//            case Bottom => "Bottom"
//            case Point(t) => s"Point(${codeFor(t)})"
//            case Top => "Top"
//
//        override def asCoordinate: Coordinate =
//          Coordinate(domain.orderedHash)
//
//        override def successor: DiscreteDomain1D[T] = domain match
//          case Point(p) => p.successorValue.map(Point(_)).getOrElse(Top)
//          case topOrBottom => topOrBottom
//
//        override def predecessor: DiscreteDomain1D[T] = domain match
//          case Point(p) => p.predecessorValue.map(Point(_)).getOrElse(Bottom)
//          case topOrBottom => topOrBottom
//

  def main(args: Array[String]): Unit =
    import DiscreteDomainXd.*
    LocalDate.now()
    val d0: Z.type = Z
    val d1: D[LocalDate, Z.type] = LocalDate.now() *: d0
    val t: D[Int, D[LocalDate, Z.type]] = 1 *: d1
//    println(t.toCodeLikeString)
    println(1 *: LocalDate.now() *: LocalDate.now() *: LocalDate.now() *: LocalDate.now() *: Z)
