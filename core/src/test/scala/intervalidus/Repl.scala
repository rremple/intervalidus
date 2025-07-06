package intervalidus

import scala.language.implicitConversions

object Repl:

  def main(args: Array[String]): Unit =
    import intervalidus.DiscreteValue.given
    import intervalidus.DomainLike.given
    import intervalidus.Interval1D.*

//    val plan1d = Data
//      .of(intervalFrom(date(2024, 1, 1)) -> "Basic")
//      .set(intervalFrom(date(2024, 4, 1)) -> "Premium")
//      .remove(intervalFromAfter(date(2024, 6, 30)))
//    println(plan1d)
//
//    val plan2d = Data
//      .of((intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25))) -> "Basic")
//      .set((intervalFrom(date(2024, 4, 1)) x intervalFrom(date(2024, 3, 15))) -> "Premium")
//      .remove(intervalFromAfter(date(2024, 6, 30)) x intervalFrom(date(2024, 6, 28)))
//    println(plan2d)

    import Interval.Patterns.*
    val interval: Interval.In3D[Int, Int, Int] = intervalFrom[Int](1) x intervalAt[Int](2) x intervalTo[Int](3)
    interval match
//      case (h @ Interval1D(s1, e1)) :+: (v @ Interval1D(s2, e2)) :+: _ =>
//        println(s"Interval1D($s1, $e1) :+: Interval1D($s2, $e2) :+: _")
      case (h @ Interval1D(s1, e1)) :+: (v @ Interval1D(s2, e2)) :+|: (d @ Interval1D(s3, e3)) =>
        println(s"Interval1D($s1, $e1) :+: Interval1D($s2, $e2) :+|: Interval1D($s3, $e3)")
        println(s"$h :+: $v :+|: $d")
        println(s"${h.toCodeLikeString} x ${v.toCodeLikeString} x ${d.toCodeLikeString}")
      case _ => ???

//    val interval = intervalFrom(date(2024, 1, 1)) x intervalFrom(date(2023, 12, 25))
//    interval.asTupled1D match
//      case (horizontal: Interval1D[LocalDate]) *: (vertical: Interval1D[LocalDate]) *: EmptyTuple =>
//        println(s"horizontal=$horizontal")
//        println(s"vertical=$vertical")
//      case _ => println(s"unexpected type")
//
//      val horizontal *: vertical *: EmptyTuple = interval.asTupled1D
//      println(s"horizontal=$horizontal")
//      println(s"vertical=$vertical")

//      val x: DomainLike[Domain.In2D[LocalDate, LocalDate]] = summon[DomainLike[Domain.In2D[LocalDate, LocalDate]]]
//      val back = Interval.untupled1D[Domain.In2D[LocalDate, LocalDate], Interval.In2D[LocalDate, LocalDate]](tupled1d)
//      println(back)
