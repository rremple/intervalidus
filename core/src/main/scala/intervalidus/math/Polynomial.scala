package intervalidus.math

import intervalidus.*
import intervalidus.Domain.In1D
import intervalidus.DimensionalFunctionBase.{DomainFunction, ValidFunction}
import intervalidus.immutable.DataFunction

import scala.Double.{NegativeInfinity, PositiveInfinity}
import scala.language.implicitConversions
import scala.math.{abs, signum}

/**
  * Polynomial with non-negative integer powers having non-zero coefficients
  *
  * @param coefficients
  *   map from exponent to the coefficient of the term with that exponent -- not allowed to be zero.
  */
case class Polynomial private (coefficients: Map[Int, Double]) extends DomainFunction[Double, In1D[Double]]:

  import Polynomial.{ExponentAndCoeffecient, from, zero, constant}

  private val exponents: Seq[Int] = coefficients.keys.toSeq

  def degree: Int = if coefficients.isEmpty then -1 else coefficients.keys.max

  /**
    * Handle infinite limit evaluations using the leading term. This avoids the NaN results which may occur if we just
    * used apply which combine the resulting infinities in each term.
    *
    * @param limitValue
    *   double value expected to be PositiveInfinity or NegativeInfinity
    * @return
    *   the limit as x approaches the limit value.
    */
  private def limitAt(limitValue: Double): Double =
    if coefficients.isEmpty then 0.0
    else
      val maxExp = degree
      val coeff = coefficients(maxExp)
      if maxExp == 0 then coeff
      else PositiveInfinity * signum(coeff) * (if maxExp % 2 == 0 then 1.0 else signum(limitValue))

  /**
    * DomainFunction apply. Evaluate P(x) at points as well as at the limits of the domain. Evaluates open as well as
    * closed points, even though open points are not technically "contained in" any interval. But it is a fair way to
    * approximate a limit at an open boundary.
    *
    * @param d
    *   domain to evaluate
    * @return
    *   value at (or approaching) the domain
    */
  override def apply(d: In1D[Double]): Double = d match
    case Domain1D.Point(x) *: EmptyTuple     => apply(x)
    case Domain1D.OpenPoint(x) *: EmptyTuple => apply(x) // ~limit as value approaches x (potentially dangerous)
    case Domain1D.Bottom *: EmptyTuple       => limitAt(NegativeInfinity)
    case Domain1D.Top *: EmptyTuple          => limitAt(PositiveInfinity)

  /**
    * Evaluate the polynomial at some point x = c0 + c1*x + c2*x^2 + ...
    *
    * @param x
    *   double value to evaluate
    * @return
    *   value of the polynomial at x
    */
  def apply(x: Double): Double =
    if coefficients.isEmpty then 0.0
    else
      /** Use Horner's method to avoid floating point artifacts: [[https://en.wikipedia.org/wiki/Horner%27s_method]] */
      (degree to 0 by -1).foldLeft(0.0): (acc, exponent) =>
        acc * x + coefficients.getOrElse(exponent, 0.0)

  /**
    * Compose this polynomial with some other polynomial (p.compose(g))(x) == p(g(x)).
    *
    * @param g
    *   polynomial to compose
    * @return
    *   g before this polynomial
    */
  def compose(g: Polynomial): Polynomial = exponents.foldLeft(zero): (acc, exponent) =>
    acc + (coefficients(exponent) * (g ^ exponent))

  /**
    * Compose some other polynomial with this polynomial (p.andThen(g))(x) == g(p(x)).
    *
    * @param g
    *   polynomial to compose
    * @return
    *   g after this polynomial
    */
  def andThen(g: Polynomial): Polynomial = g.compose(this)

  /**
    * Represent this polynomial as a string, e.g., "-3x^2 + 1".
    */
  override def toString: String =
    if coefficients.isEmpty then "0"
    else
      exponents
        .sortBy(-_) // decending, largest exponent first
        .foldLeft(""): (acc, exponent) =>
          val c = coefficients(exponent)
          val cAbs = abs(c)
          val cSign = if c < 0 then "-" else "+" // should never be 0.0
          val cFormat = if !c.isInfinite && c.isWhole then cAbs.toLong.toString else cAbs.toString
          val eFormat = if exponent == 0 then "" else if exponent == 1 then "x" else s"x^$exponent"
          val term =
            if eFormat.isEmpty then cFormat
            else if cAbs == 1.0 then eFormat
            else s"$cFormat$eFormat"
          if acc.isEmpty && cSign == "+" then term
          else if acc.isEmpty then s"$cSign$term"
          else s"$acc $cSign $term"

  def toCodeLikeString: String =
    if coefficients.isEmpty then "Polynomial.zero"
    else coefficients.map((e, c) => s"$e -> $c").mkString("Polynomial(", ", ", ")")

  // Exact calculus

  /**
    * The formal derivative of this polynomial. See [[https://en.wikipedia.org/wiki/Formal_derivative]].
    */
  def derivative: Polynomial =
    val entries = exponents.collect:
      case exponent if exponent != 0 => exponent - 1 -> coefficients(exponent) * exponent
    from(entries)

  /**
    * The formal antiderivative of this polynomial without a constant term. See
    * [[https://en.wikipedia.org/wiki/Antiderivative]] and
    * [[https://en.wikipedia.org/wiki/Formal_power_series#Formal_antidifferentiation]]
    * @param constant
    *   the constant of integration, see [[https://en.wikipedia.org/wiki/Constant_of_integration]]
    */
  def integral: Polynomial =
    val entries = exponents.map: exponent =>
      exponent + 1 -> coefficients(exponent) / (exponent + 1)
    from(entries)

  /**
    * Finds the area in some interval using the integral.
    *
    * @param bounds
    *   in interval defining the bounds of integration
    */
  def integrate(bounds: Interval[In1D[Double]]): Double =
    val f = integral
    f(bounds.end) - f(bounds.start)

  // Arithmetic

  /**
    * The negation of this polynomial such that p + p.negated == zero.
    */
  def negated: Polynomial = Polynomial(coefficients.view.mapValues(-_).toMap)

  /**
    * This polynomial added to another polynomial.
    */
  def plus(that: Polynomial): Polynomial =
    val allExponents = (exponents ++ that.exponents).toSeq.distinct
    val entries = allExponents.map: exponent =>
      exponent -> (coefficients.getOrElse(exponent, 0.0) + that.coefficients.getOrElse(exponent, 0.0))
    from(entries)

  /**
    * This polynomial added to some scalar. (An optimization to allow for adding a scalar without first lifting it into
    * a polynomial.)
    */
  def plus(that: Double): Polynomial =
    val newConstantTerm = coefficients.getOrElse(0, 0.0) + that
    if newConstantTerm == 0.0 then Polynomial(coefficients.removed(0))
    else Polynomial(coefficients.updated(0, newConstantTerm))

  /**
    * Subtracts another polynomial from this polynomial.
    */
  def minus(that: Polynomial): Polynomial = this + that.negated

  /**
    * Subtracts some scalar from this polynomial. (An optimization to allow for subtracting a scalar without first
    * lifting it into a polynomial.)
    */
  def minus(that: Double): Polynomial = this + (-that)

  /**
    * This polynomial multiplied with another polynomial.
    */
  def times(that: Polynomial): Polynomial =
    val entries: Seq[ExponentAndCoeffecient] = for
      thisExponent <- exponents
      thatExponent <- that.exponents
    yield (thisExponent + thatExponent, coefficients(thisExponent) * that.coefficients(thatExponent))
    from(entries.groupMapReduce(_.exponent)(_.coeffecient)(_ + _))

  /**
    * This polynomial multiplied by some scalar. (An optimization to allow for multiplication by a scalar without first
    * lifting it into a polynomial.)
    */
  def times(that: Double): Polynomial = from(coefficients.view.mapValues(_ * that))

  /**
    * This polynomial raised to some power.
    */
  def pow(exponent: Int): Polynomial =
    require(exponent >= 0, "Exponent must be non-negative")
    if exponent == 0 then constant(1.0)
    else if exponent == 1 then this
    else
      (1 until exponent).foldLeft(this): (acc, _) =>
        acc * this

  // equivalent symbolic method names

  /**
    * Same as [[negated]].
    *
    * The negation of this polynomial such that p + p.negated == zero.
    */
  def unary_- : Polynomial = negated

  /**
    * Same as [[plus]].
    *
    * This polynomial added to another polynomial.
    */
  infix def +(that: Polynomial): Polynomial = plus(that)

  /**
    * Same as [[plus]].
    *
    * This polynomial added to some scalar. (An optimization to allow for adding a scalar without first lifting it into
    * a polynomial.)
    */
  infix def +(that: Double): Polynomial = plus(that)

  /**
    * Same as [[minus]].
    *
    * Subtracts another polynomial from this polynomial.
    */
  infix def -(that: Polynomial): Polynomial = minus(that)

  /**
    * Same as [[minus]].
    *
    * Subtracts some scalar from this polynomial. (An optimization to allow for subtracting a scalar without first
    * lifting it into a polynomial.)
    */
  infix def -(that: Double): Polynomial = minus(that)

  /**
    * Same as [[times]].
    *
    * This polynomial multiplied with another polynomial.
    */
  infix def *(that: Polynomial): Polynomial = times(that)

  /**
    * Same as [[times]].
    *
    * This polynomial multiplied by some scalar. (An optimization to allow for multiplication by a scalar without first
    * lifting it into a polynomial.)
    */
  infix def *(that: Double): Polynomial = times(that)

  /**
    * Same as [[pow]].
    *
    * This polynomial raised to some power.
    */
  infix def ^(exponent: Int): Polynomial = pow(exponent)

object Polynomial:
  type ExponentAndCoeffecient = (exponent: Int, coeffecient: Double)

  /** constant zero polynomial */
  val zero: Polynomial = Polynomial(Map.empty)

  /** constant term polynomial (degree 0) */
  def constant(value: Double): Polynomial = if value == 0.0 then zero else Polynomial(Map(0 -> value))

  /** x term polynomial (degree 1) */
  val x = Polynomial(Map(1 -> 1.0))

  private def from(entries: Iterable[ExponentAndCoeffecient]): Polynomial =
    Polynomial(entries.collect {
      case t if t.coeffecient != 0.0 => t.toTuple
    }.toMap)

  /**
    * Construct a polynomial using pairs of numbers representing exponents and coefficients.
    *
    * E.g., 'Polynomial(1 -> 3.5, 10 -> -1)' constructs "-x^10 + 3.5x".
    *
    * @param terms
    *   exponent -> coefficient pairs
    * @return
    *   a polynomial
    */
  def apply(terms: ExponentAndCoeffecient*): Polynomial =
    require(terms.forall(_.exponent >= 0), "Exponent must be non-negative")
    from(terms)

  /**
    * Allow scalar * polynomial as well as polynomial * scalar
    */
  extension (scalar: Double) infix def *(poly: Polynomial): Polynomial = poly * scalar

  /**
    * Allow scalar * polynomial as well as polynomial * scalar
    */
  extension (scalar: Int) infix def *(poly: Polynomial): Polynomial = poly * scalar.toDouble

  /**
    * Automatically convert a scalar to a constant polynomial
    */
  given Conversion[Int, Polynomial] = d => if d == 0 then zero else constant(d.toDouble)

  given Conversion[Double, Polynomial] = d => if d == 0.0 then zero else constant(d)

  /**
    * A spline is a function defined piecewise by polynomials. DataFunction is a great way to model these. See
    * [[https://en.wikipedia.org/wiki/Spline_(mathematics)]]
    */
  type Spline = DataFunction[Double, In1D[Double]]
  type Piece = ValidFunction[Double, In1D[Double]]

  import DataFunction.{asData, asDataFunction} // extension methods

  given DomainValueLike[Double] => Conversion[Polynomial, Spline] = DataFunction.ofValue

  /**
    * Using a DataFunction as a spline. The function value type is DomainFunction[Double, Domain.In1D[Double]], but the
    * runtime type must be Polynomial or functions on splines will fail.
    */
  extension (piecewise: Spline)(using DomainValueLike[Double])
    private def applyToPieces(f: Polynomial => Polynomial): Spline =
      piecewise.collectValues:
        case p: Polynomial => f(p)
        case theUnexpected => throw Exception(s"didn't expect $theUnexpected")

    private def zipAndApplyToPieces(that: Spline, f: (Polynomial, Polynomial) => Polynomial): Spline =
      piecewise.asData
        .zipAll(that.asData, zero, zero)
        .collectValues:
          case (p: Polynomial, g: Polynomial) => f(p, g): DomainFunction[Double, In1D[Double]]
          case theUnexpected                  => throw Exception(s"didn't expect $theUnexpected")
        .asDataFunction

    def toCodeLikeString: String =
      val pieces = piecewise.getAll.collect:
        case ValidData(p: Polynomial, interval) => s"${interval.toCodeLikeStringWithParens} -> ${p.toCodeLikeString}"
        case theUnexpected                      => throw Exception(s"didn't expect $theUnexpected")
      pieces.mkString("DataFunction.of(", ", ", ")")

    // Exact calculus

    /**
      * The formal derivative of a spline is just the collection of the derivatives of each polynomial piece.
      */
    def derivative: Spline = piecewise.applyToPieces(_.derivative)

    /**
      * The continuous cumulative integral function without an initial constant term over the unbounded domain (even
      * where the piecewise function is not defined). It can be evaluated to calculate the cumulative "area" under the
      * piecewise curve even across piece boundaries. It is the collection of the antiderivatives of each polynomial
      * piece with the constant of integration adjusted in each so that the area calculation is continuous.
      */
    def integral: Spline =
      val integralPieces: Seq[Piece] = piecewise
        .fill(Interval1D.unbounded -> zero)
        .foldLeft(List.empty[Piece]):
          case (Nil, ValidData(p: Polynomial, i)) => List(i -> p.integral)
          case (acc @ (ValidData(priorP: Polynomial, priorI) :: _), ValidData(p: Polynomial, i)) =>
            val indefinite = p.integral
            val constant = priorP(priorI.end) - indefinite(i.start)
            (i -> (indefinite + constant)) :: acc
          case theUnexpected => throw Exception(s"didn't expect $theUnexpected")

      DataFunction(integralPieces.reverse)

    /**
      * Finds the area in some interval using the cumulative integral function.
      * @param bounds
      *   in interval defining the bounds of integration
      */
    def integrate(bounds: Interval[In1D[Double]]): Double =
      val f = piecewise.integral
      f(bounds.end) - f(bounds.start)

    // Arithmetic

    /**
      * The negation of this spline such that p + p.negated == zero.
      */
    def negated: Spline = piecewise.applyToPieces(_.negated)

    /**
      * This spline added to another spline.
      */
    def plus(that: Spline): Spline = piecewise.zipAndApplyToPieces(that, _ + _)

    /**
      * This spline added to some scalar. (An optimization to allow for adding a scalar without first lifting it into a
      * spline.)
      */
    def plus(that: Double): Spline = piecewise.applyToPieces(_ + that)

    /**
      * Subtracts another spline from this spline.
      */
    def minus(that: Spline): Spline = piecewise + that.negated

    /**
      * Subtracts some scalar from this spline. (An optimization to allow for subtracting a scalar without first lifting
      * it into a spline.)
      */
    def minus(that: Double): Spline = piecewise + (-that)

    /**
      * This spline multiplied with another spline.
      */
    def times(that: Spline): Spline = piecewise.zipAndApplyToPieces(that, _ * _)

    /**
      * This spline multiplied by some scalar. (An optimization to allow for multiplication by a scalar without first
      * lifting it into a spline.)
      */
    def times(that: Double): Spline = piecewise.applyToPieces(_ * that)

    /**
      * This spline raised to some power.
      */
    def pow(exponent: Int): Spline = piecewise.applyToPieces(_ ^ exponent)

    // equivalent symbolic method names

    /**
      * Same as [[plus]].
      *
      * This spline added to another spline.
      */
    infix def +(that: Spline): Spline = piecewise.plus(that)

    /**
      * Same as [[plus]].
      *
      * This spline added to some scalar. (An optimization to allow for adding a scalar without first lifting it into a
      * spline.)
      */
    infix def +(that: Double): Spline = piecewise.plus(that)

    /**
      * Same as [[minus]].
      *
      * Subtracts another spline from this spline.
      */
    infix def -(that: Spline): Spline = piecewise.minus(that)

    /**
      * Same as [[minus]].
      *
      * Subtracts some scalar from this spline. (An optimization to allow for subtracting a scalar without first lifting
      * it into a spline.)
      */
    infix def -(that: Double): Spline = piecewise.minus(that)

    /**
      * Same as [[times]].
      *
      * This spline multiplied with another spline.
      */
    infix def *(that: Spline): Spline = piecewise.times(that)

    /**
      * Same as [[times]].
      *
      * This spline multiplied by some scalar. (An optimization to allow for multiplication by a scalar without first
      * lifting it into a spline.)
      */
    infix def *(that: Double): Spline = piecewise.times(that)

    /**
      * Same as [[pow]].
      *
      * This spline raised to some power.
      */
    infix def ^(exponent: Int): Spline = piecewise.pow(exponent)

  /**
    * Fits a natural cubic spline to the given data points. Assumes there are at least 3 data points and that x values
    * are strictly increasing. See [[https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm]]
    */
  def fitCubicSpline(points: Seq[(Double, Double)])(using DomainValueLike[Double]): Spline =
    require(points.length >= 3, "At least 3 points are required for a cubic spline.")

    val (x, y) = points.unzip match
      case (xs, ys) => (xs.toArray, ys.toArray)

    val n = points.length - 1 // number of polynomial pieces
    val h = Array.tabulate(n): i => // piece widths
      require(x(i + 1) > x(i), "x values must be strictly increasing.")
      x(i + 1) - x(i)

    // 1. Forward sweep of Thomas algorithm (natural boundary conditions: mu(0) = 0, z(0) = 0)
    val mu = new Array[Double](n)
    val z = new Array[Double](n)

    for i <- 1 until n do
      val alpha = 3.0 * ((y(i + 1) - y(i)) / h(i) - (y(i) - y(i - 1)) / h(i - 1))
      val l = 2.0 * (x(i + 1) - x(i - 1)) - h(i - 1) * mu(i - 1)
      mu(i) = h(i) / l
      z(i) = (alpha - h(i - 1) * z(i - 1)) / l

    // 2. Back-substitution for second derivative coefficients (natural boundary conditions: c(0) = 0, c(n) = 0)
    val c = new Array[Double](n + 1)

    for i <- (n - 1) to 1 by -1 do c(i) = z(i) - mu(i) * c(i + 1)

    // 3. Construct piecewise polynomials directly
    val pieces = for i <- 0 until n yield
      val pieceInterval =
        if i == 0 then Interval1D.interval(x(0), x(1)) // closed start and closed end in first piece
        else Interval1D.intervalFromAfter(x(i)).to(x(i + 1)) // open start and closed end after that

      val firstOrder = (y(i + 1) - y(i)) / h(i) - h(i) * (c(i + 1) + 2.0 * c(i)) / 3.0
      val secondOrder = c(i)
      val thirdOrder = (c(i + 1) - c(i)) / (3.0 * h(i))
      val dx = Polynomial.x - x(i)
      val pieceFunction: DomainFunction[Double, In1D[Double]] =
        (thirdOrder * (dx ^ 3)) + (secondOrder * (dx ^ 2)) + (firstOrder * dx) + y(i)

      pieceInterval -> pieceFunction

    DataFunction(pieces).compressAll()
