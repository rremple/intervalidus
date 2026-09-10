package intervalidus

import intervalidus.math.Polynomial.*
import intervalidus.ContinuousValue.DoubleContinuousValue

import java.awt.geom.{Ellipse2D, Line2D, Path2D, Point2D, Rectangle2D}
import java.awt.{BasicStroke, Color, Dimension, Graphics, Graphics2D, RenderingHints}
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.language.implicitConversions
import scala.math

object VisualizeSpline:

  def apply(
    spline: Spline,
    domainInterval: Interval1D[Double],
    specialPoints: Seq[Double] = Seq.empty,
    drawTangents: Boolean = false,
    pointCount: Integer = 1000
  ): Unit =
    val mainPanel = new VisualizeSpline(spline, domainInterval, specialPoints, drawTangents, pointCount)
    val frame = JFrame("Visualize Spline Data")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.getContentPane.add(mainPanel)
    frame.pack()
    frame.setLocationByPlatform(true)
    frame.setVisible(true)

protected class VisualizeSpline(
  spline: Spline,
  domainInterval: Interval1D[Double],
  specialPoints: Seq[Double],
  drawTangents: Boolean,
  pointCount: Integer
) extends JPanel:

  require(pointCount > 1, "must have more than one point to plot")

  private val domainLabelCount = 11
  private val rangeLabelCount = 11

  private val margin: Double = 10.0 // top/bottom/left/right margin (nothing is drawn here)
  private val specialPointSize: Double = 10.0 // size of open circles for special points
  private val labelMargin: Double = 10.0 // space between label and axis
  private val tickSize: Double = labelMargin // size of an axis tick mark
  private val tangentSize: Double = 100.0 // size of a tangent marker

  private val (domainMin, domainMax) = domainInterval match
    case Interval1D(Domain1D.Point(start), Domain1D.Point(end)) => (start, end)
    case _ => throw IllegalArgumentException("domain must be closed")
  private val domainStep = (domainMax - domainMin) / (pointCount - 1)

  private val domain = for i <- 0 until pointCount yield domainStep * i + domainMin
  private val range = for i <- 0 until pointCount yield spline.getAt(domain(i))

  private val (rangeMin, rangeMax) =
    val (minRaw, maxRaw) = (range.flatten.minOption.getOrElse(0.0).floor, range.flatten.maxOption.getOrElse(0.0).ceil)
    if minRaw == maxRaw then (minRaw - 1.0, maxRaw + 1.0) else (minRaw, maxRaw)

  private val domainLabelStep = (domainMax - domainMin) / (domainLabelCount - 1)
  private val domainLabels = for i <- 0 until domainLabelCount yield f"${domainLabelStep * i + domainMin}%.2f"
  private val rangeLabelStep = (rangeMax - rangeMin) / (rangeLabelCount - 1)
  private val rangeLabels = for i <- 0 until rangeLabelCount yield f"${rangeLabelStep * i + rangeMin}%.2f"

  override def getPreferredSize: Dimension = Dimension(1200, 500) // just the initial width and height

  override protected def paintComponent(graphicsOld: Graphics): Unit =
    super.paintComponent(graphicsOld)
    graphicsOld match
      case graphics: Graphics2D => paintComponent2D(graphics)
      case unexpected           => throw Exception(s"graphics type was $unexpected")

  private def paintComponent2D(graphics: Graphics2D): Unit =
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    val fontMetrics = graphics.getFontMetrics

    // Label is centered horizontally and vertically, pushed down by label margin
    def drawDomainLabel(label: String, labelBox: Rectangle2D.Double): Unit =
      val labelBounds = fontMetrics.getStringBounds(label, graphics)
      val labelX = labelBox.x + (labelBox.getWidth - labelBounds.getWidth) / 2 // center
      val labelY = labelBox.y + (labelBox.getHeight - labelBounds.getHeight) / 2 + fontMetrics.getAscent // center
      graphics.drawString(label, labelX.toInt, (labelY + labelMargin).toInt)

    def drawDomainAxisTick(point: Point2D.Double): Unit =
      val halfSize = tickSize / 2
      val tick = Line2D.Double(point.x, point.y - halfSize, point.x, point.y + halfSize)
      graphics.draw(tick)

    // Label is centered vertically and right justified horizontally, pushed left by label margin
    def drawRangeLabel(label: String, labelBox: Rectangle2D.Double): Unit =
      val labelBounds = fontMetrics.getStringBounds(label, graphics)
      val labelX = labelBox.x + (labelBox.getWidth - labelBounds.getWidth) // right justify
      val labelY = labelBox.y + (labelBox.getHeight - labelBounds.getHeight) / 2 + fontMetrics.getAscent // center
      graphics.drawString(label, (labelX - labelMargin).toInt, labelY.toInt)

    def drawRangeAxisTick(point: Point2D.Double): Unit =
      val halfSize = tickSize / 2
      val tick = Line2D.Double(point.x - halfSize, point.y, point.x + halfSize, point.y)
      graphics.draw(tick)

    def drawSpecialPoint(point: Point2D.Double): Unit =
      val size = specialPointSize
      val halfSize = size / 2
      val ellipse = Ellipse2D.Double(point.x - halfSize, point.y - halfSize, size, size)
      graphics.draw(ellipse)

    def drawSamplePath(path2D: Path2D.Double): Unit =
      graphics.draw(path2D)

    def drawTangentPath(path2D: Path2D.Double): Unit =
      graphics.setColor(Color.BLUE)
      graphics.setStroke(BasicStroke(2.0f))
      graphics.draw(path2D)

    def maxLabelDimension(labels: Seq[String], dimension: Rectangle2D => Double): Double =
      labels.map(label => dimension(fontMetrics.getStringBounds(label, graphics))).maxOption.getOrElse(0.0)

    val maxRangeLabelHeight = maxLabelDimension(rangeLabels, _.getHeight)
    val maxRangeLabelWidth = maxLabelDimension(rangeLabels, _.getWidth)
    val maxDomainLabelHeight = maxLabelDimension(domainLabels, _.getHeight)
    val maxDomainLabelWidth = maxLabelDimension(domainLabels, _.getWidth)

    val extraDomainMargin = maxRangeLabelWidth + labelMargin + maxDomainLabelWidth / 2
    val extraRangeMargin = maxDomainLabelHeight + labelMargin
    val insideDomain: Double = getWidth.toDouble - margin * 2.0 - extraDomainMargin
    val insideRange: Double = getHeight.toDouble - margin * 2.0 - extraRangeMargin
    val domainToHorizontal = insideDomain / (domainMax - domainMin)
    val rangeToVertical = insideRange / (rangeMax - rangeMin)

    def domainPosition(value: Double): Double =
      require(value <= domainMax && value >= domainMin, s"$value <= $domainMax && $value >= $domainMin")
      margin + maxRangeLabelWidth + (value - domainMin) * domainToHorizontal + labelMargin

    def rangePosition(value: Double): Double =
      require(value <= rangeMax && value >= rangeMin, s"$value <= $rangeMax && $value >= $rangeMin")
      margin + (rangeMax - value) * rangeToVertical

    // draw and label axes

    val axisOrigin = Point2D.Double(domainPosition(domainMin), rangePosition(rangeMin))
    val axisExtent = Point2D.Double(domainPosition(domainMax), rangePosition(rangeMax))
    graphics.draw(Line2D.Double(axisOrigin.x, axisOrigin.y, axisExtent.x, axisOrigin.y)) // domain axis
    graphics.draw(Line2D.Double(axisOrigin.x, axisOrigin.y, axisOrigin.x, axisExtent.y)) // range axis

    for (label, index) <- domainLabels.zipWithIndex do
      val point = Point2D.Double(domainPosition(domainLabelStep * index + domainMin), axisOrigin.y)
      drawDomainAxisTick(point)
      val labelBox = Rectangle2D.Double(
        point.x - maxDomainLabelWidth / 2, // center horizontally
        point.y,
        maxDomainLabelWidth,
        maxDomainLabelHeight
      )
      drawDomainLabel(label, labelBox)

    for (label, index) <- rangeLabels.zipWithIndex do
      val point = Point2D.Double(axisOrigin.x, rangePosition(rangeLabelStep * index + rangeMin))
      drawRangeAxisTick(point)
      val rectangle = Rectangle2D.Double(
        point.x - maxRangeLabelWidth,
        point.y - maxRangeLabelHeight / 2, // center vertically
        maxRangeLabelWidth,
        maxRangeLabelHeight
      )
      drawRangeLabel(label, rectangle)

    // plot sample data (as a path)

    val samples = domain.zip(range)
    val (samplePath, _) = samples.foldLeft((Path2D.Double(), false)):
      case ((path, inSegment), (d, Some(r))) =>
        val (x, y) = (domainPosition(d), rangePosition(r))
        if inSegment then path.lineTo(x, y) else path.moveTo(x, y)
        (path, true)
      case ((path, _), _) => (path, false)

    drawSamplePath(samplePath)

    // plot special points and (maybe) tangents at special points

    val derivative = spline.derivative
    val tangentPath = Path2D.Double()
    val tangentHalfLength = tangentSize / 2.0

    for
      d <- specialPoints
      if d >= domainMin && d <= domainMax
      r <- spline.getAt(d)
    do
      val x = domainPosition(d)
      val y = rangePosition(r)
      drawSpecialPoint(Point2D.Double(x, y))
      if drawTangents then
        val screenSlope = derivative(d) * (-rangeToVertical / domainToHorizontal)
        val screenTheta = math.atan(screenSlope)
        val dx = tangentHalfLength * math.cos(screenTheta)
        val dy = tangentHalfLength * math.sin(screenTheta)
        tangentPath.moveTo(x - dx, y - dy)
        tangentPath.lineTo(x + dx, y + dy)

    if drawTangents then drawTangentPath(tangentPath)

@main
def splineIt(): Unit =
  import Interval1D.interval

  val points = Seq((0.0, 0.0), (1.0, 4.0), (2.0, 20.0), (3.0, 36.0), (4.0, 40.0))
  val f = fitCubicSpline(points)

  println(s"f:\n$f")
  println(s"f':\n${f.derivative}")
  println(s"f'':\n${f.derivative.derivative}")
  println(s"f''':\n${f.derivative.derivative.derivative}")
  println(s"f'''':\n${f.derivative.derivative.derivative.derivative}")

  val xSpecial = points.map(_._1)
  val xInterval = interval(-0.5, 4.5)

  // f: A clean, monotonic sigmoidal curve passing smoothly through every interpolation point.
  VisualizeSpline(f, xInterval, xSpecial, drawTangents = true)

  // f': Parabolic arcs with peak at the inflection point x = 2.0, smoothly tapering at the boundaries.
  VisualizeSpline(f.derivative, xInterval, xSpecial, drawTangents = true)

  // f'': Continuous, piecewise-linear curvature that crosses exactly through zero at the inflection point x = 2.0,
  // with natural boundary conditions pulling it to zero on the edges.
  VisualizeSpline(f.derivative.derivative, xInterval, xSpecial, drawTangents = true)

  // f''': Piecewise-constant step functions, jumping instantaneously at the interior knots of x = 1.0 and 3.0.
  VisualizeSpline(f.derivative.derivative.derivative, xInterval, xSpecial, drawTangents = true)

  // f'''': Zero everywhere.
  VisualizeSpline(f.derivative.derivative.derivative.derivative, xInterval, xSpecial, drawTangents = true)
