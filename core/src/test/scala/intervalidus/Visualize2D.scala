package intervalidus

import java.awt.geom.{Line2D, Point2D, Rectangle2D}
import java.awt.{Dimension, Graphics, Graphics2D}
import java.time.LocalDate
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.language.implicitConversions
import scala.util.chaining.*

object Visualize2D:
  import Interval.Patterns.*

  // compatibility with old fixed Interval2D by using pattern match extractors
  extension [R1: DomainValueLike, R2: DomainValueLike](interval: Interval.In2D[R1, R2])
    def horizontal: Interval1D[R1] = interval match
      case horizontal x_: _ => horizontal
    def vertical: Interval1D[R2] = interval match
      case _ x_: vertical => vertical

  def apply[V, R1: DomainValueLike, R2: DomainValueLike](
    validData: Iterable[ValidData.In2D[V, R1, R2]],
    title: String
  ): Unit =
    val mainPanel = new Visualize2D(validData, title)
    val frame = new JFrame("Visualize 2D Data")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.add(mainPanel)
    frame.pack()
    frame.setLocationByPlatform(true)
    frame.setVisible(true)

  def apply[V, R1: DomainValueLike, R2: DomainValueLike](
    dataIn2D: DimensionalBase.In2D[V, R1, R2],
    millis: Long = 0,
    title: String = "Visualize 2D data"
  ): Unit =
    apply(dataIn2D.getAll, title)
    Thread.sleep(millis)

protected class Visualize2D[V, R1: DomainValueLike, R2: DomainValueLike](
  validData: Iterable[ValidData.In2D[V, R1, R2]],
  title: String
) extends JPanel:
  import Visualize2D.*
  private val verticalInterstitialMargin: Double = 20.0
  private val horizontalInterstitialMargin: Double = 20.0
  private val outsideMargin: Double = 40.0 // Note that the title is printed within this margin

  override def getPreferredSize: Dimension = new Dimension(1200, 500) // TODO: can we do better than guessing?

  private val horizontalIntervals = Interval1D.uniqueIntervals(validData.map(_.interval.horizontal))
  private val verticalIntervals = Interval1D.uniqueIntervals(validData.map(_.interval.vertical))
  private val verticalLabelCount = verticalIntervals.size

  override protected def paintComponent(graphicsOld: Graphics): Unit =
    super.paintComponent(graphicsOld)
    val graphics = graphicsOld.asInstanceOf[Graphics2D]
    val fontMetrics = graphics.getFontMetrics

    def centerString(string: String, rectangle: Rectangle2D.Double): Point2D.Double =
      val labelBounds = fontMetrics.getStringBounds(string, graphics)
      val labelX = rectangle.getX + (rectangle.getWidth - labelBounds.getWidth) / 2
      val labelY = rectangle.getY + (rectangle.getHeight - labelBounds.getHeight) / 2 + fontMetrics.getAscent
      Point2D.Double(labelX, labelY)

    def drawStringCentered(label: String, rectangle: Rectangle2D.Double): Unit =
      val labelPosition = centerString(label, rectangle)
      graphics.drawString(label, labelPosition.getX.toInt, labelPosition.getY.toInt)

    def drawLeftEdge(rectangle: Rectangle2D.Double): Unit =
      val edge = new Line2D.Double(rectangle.getX, rectangle.getY, rectangle.getX, rectangle.getY + rectangle.getHeight)
      graphics.draw(edge)

    def drawTopEdge(rectangle: Rectangle2D.Double): Unit =
      val edge = new Line2D.Double(rectangle.getX, rectangle.getY, rectangle.getX + rectangle.getWidth, rectangle.getY)
      graphics.draw(edge)

    val maxLabelHeight =
      verticalIntervals.map(i => fontMetrics.getStringBounds(i.toString, graphics).getHeight).maxOption.getOrElse(0.0)
    val maxVerticalLabelWidth =
      verticalIntervals.map(i => fontMetrics.getStringBounds(i.toString, graphics).getWidth).maxOption.getOrElse(0.0)
    val maxHorizontalLabelOrValueWidth = (
      horizontalIntervals.map(i => fontMetrics.getStringBounds(i.toString, graphics).getWidth) ++
        validData.map(v => fontMetrics.getStringBounds(v.value.toString, graphics).getWidth)
    ).maxOption.getOrElse(0.0)

    def verticalPositionByIndex(index: Int) = outsideMargin +
      verticalInterstitialMargin * (verticalLabelCount - index) +
      maxLabelHeight * (verticalLabelCount - index)

    val verticalPositionData = verticalIntervals.zipWithIndex.map: (interval, index) =>
      val startPosition = verticalPositionByIndex(index)
      val endPosition = verticalPositionByIndex(index + 1)
      val rectangle = new Rectangle2D.Double(
        outsideMargin,
        startPosition,
        maxVerticalLabelWidth + horizontalInterstitialMargin,
        endPosition - startPosition
      )
      drawStringCentered(interval.toString, rectangle)
      drawTopEdge(rectangle)
      (interval.start -> startPosition, interval.end -> endPosition)

    val (verticalPositionByStart, verticalPositionByEnd) = verticalPositionData.unzip.pipe: (startData, endData) =>
      (Map.from(startData), Map.from(endData))
    val verticalStartPosition = verticalPositionByIndex(0)
    val verticalEndPosition = verticalPositionByIndex(verticalIntervals.size)
    val verticalSize = verticalStartPosition + outsideMargin
    val verticalEndRectangle = new Rectangle2D.Double(
      outsideMargin,
      verticalEndPosition,
      maxVerticalLabelWidth + horizontalInterstitialMargin,
      0
    )
    drawTopEdge(verticalEndRectangle)

    def horizontalPositionByIndex(index: Int) = outsideMargin +
      horizontalInterstitialMargin * (index + 1) +
      maxHorizontalLabelOrValueWidth * index +
      maxVerticalLabelWidth

    val horizontalPositionData = horizontalIntervals.zipWithIndex.map: (interval, index) =>
      val startPosition = horizontalPositionByIndex(index)
      val endPosition = horizontalPositionByIndex(index + 1)
      val rectangle = new Rectangle2D.Double(
        startPosition,
        verticalStartPosition,
        endPosition - startPosition,
        verticalInterstitialMargin + maxLabelHeight
      )
      drawStringCentered(interval.toString, rectangle)
      drawLeftEdge(rectangle)
      (interval.start -> startPosition, interval.end -> endPosition)

    val (horizontalPositionByStart, horizontalPositionByEnd) = horizontalPositionData.unzip.pipe:
      (startData, endData) => (Map.from(startData), Map.from(endData))
    val horizontalEndPosition = horizontalPositionByIndex(horizontalIntervals.size)
    val horizontalSize = horizontalEndPosition + outsideMargin
    val horizontalEndRectangle = new Rectangle2D.Double(
      horizontalEndPosition,
      verticalStartPosition,
      0,
      verticalInterstitialMargin + maxLabelHeight
    )
    drawLeftEdge(horizontalEndRectangle)

    drawStringCentered(title, Rectangle2D.Double(0, 0, horizontalSize, outsideMargin))
    setSize(new Dimension(horizontalSize.toInt, verticalSize.toInt))

    validData.foreach: v =>
      val leftPosition = horizontalPositionByStart(v.interval.horizontal.start)
      val rightPosition = horizontalPositionByEnd(v.interval.horizontal.end)
      val lowPosition = verticalPositionByStart(v.interval.vertical.start)
      val highPosition = verticalPositionByEnd(v.interval.vertical.end)
      val rectangle =
        new Rectangle2D.Double(leftPosition, highPosition, rightPosition - leftPosition, lowPosition - highPosition)
      graphics.draw(rectangle)
      drawStringCentered(v.value.toString, rectangle)

@main
def tryIt(): Unit =
  import intervalidus.DiscreteValue.given
  // import intervalidus.ContinuousValue.given

  import Interval1D.{interval, intervalFrom, intervalTo, unbounded}

  val now = LocalDate.now()

  val allData: Seq[ValidData.In2D[String, LocalDate, Int]] = List(
    (unbounded[LocalDate] x unbounded[Int]) -> "<default>",
    (intervalTo(now) x intervalTo(8)) -> "Hello",
    (interval(now.minusDays(7), now.plusDays(14)) x interval(5, 6)) -> "World",
    (intervalFrom(now.plusDays(7)) x intervalFrom(5)) -> "!"
  )
  val mutableFixture: mutable.Data.In2D[String, LocalDate, Int] = mutable.Data()
  allData.foreach(mutableFixture.set)

  Visualize2D(mutableFixture, 5)

  val emptyData: immutable.Data.In2D[String, LocalDate, Int] = immutable.Data()
  val immutableFixture = allData.foldLeft(emptyData): (prev, d) =>
    prev.set(d)

  Visualize2D(immutableFixture, 5)
