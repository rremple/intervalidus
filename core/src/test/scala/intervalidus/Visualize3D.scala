package intervalidus

import java.awt.Desktop
import java.net.{InetSocketAddress, URI, URLEncoder}
import java.nio.file.{Files, Path, Paths}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import intervalidus.collection.{Box, Coordinate}

import java.nio.charset.StandardCharsets
import scala.annotation.nowarn
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object Visualize3D:

  import Interval.Patterns.*

  // compatibility with old fixed Interval3D by using pattern match extractors
  extension [R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](interval: Interval.In3D[R1, R2, R3])
    @nowarn("msg=match may not be exhaustive")
    def horizontal: Interval1D[R1] = interval match
      case horizontal :+: _ :+|: _ => horizontal
    @nowarn("msg=match may not be exhaustive")
    def vertical: Interval1D[R2] = interval match
      case _ :+: vertical :+|: _ => vertical
    @nowarn("msg=match may not be exhaustive")
    def depth: Interval1D[R3] = interval match
      case _ :+: _ :+|: depth => depth

  private val webRootDirPath = Paths.get("core", "vis3d")
  private val port = 8080

  def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    validData: Iterable[ValidData.In3D[V, R1, R2, R3]],
    title: String
  ): Unit =
    val rootDir = webRootDirPath.toAbsolutePath
    if !Files.isDirectory(rootDir) then println(s"Error: Web root directory not found at $rootDir")
    else if validData.isEmpty then println(s"Error: No data to plot")
    else
      val origin = Coordinate(0, 0, 0)
      val intervals = validData.map(_.interval)
      val hasUnbounded = intervals.exists: i =>
        i.horizontal.start.isUnbounded ||
          i.vertical.start.isUnbounded ||
          i.depth.start.isUnbounded ||
          i.horizontal.end.isUnbounded ||
          i.vertical.end.isUnbounded ||
          i.depth.end.isUnbounded

      // a sample coordinate in the space of bounded values, with the origin as the default for unbounded
      val sample = intervals
        .foldLeft(origin): (prior, i) =>
          val beforePrior = prior.projectBeforeBounded(
            i.asBox.minPoint,
            i.horizontal.start.isUnbounded,
            i.vertical.start.isUnbounded,
            i.depth.start.isUnbounded
          )
          val afterPrior = beforePrior.projectAfterBounded(
            i.asBox.maxPoint,
            i.horizontal.end.isUnbounded,
            i.vertical.end.isUnbounded,
            i.depth.end.isUnbounded
          )
          afterPrior

      println(s"sample = $sample")
      val (minBounded, maxBounded) = intervals
        .foldLeft((sample, sample)):
          case ((priorMin, priorMax), i) =>
            val newMin = priorMin.projectBeforeBounded(
              i.asBox.minPoint,
              i.horizontal.start.isUnbounded,
              i.vertical.start.isUnbounded,
              i.depth.start.isUnbounded
            )
            val newMax = priorMax.projectAfterBounded(
              i.asBox.maxPoint,
              i.horizontal.end.isUnbounded,
              i.vertical.end.isUnbounded,
              i.depth.end.isUnbounded
            )
            (newMin, newMax)

      val margin = Coordinate(1, 1, 1)
      val clipWithin = Box(minBounded - margin, maxBounded + margin)
      if hasUnbounded then println(s"Unbounded intervals clipped to coordinate box $clipWithin")
      val dataParameters = validData.map: d =>
        val box = d.interval.asBox
        val (minParam, maxParam) = (box.minPoint.toUrlFragment(clipWithin), box.maxPoint.toUrlFragment(clipWithin))
        val (text1Param, text2Param) = (d.value.toString, d.interval.toString)
        val param = s"""{"min":$minParam,"max":$maxParam,"text1":"$text1Param","text2":"$text2Param"}"""
        URLEncoder.encode(param, StandardCharsets.UTF_8)

      val titleParameter = URLEncoder.encode(title, StandardCharsets.UTF_8)
      val result = runServer(rootDir).flatMap: _ =>
        openInBrowser(s"http://localhost:$port?title=$titleParameter&${dataParameters.mkString("data=[", ",", "]")}")
      result match
        case Failure(e) => e.printStackTrace()
        case Success(_) => ()

  def apply[V, R1: DomainValueLike, R2: DomainValueLike, R3: DomainValueLike](
    dataIn3D: DimensionalBase.In3D[V, R1, R2, R3],
    delay: Long = 0,
    title: String = "Visualize 3D data"
  ): Unit =
    apply(dataIn3D.getAll, title)
    Thread.sleep(delay)

  private def openInBrowser(url: String): Try[Unit] =
    val result = Try:
      if !Desktop.isDesktopSupported then
        println("Desktop is not supported.")
        println(s"Please manually open this URL in your browser: $url")
      else
        val desktop = Desktop.getDesktop
        if !desktop.isSupported(Desktop.Action.BROWSE) then
          println("Desktop 'browse' action is not supported.")
          println(s"Please manually open this URL in your browser: $url")
        else
          println(s"Attempting to open URL in default browser: $url")
          desktop.browse(new URI(url))
          println("Browser launch command issued.")

    result.recoverWith:
      case NonFatal(e) =>
        println(s"Error opening browser: ${e.getMessage}")
        result

  private def runServer(rootDir: Path): Try[Unit] =
    val result = Try:
      val server = HttpServer.create(new InetSocketAddress(port), 0)
      server.createContext("/", new StaticFileHandler(rootDir))
      server.setExecutor(null) // Use default executor
      server.start()

      println(s"HTTP server started on http://localhost:$port")
      println(s"Serving files from: $rootDir")
      println(s"Stop the running process (e.g., by pressing Ctrl+C) to stop the server.")

      // Keep the server running until interrupted
      Runtime.getRuntime.addShutdownHook(new Thread(() =>
        println("Stopping server...")
        server.stop(0)
        println("Server stopped.")
      ))

    result.recoverWith:
      case NonFatal(e) =>
        println(s"Could not start server: ${e.getMessage}")
        result

  private class StaticFileHandler(rootDir: Path) extends HttpHandler:
    private val MimeTypes = Map(
      ".html" -> "text/html",
      ".js" -> "application/javascript",
      ".css" -> "text/css" // ,
      // ".json" -> "application/json",
      // ".png" -> "image/png",
      // ".jpg" -> "image/jpeg",
      // ".gif" -> "image/gif",
      // ".svg" -> "image/svg+xml"
    ).withDefaultValue("application/octet-stream")

    override def handle(exchange: HttpExchange): Unit =
      def sendContent(filePath: Path): Unit =
        val extension = filePath.getFileName.toString.lastIndexOf('.') match {
          case -1 => ""
          case i  => filePath.getFileName.toString.substring(i)
        }
        val contentType = MimeTypes(extension.toLowerCase)
        exchange.getResponseHeaders.set("Content-Type", contentType)
        exchange.sendResponseHeaders(200, Files.size(filePath))
        val os = exchange.getResponseBody
        Files.copy(filePath, os)
        os.close()

      def sendNotFound(message: String): Unit =
        val response = s"<h1>404 Not Found</h1><p>$message</p>".getBytes("UTF-8")
        exchange.getResponseHeaders.set("Content-Type", "text/html; charset=UTF-8")
        exchange.sendResponseHeaders(404, response.length)
        val os = exchange.getResponseBody
        os.write(response)
        os.close()

      def sendError(message: String): Unit =
        val response = s"<h1>500 Internal Server Error</h1><p>$message</p>".getBytes("UTF-8")
        exchange.getResponseHeaders.set("Content-Type", "text/html; charset=UTF-8")
        exchange.sendResponseHeaders(500, response.length)
        val os = exchange.getResponseBody
        os.write(response)
        os.close()

      // Default to index.html if requesting root
      val requestURI = exchange.getRequestURI.getPath
      val effectivePath = if requestURI == "/" || requestURI.isEmpty then "/index.html" else requestURI
      try
        val filePath = rootDir.resolve(effectivePath.stripPrefix("/"))
        if Files.exists(filePath) && !Files.isDirectory(filePath) then sendContent(filePath)
        else sendNotFound(s"File not found: $effectivePath")
      catch
        case NonFatal(e) =>
          println("Server error:")
          e.printStackTrace()
          sendError(s"Server error: ${e.getMessage}")
      finally exchange.close()

  def main(args: Array[String]): Unit =
    import intervalidus.DiscreteValue.given
    import intervalidus.Interval1D.*
    import scala.language.implicitConversions
    val data = intervalidus.immutable.Data(
      Seq(
        (intervalFrom(-6).to(5) x intervalFrom(0).to(5) x intervalFrom(0).to(5)) -> "Hello",
        (intervalFrom(5).to(10) x intervalFrom(-1).to(6) x intervalFrom(-1).to(6)) -> "World"
      )
    )
    Visualize3D(data)
