import sbt.Def

import java.net.URI
import javax.xml.parsers.DocumentBuilderFactory

def latestScala3Nightly(): Option[String] =
  try
    val url = URI
      .create(
        "https://repo.scala-lang.org/artifactory/maven-nightlies/org/scala-lang/scala3-compiler_3/maven-metadata.xml"
      )
      .toURL
    val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
    val nodes = builder.parse(url.openStream()).getElementsByTagName("latest")
    if nodes.getLength == 0 then None
    else
      val latest = nodes.item(0).getTextContent
      println(s"Found latest: $latest")
      Some(latest)
  catch case _: Exception => None

//resolvers += Resolver.scalaNightlyRepository
//val scalaVersionLastKnownGood = "3.10.0-RC1-bin-20260812-7adc7af-NIGHTLY"
//ThisBuild / scalaVersion := latestScala3Nightly().getOrElse(scalaVersionLastKnownGood)
ThisBuild / scalaVersion := "3.9.0"

ThisBuild / organization := "io.github.rremple"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible // None // as a stopgap

// Ignore dynamic version suffixes for internal subproject dependencies
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+\\+\\d+".r)

// =========================================================================================
// NATIVE MAVEN CENTRAL (SONATYPE CENTRAL) PUBLISHING SETTINGS
// =========================================================================================
ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (version.value.endsWith("-SNAPSHOT")) Some("central-snapshots" at centralSnapshots)
  else localStaging.value // Native sbt 1.11+ zero-plugin staging engine
}

// POM Metadata (Mandatory for Maven Central validation)
ThisBuild / homepage := Some(uri("https://github.com/rremple/intervalidus"))
ThisBuild / licenses := List("Apache-2.0" -> uri("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers := List(
  Developer(
    id = "rremple",
    name = "Russell Remple",
    email = "rremple@users.noreply.github.com",
    url = uri("https://github.com/rremple")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    uri("https://github.com/rremple/intervalidus"),
    "scm:git@github.com:rremple/intervalidus.git"
  )
)

// =========================================================================================
// SUBPROJECT CONFIGURATIONS
// =========================================================================================

def commonSettings(projectName: String): Seq[Def.Setting[?]] = Seq(
  name := projectName,
  description := s"Intervalidus, for all your interval-based data needs: $projectName module",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  scalacOptions ++= Seq("-source", "future"),
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-Wunused:privates",
    "-Wunused:locals",
    "-Wunused:explicits", // not :implicits or :params -- too many false positives
    "-Wunused:nowarn"
  ),
  // scalacOptions ++= Seq("-Werror"),
  coverageFailOnMinimum := true,
  coverageMinimumStmtTotal := 99,
  coverageMinimumBranchTotal := 99,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test
)

def commonPublishSettings(projectName: String): Seq[Def.Setting[?]] = commonSettings(projectName)

def commonNoPublishSettings(projectName: String): Seq[Def.Setting[?]] = commonSettings(projectName) ++ Seq(
  publish / skip := true,
  Compile / packageDoc / publishArtifact := false,
  coverageEnabled := false
)

lazy val siteTarget = settingKey[File]("Full site target directory")
lazy val siteRevision = settingKey[String]("Revision for documentation site links (usually just master)")

@transient
lazy val makeSite = taskKey[Seq[File]]("Generate unified docs and inject the landing page")
@transient
lazy val siteCheckAll = taskKey[Unit]("Scans generated HTML for Scaladoc issues")

// Projects included in published API documentation and tested by default
lazy val docProjects = Seq(
  core,
  collection,
  `intervalidus-pickle`,
  `intervalidus-weepickle`,
  `intervalidus-upickle`,
  `intervalidus-circe`,
  `intervalidus-play`,
  `intervalidus-tinyrule`
)
// Projects that are tested by default, but not included in published API documentation
// (note that the laws project isn't included here, so laws tests won't be run by default)
lazy val nonDocProjects = Seq(
  `intervalidus-examples`,
  `intervalidus-example-mongodb`,
  bench
)
lazy val allProjects = (docProjects ++ nonDocProjects)

// Project extends ProjectReference
def referencesTo(ps: Seq[Project]): Seq[ProjectReference] = ps.map(identity)

lazy val root = (project in file("."))
  .disablePlugins(MimaPlugin)
  .aggregate(referencesTo(allProjects)*)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    name := "intervalidus-root",
    versionPolicyCheck / aggregate := true,
    publish / skip := true,
    siteTarget := baseDirectory.value / "target" / "site",
    siteRevision := {
      dynverGitDescribeOutput.value match
        case Some(gitDescription) =>
          val ref = gitDescription.ref.value
          if ref != "0.0.0" then ref else gitDescription.commitSuffix.sha
        case None =>
          "master"
    },
    makeSite := {
      val log = streams.value.log
      log.info(s"Making site using siteRevision ${siteRevision.value}")
      val sourceFile = baseDirectory.value / "src" / "site" / "index.html"
      val targetFile = siteTarget.value / "index.html"
      val copiedFiles =
        if !sourceFile.exists() then
          log.warn(s"Source missing: $sourceFile")
          Seq()
        else
          IO.copyFile(sourceFile, targetFile)
          Seq(targetFile)
      (Compile / unidoc).value ++ copiedFiles
    },
    ScalaUnidoc / unidoc / target := siteTarget.value / "api",
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-title", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-version", version.value),
    ScalaUnidoc / unidoc / scalacOptions += {
      val rootBase = baseDirectory.value
      val subprojectBases = docProjects.map(_.base)
      val sourcePath = "src/main/scala"
      val sourceLinkMappings = subprojectBases.map: subprojectBase =>
        val absoluteBase = subprojectBase.getAbsolutePath
        val relativeRepoPath = rootBase.toURI.relativize(subprojectBase.toURI).getPath.stripSuffix("/")
        val repoSubPath = if relativeRepoPath.isEmpty then sourcePath else s"$relativeRepoPath/$sourcePath"
        s"$absoluteBase/$sourcePath=github://rremple/intervalidus/${siteRevision.value}#$repoSubPath"
      s"-source-links:${sourceLinkMappings.mkString(",")}"
    },
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(referencesTo(docProjects)*),
    // Documentation checks (because sometimes my scaladoc symbolic references get broken)
    siteCheckAll := {
      val log = streams.value.log

      // Ensure the site is built first
      // Calling .value forces SBT to run these tasks completely before proceeding.
      val unifiedDocs = makeSite.value
      val siteDir = (ScalaUnidoc / unidoc / target).value

      log.info(s"Scanning site HTML in $siteDir for issues...")
      // Find all HTML files recursively
      val htmlFiles = (siteDir ** "*.html").get()
      var brokenCount = 0

      htmlFiles.foreach: file =>
        val lines = IO.readLines(file)
        // log.info(s"Scanning ${file.getName}...")
        lines.zipWithIndex.foreach: (line, index) =>
          // Matches things like <p>$symbol or similar unexpanded patterns
          if line.contains("<p>$") then
            log.warn(s"Scaladoc-related issue found in ${file.getName}:${index + 1} -> $line")
            brokenCount += 1

      if brokenCount == 0 then log.info("Site looks clean.")
      else
        log.error(s"Found $brokenCount site issues.")
        throw new MessageOnlyException(s"Site check failed with $brokenCount errors.")
    }
  )

lazy val core = project
  .dependsOn(collection)
  .settings(commonPublishSettings("intervalidus"))
  .settings(Test / discoveredMainClasses := Seq()) // ignore multiple examples

lazy val collection = project
  .settings(commonPublishSettings("intervalidus-collection"))

lazy val `intervalidus-pickle` = (project in file("json/common"))
  .dependsOn(core)
  .settings(commonPublishSettings("intervalidus-pickle-common"))

val jackson2Version = "2.22.2"
lazy val `intervalidus-weepickle` = (project in file("json/weepickle"))
  .dependsOn(core, `intervalidus-pickle` % "compile->compile;test->test")
  .settings(commonPublishSettings("intervalidus-weepickle"))
  .settings(
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % jackson2Version,
    libraryDependencies += "com.rallyhealth" %% "weepickle-v1" % "1.9.1"
  )

lazy val `intervalidus-upickle` = (project in file("json/upickle"))
  .dependsOn(core, `intervalidus-pickle` % "compile->compile;test->test")
  .settings(commonPublishSettings("intervalidus-upickle"))
  .settings(
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.3"
  )

lazy val `intervalidus-circe` = (project in file("json/circe"))
  .dependsOn(core, `intervalidus-pickle` % "test->test")
  .settings(commonPublishSettings("intervalidus-circe"))
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.16",
      "io.circe" %% "circe-parser" % "0.14.16" % Test
    )
  )

lazy val `intervalidus-play` = (project in file("json/play"))
  .dependsOn(core, `intervalidus-pickle` % "test->test")
  .settings(commonPublishSettings("intervalidus-play"))
  .settings(
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.6",
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % jackson2Version,
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % jackson2Version
  )

lazy val `intervalidus-tinyrule` = (project in file("sidequests/tinyrule"))
  .settings(commonPublishSettings("intervalidus-tinyrule"))

lazy val `intervalidus-examples` = (project in file("examples"))
  .disablePlugins(MimaPlugin)
  .dependsOn(core, `intervalidus-tinyrule`)
  .settings(commonNoPublishSettings("intervalidus-examples"))
  .settings(Compile / discoveredMainClasses := Seq()) // ignore multiple examples

val mongodbVersion = "5.11.0"
val testcontainersVersion = "0.44.1"

lazy val `intervalidus-example-mongodb` = (project in file("example-mongodb"))
  .disablePlugins(MimaPlugin)
  .dependsOn(core, `intervalidus-weepickle`, `intervalidus-upickle`, `intervalidus-circe`, `intervalidus-play`)
  .settings(
    commonNoPublishSettings("intervalidus-example-mongodb"),
    libraryDependencies ++= Seq(
      "org.mongodb" % "bson" % mongodbVersion,
      "org.mongodb" % "mongodb-driver-sync" % mongodbVersion,
      "com.dimafeng" %% "testcontainers-scala-scalatest" % testcontainersVersion % Test,
      "com.dimafeng" %% "testcontainers-scala-mongodb" % testcontainersVersion % Test,
      "org.slf4j" % "slf4j-nop" % "2.0.19" % Test
    )
  )

lazy val laws = project
  .disablePlugins(MimaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings("laws"))
  .settings(
    Test / parallelExecution := true,
    Test / fork := true,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings("bench"))
