import sbt.Def

ThisBuild / scalaVersion := "3.3.8"

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
ThisBuild / homepage := Some(url("https://github.com/rremple/intervalidus"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers := List(
  Developer(
    id = "rremple",
    name = "Russell Remple",
    email = "rremple@users.noreply.github.com",
    url = url("https://github.com/rremple")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rremple/intervalidus"),
    "scm:git@github.com:rremple/intervalidus.git"
  )
)

// =========================================================================================
// SUBPROJECT CONFIGURATIONS
// =========================================================================================

def commonSettings(projectName: String): Seq[Def.Setting[_]] = Seq(
  name := projectName,
  description := s"Intervalidus, for all your interval-based data needs: $projectName module",
  scalacOptions ++= Seq("-feature", "-deprecation"), // , "-Wunused:all", "-source", "future"),
  coverageFailOnMinimum := true,
  coverageMinimumStmtTotal := 99,
  coverageMinimumBranchTotal := 99,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % Test
)

def commonPublishSettings(projectName: String): Seq[Def.Setting[_]] = commonSettings(projectName) ++ Seq(
  tastyMiMaPreviousArtifacts := mimaPreviousArtifacts.value,
  tastyMiMaReportIssues := {
    if (versionPolicyIntention.value == Compatibility.None) // e.g., major release
      streams.value.log.info(s"TASTy compatibility check skipped for ${name.value}.")
    else tastyMiMaReportIssues.value
  }
)

def commonNoPublishSettings(projectName: String): Seq[Def.Setting[_]] = commonSettings(projectName) ++ Seq(
  publish / skip := true,
  Compile / packageDoc / publishArtifact := false,
  coverageEnabled := false
)

lazy val makeSite = taskKey[Seq[File]]("Generate unified docs and inject the landing page")
lazy val siteTarget = settingKey[File]("Full site target directory")

lazy val root = (project in file("."))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .aggregate(
    core,
    collection,
    `intervalidus-pickle`,
    `intervalidus-weepickle`,
    `intervalidus-upickle`,
    `intervalidus-tinyrule`,
    `intervalidus-examples`,
    `intervalidus-example-mongodb`,
    bench
  )
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    name := "intervalidus-root",
    versionPolicyCheck / aggregate := true,
    publish / skip := true,
    siteTarget := baseDirectory.value / "target" / "site",
    makeSite := {
      val log = streams.value.log
      val sourceFile = baseDirectory.value / "src" / "site" / "index.html"
      val targetFile = siteTarget.value / "index.html"
      val copiedFiles = if (!sourceFile.exists()) {
        log.warn(s"Source missing: $sourceFile")
        Seq()
      } else {
        IO.copyFile(sourceFile, targetFile)
        Seq(targetFile)
      }
      (Compile / unidoc).value ++ copiedFiles
    },
    ScalaUnidoc / unidoc / target := siteTarget.value / "api",
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-title", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-version", version.value),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-source-links:github://rremple/intervalidus",
      "-revision",
      dynverGitDescribeOutput.value.map(_.ref.value).getOrElse("master")
    ),
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      `intervalidus-examples`,
      `intervalidus-example-mongodb`,
      laws,
      bench
    )
  )

lazy val core = project
  .dependsOn(collection)
  .settings(commonPublishSettings("intervalidus"))

lazy val collection = project
  .settings(commonPublishSettings("intervalidus-collection"))

lazy val `intervalidus-pickle` = (project in file("json/common"))
  .dependsOn(core)
  .settings(commonPublishSettings("intervalidus-pickle-common"))

lazy val `intervalidus-weepickle` = (project in file("json/weepickle"))
  .dependsOn(core, `intervalidus-pickle` % "compile->compile;test->test")
  .settings(commonPublishSettings("intervalidus-weepickle"))
  .settings(
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.22.0",
    libraryDependencies += "com.rallyhealth" %% "weepickle-v1" % "1.9.1"
  )

lazy val `intervalidus-upickle` = (project in file("json/upickle"))
  .dependsOn(core, `intervalidus-pickle` % "compile->compile;test->test")
  .settings(commonPublishSettings("intervalidus-upickle"))
  .settings(
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.3"
  )

lazy val `intervalidus-tinyrule` = (project in file("sidequests/tinyrule"))
  .settings(commonPublishSettings("intervalidus-tinyrule"))

lazy val `intervalidus-examples` = (project in file("examples"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core, `intervalidus-tinyrule`)
  .settings(commonNoPublishSettings("intervalidus-examples"))

val mongodbVersion = "5.8.0"
val testcontainersVersion = "0.44.1"

lazy val `intervalidus-example-mongodb` = (project in file("example-mongodb"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core, `intervalidus-weepickle`, `intervalidus-upickle`)
  .settings(
    commonNoPublishSettings("intervalidus-example-mongodb"),
    libraryDependencies ++= Seq(
      "org.mongodb" % "bson" % mongodbVersion,
      "org.mongodb" % "mongodb-driver-sync" % mongodbVersion,
      "com.dimafeng" %% "testcontainers-scala-scalatest" % testcontainersVersion % Test,
      "com.dimafeng" %% "testcontainers-scala-mongodb" % testcontainersVersion % Test,
      "org.slf4j" % "slf4j-nop" % "2.0.18" % Test
    )
  )

lazy val laws = project
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings("laws"))
  .settings(
    Test / parallelExecution := true,
    Test / fork := true,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings("bench"))

// =========================================================================================
// DOCUMENTATION CHECKS (because sometimes my scaladoc symbolic references get broken)
// =========================================================================================

lazy val siteCheckAll = taskKey[Unit]("Scans generated HTML for Scaladoc issues")

siteCheckAll := {
  val log = streams.value.log

  // Ensure the site is built first
  // Calling .value forces SBT to run these tasks completely before proceeding.
  log.info("Making site...")
  val unifiedDocs = (Compile / unidoc).value
  val siteDir = (ScalaUnidoc / unidoc / target).value

  log.info(s"Scanning site HTML in $siteDir for issues...")
  // Find all HTML files recursively
  val htmlFiles = (siteDir ** "*.html").get
  var brokenCount = 0

  htmlFiles.foreach { file =>
    val lines = IO.readLines(file)
    // log.info(s"Scanning ${file.getName}...")
    lines.zipWithIndex.foreach { case (line, index) =>
      // Matches things like <p>$symbol or similar unexpanded patterns
      if (line.contains("<p>$")) {
        log.warn(s"Scaladoc-related issue found in ${file.getName}:${index + 1} -> $line")
        brokenCount += 1
      }
    }
  }

  if (brokenCount == 0) {
    log.info("Site looks clean.")
  } else {
    log.error(s"Found $brokenCount site issues.")
    throw new MessageOnlyException(s"Site check failed with $brokenCount errors.")
  }
}
