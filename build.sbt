import sbt.Def

ThisBuild / scalaVersion := "3.3.7"

ThisBuild / organization := "rremple" // necessary for the sbt-ghpages and sbt-github-packages
ThisBuild / githubOwner := (ThisBuild / organization).value
ThisBuild / githubRepository := "intervalidus"
ThisBuild / publishTo := githubPublishTo.value // based on githubOwner and githubRepository

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible // None // as a stopgap
// Ignore dynamic version suffixes for internal sub-project dependencies
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+\\+\\d+".r)

publishMavenStyle := true

def commonSettings(projectName: String): Seq[Def.Setting[_]] = Seq(
  name := projectName,
  scalacOptions ++= Seq("-feature", "-deprecation"), // , "-Wunused:all", "-source", "future"),
  githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES"),
  coverageFailOnMinimum := true,
  coverageMinimumStmtTotal := 99,
  coverageMinimumBranchTotal := 99,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
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
    `bench`
  )
  .enablePlugins(ScalaUnidocPlugin, SiteScaladocPlugin, GhpagesPlugin)
  .settings(
    name := "intervalidus-root",
    githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES"),
    versionPolicyCheck / aggregate := true,
    publish / skip := true,
    // Publish unified API to the GitHub Pages site: unidoc; makeSite; ghpagesPushSite
    //git.remoteRepo := "git@github.com:rremple/intervalidus.git",
    git.remoteRepo :=
      s"https://${githubTokenSource.value}@github.com/${githubOwner.value}/${githubRepository.value}.git",
    SiteScaladoc / siteSubdirName := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, SiteScaladoc / siteSubdirName),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-project", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-title", "Intervalidus API"),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq("-doc-version", version.value),
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      `intervalidus-examples`,
      `intervalidus-example-mongodb`,
      `bench`
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

val mongodbVersion = "5.6.3"
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
      "org.slf4j" % "slf4j-nop" % "2.0.17" % Test
    )
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings("bench"))
