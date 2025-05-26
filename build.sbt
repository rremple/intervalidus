ThisBuild / scalaVersion := "3.3.5"

ThisBuild / organization := "rremple" // necessary for the sbt-ghpages and sbt-github-packages
ThisBuild / githubOwner := "rremple"
ThisBuild / githubRepository := "intervalidus"
ThisBuild / publishTo := Some(
  MavenRepository("GitHub Packages", s"https://maven.pkg.github.com/${githubOwner.value}/${githubRepository.value}")
)
publishMavenStyle := true
githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES")

def commonSettings = Seq(
  scalacOptions ++= Seq("-feature", "-deprecation"), // , "-Wunused:all"),
  githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES"),
  coverageFailOnMinimum := true,
  coverageMinimumStmtTotal := 99,
  coverageMinimumBranchTotal := 99,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

lazy val core = project
  .enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
  .settings(commonSettings)
  .settings(
    name := "intervalidus",
    git.remoteRepo := "git@github.com:rremple/intervalidus.git"
  )

lazy val `intervalidus-weepickle` = (project in file("json/weepickle"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "intervalidus-weepickle",
    libraryDependencies += "com.rallyhealth" %% "weepickle-v1" % "1.9.1"
  )

lazy val `intervalidus-upickle` = (project in file("json/upickle"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "intervalidus-upickle",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.0.2"
  )

lazy val `intervalidus-tinyrule` = (project in file("sidequests/tinyrule"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "intervalidus-tinyrule"
  )

lazy val `intervalidus-examples` = (project in file("examples"))
  .dependsOn(core, `intervalidus-tinyrule`)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    Compile / packageDoc / publishArtifact := false,
    coverageEnabled := false
)

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    Compile / packageDoc / publishArtifact := false,
    coverageEnabled := false
  )

publish / skip := true // ROOT