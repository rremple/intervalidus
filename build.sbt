ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"
def commonSettings = Seq(
  scalacOptions ++= Seq("-feature", "-deprecation"), // , "-Wunused:all"),
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
    organization := "rremple", // necessary for the GhpagesPlugin
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
  .settings(
    publish / skip := true,
    Compile / packageDoc / publishArtifact := false,
    coverageEnabled := false
  )
