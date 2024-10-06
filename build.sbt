ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
  .settings(
    name := "intervalidus",
    organization := "rremple", // necessary for the GhpagesPlugin
    git.remoteRepo := "git@github.com:rremple/intervalidus.git",
    scalacOptions += "-feature",
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 99,
    coverageMinimumBranchTotal := 99,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test // and nothing else!
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .dependsOn(root)
  .settings(
    publish / skip := true,
    Compile / packageDoc / publishArtifact := false,
    coverageEnabled := false
  )
