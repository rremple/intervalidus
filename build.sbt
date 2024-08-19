ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "intervalidus",
    scalacOptions += "-feature",
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 99,
    coverageMinimumBranchTotal := 99,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
