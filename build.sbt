ThisBuild / scalaVersion := "3.3.6"

ThisBuild / organization := "rremple" // necessary for the sbt-ghpages and sbt-github-packages
ThisBuild / githubOwner := "rremple"
ThisBuild / githubRepository := "intervalidus"
ThisBuild / publishTo := Some(
  "GitHub Packages" at s"https://maven.pkg.github.com/${githubOwner.value}/${githubRepository.value}"
)
publishMavenStyle := true

def commonSettings = Seq(
  scalacOptions ++= Seq("-feature", "-deprecation"), // , "-Wunused:all"),
  githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES"),
  coverageFailOnMinimum := true,
  coverageMinimumStmtTotal := 99,
  coverageMinimumBranchTotal := 99,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
def commonPublishSettings = commonSettings ++ Seq(
  versionScheme := Some("early-semver"),
  mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
  tastyMiMaPreviousArtifacts := mimaPreviousArtifacts.value
)
def commonNoPublishSettings = commonSettings ++ Seq(
  publish / skip := true,
  Compile / packageDoc / publishArtifact := false,
  coverageEnabled := false
)

lazy val root = (project in file("."))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .aggregate(
    core,
    collection,
    `intervalidus-weepickle`,
    `intervalidus-upickle`,
    `intervalidus-tinyrule`,
    `intervalidus-examples`,
    `intervalidus-example-mongodb`,
    `bench`
  )
  .settings(
    name := "intervalidus-root",
    githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("PUBLISH_TO_PACKAGES"),
    publish / skip := true
  )

lazy val core = project
  .enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
  .disablePlugins(MimaPlugin, TastyMiMaPlugin) // for now (pre 1.0)
  .dependsOn(collection)
  .settings(commonPublishSettings)
  .settings(
    name := "intervalidus",
    git.remoteRepo := "git@github.com:rremple/intervalidus.git"
  )

lazy val collection = project
  .disablePlugins(MimaPlugin, TastyMiMaPlugin) // for now (pre 1.0)
  .settings(commonPublishSettings)
  .settings(
    name := "intervalidus-collection"
  )

lazy val `intervalidus-weepickle` = (project in file("json/weepickle"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin) // for now (pre 1.0)
  .dependsOn(core)
  .settings(commonPublishSettings)
  .settings(
    name := "intervalidus-weepickle",
    libraryDependencies += "com.rallyhealth" %% "weepickle-v1" % "1.9.1"
  )

lazy val `intervalidus-upickle` = (project in file("json/upickle"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin) // for now (pre 1.0)
  .dependsOn(core)
  .settings(commonPublishSettings)
  .settings(
    name := "intervalidus-upickle",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.3.2"
  )

lazy val `intervalidus-tinyrule` = (project in file("sidequests/tinyrule"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin) // for now (pre 1.0)
  .settings(commonPublishSettings)
  .settings(
    name := "intervalidus-tinyrule"
  )

lazy val `intervalidus-examples` = (project in file("examples"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core, `intervalidus-tinyrule`)
  .settings(commonNoPublishSettings)

lazy val `intervalidus-example-mongodb` = (project in file("example-mongodb"))
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core, `intervalidus-weepickle`, `intervalidus-upickle`)
  .settings(
    commonNoPublishSettings,
    libraryDependencies ++= Seq(
      "org.mongodb" % "bson" % "5.6.1",
      "org.mongodb" % "mongodb-driver-sync" % "5.6.1",
      "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.43.0" % Test,
      "com.dimafeng" %% "testcontainers-scala-mongodb" % "0.43.0" % Test,
      "org.slf4j" % "slf4j-nop" % "2.0.17" % Test
    )
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .disablePlugins(MimaPlugin, TastyMiMaPlugin)
  .dependsOn(core)
  .settings(commonNoPublishSettings)
