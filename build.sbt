Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "slox",
    libraryDependencies := Seq(
      "com.lihaoyi" %% "pprint" % "0.9.0"
    ),
    assembly / mainClass := Some("org.willena.slox.run"),
  )
