Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version      := "1.2.0"
ThisBuild / scalaVersion := "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name                       := "slox",
    assembly / mainClass       := Some("org.willena.slox.run"),
    assembly / assemblyJarName := "slox.jar",
  )
