Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "slox"
  )
