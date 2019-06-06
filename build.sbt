ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "com.septa97"

lazy val app = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "TetrisCli",
    mainClass in assembly := Some("com.septa97.tetriscli.Tetris")
  )
