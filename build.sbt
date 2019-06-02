name := "tetris-cli"

version := "0.1"

scalaVersion := "2.12.8"

lazy val app = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "TetrisCli",
    mainClass in assembly := Some("com.septa97.tetriscli.Tetris")
  )
