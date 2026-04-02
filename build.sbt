ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "PPM_projeto_1"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
