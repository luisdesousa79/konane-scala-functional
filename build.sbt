ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "JG2_LuisSousa129329_MarceloOliveira111874_NelssyPina123572"
  )
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

libraryDependencies += "org.openjfx" % "javafx-base" % "25.0.2"
libraryDependencies += "org.openjfx" % "javafx-controls" % "25.0.2"
libraryDependencies += "org.openjfx" % "javafx-fxml" % "25.0.2"