import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

scalacOptions ++= "-deprecation" :: "-feature" :: "-Xlint" :: Nil
scalacOptions in (Compile, console) ~= {_.filterNot(_ == "-Xlint")}

lazy val root = (project in file("."))
  .settings(
    name := "notation-structure",
    libraryDependencies += scalaTest % Test
  )

lazy val midi = (project in file("midi"))
  .settings(
    name := "notation-structure-midi",
    libraryDependencies += scalaTest % Test,
    fork in run := true,
    connectInput in run := true
  )
  .dependsOn(root)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
