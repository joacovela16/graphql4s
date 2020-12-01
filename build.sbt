import Dependencies._

ThisBuild / name := "graphql4s"
ThisBuild / scalaVersion := scalaV
ThisBuild / version := "0.1.0"
ThisBuild / organization := "jsoft.graphql4s"
ThisBuild / organizationName := "jsoft"
ThisBuild / scalacOptions := Seq("-language:implicitConversions")
ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      propensive,
      ScalaReflect,
      fastparse,
      sext,
      monix,
      JacksonCore,
      JacksonDatabind
    )
  )