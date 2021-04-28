import Dependencies._

val localVersion = "0.1.2"
ThisBuild / scalaVersion := scalaV
ThisBuild / version := localVersion
ThisBuild / organization := "jsoft.graphql4s"
ThisBuild / organizationName := "jsoft"
ThisBuild / scalacOptions := Seq("-language:implicitConversions")
ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / bintrayReleaseOnPublish := false

lazy val disablingPublishingSettings = Seq(publish / skip := true, publishArtifact := false)

lazy val enablingPublishingSettings = Seq(
  publishArtifact := true,
  publishMavenStyle := true,
  // http://www.scala-sbt.org/0.12.2/docs/Detailed-Topics/Artifacts.html
  Test / publishArtifact := false,
  // Bintray
  bintrayPackageLabels := Seq("scala", "sbt"),
  bintrayRepository := "maven",
  bintrayVcsUrl := Option("https://github.com/joacovela16/modux"),
  bintrayOrganization := Option("jsoft"),
)

lazy val root = (project in file("."))
  .settings(
    name := libName,
    libraryDependencies ++= Seq(
      propensive,
      ScalaReflect,
      fastparse,
      akkaStream,
      jacksonModuleScala,
      sext,
      monix,
      JacksonCore,
      JacksonDatabind,
      Slf4jApi
    ),
    enablingPublishingSettings
  )