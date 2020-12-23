import Dependencies._

ThisBuild / name := libName
ThisBuild / scalaVersion := scalaV
ThisBuild / version := "0.1.0"
ThisBuild / organization := "jsoft.graphql4s"
ThisBuild / organizationName := "jsoft"
ThisBuild / scalacOptions := Seq("-language:implicitConversions")
ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))


Global / onChangedBuildSource := ReloadOnSourceChanges
bintrayReleaseOnPublish in ThisBuild := false


lazy val disablingPublishingSettings = Seq(skip in publish := true, publishArtifact := false)

lazy val enablingPublishingSettings = Seq(
  publishArtifact := true,
  publishMavenStyle := true,
  // http://www.scala-sbt.org/0.12.2/docs/Detailed-Topics/Artifacts.html
  publishArtifact in Test := false,
  // Bintray
  bintrayPackageLabels := Seq("scala", "sbt"),
  bintrayRepository := "maven",
  bintrayVcsUrl := Option("https://github.com/joacovela16/modux"),
  bintrayOrganization := Option("jsoft"),
)

lazy val root = (project in file("."))
  .settings(
    name:= libName,
    version in publish := "0.1.1",
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