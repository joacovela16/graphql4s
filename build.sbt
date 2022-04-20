import Dependencies._

Global / onChangedBuildSource := ReloadOnSourceChanges
version := "0.1.3"
organization := "io.github.joacovela16"
organizationName := "jsoft"
homepage := Some(url("https://github.com/joacovela16/graphql4s"))
scalacOptions := Seq("-language:implicitConversions")
licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

ThisBuild / sonatypeSessionName := s"[sbt-sonatype] ${name.value}-${scalaBinaryVersion.value}-${version.value}"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org"
ThisBuild / publishTo := sonatypePublishTo.value

ThisBuild / scmInfo := Some(
	ScmInfo(
		url("https://github.com/joacovela16/graphql4s"),
		"scm:git@github.com:joacovela16/graphql4s.git"
	)
)

ThisBuild / developers := List(
	Developer(
		id = "joacovela16",
		name = "Joaquin",
		email = "joaquinvelazquezcamacho@gmail.com",
		url = url("https://github.com/joacovela16")
	)
)

lazy val root = (project in file("."))
	.settings(
		name := libName,
		crossScalaVersions := Dependencies.scalaVersions,
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
		)
	)