ThisBuild / name := "graphql4s"
ThisBuild / scalaVersion := "2.12.12"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "jsoft.graphql4s"
ThisBuild / organizationName := "jsoft"
ThisBuild / scalacOptions := Seq("-language:implicitConversions")
ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

libraryDependencies += "com.propensive" %% "magnolia" % "0.17.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
// https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-core
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.11.3"
// https://mvnrepository.com/artifact/com.chuusai/shapeless
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"
libraryDependencies += "io.circe" %% "circe-parser" % "0.13.0"
libraryDependencies += "io.circe" %% "circe-generic" % "0.13.0"
libraryDependencies += "io.circe" %% "circe-core" % "0.13.0"
libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.4"
//libraryDependencies += "dev.zio" %% "zio" % "1.0.1"
libraryDependencies += "io.monix" %% "monix" % "3.2.2+44-ab4c068c"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.30"

