import sbt._

case object Dependencies {

  private final val akkaVersion: String = "2.6.14"

  lazy val libName = "graphql4s"
  lazy val scalaV = "2.12.12"
  val scalaVersions: Seq[String] = Seq("2.12.13", "2.13.4")
  lazy val propensive = "com.propensive" %% "magnolia" % "0.17.0"
  lazy val ScalaReflect = "org.scala-lang" % "scala-reflect" % scalaV
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "2.2.2"
  lazy val sext = "com.github.nikita-volkov" % "sext" % "0.2.4"
  lazy val monix = "io.monix" %% "monix" % "3.2.2+44-ab4c068c"
  lazy val JacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.11.3"
  lazy val JacksonDatabind = "com.fasterxml.jackson.core" % "jackson-databind" % "2.11.3"
  lazy val jacksonModuleScala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.11.3"
  lazy val Slf4jApi = "org.slf4j" % "slf4j-api" % "1.7.30"
  lazy val akkaStream = "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion
}
