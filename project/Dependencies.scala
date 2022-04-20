import sbt._

case object Dependencies {

  private final val akkaVersion: String = "2.6.19"

  lazy val libName: String = "graphql4s"
  lazy val scalaV: String = "2.13.8"
  val scalaVersions: Seq[String] = Seq("2.12.15", "2.13.8")
  lazy val propensive = "com.propensive" %% "magnolia" % "0.17.0"
  lazy val ScalaReflect = "org.scala-lang" % "scala-reflect" % scalaV
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "2.3.3"
  lazy val sext = "com.github.nikita-volkov" % "sext" % "0.2.6"
  lazy val monix = "io.monix" %% "monix" % "3.4.0"
  lazy val JacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.13.2"
  lazy val JacksonDatabind = "com.fasterxml.jackson.core" % "jackson-databind" % "2.13.2.2"
  lazy val jacksonModuleScala = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.2"
  lazy val Slf4jApi = "org.slf4j" % "slf4j-api" % "1.7.36"
  lazy val akkaStream = "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion
}
