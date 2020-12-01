import sbt._

case object Dependencies {
  lazy val scalaV = "2.12.12"
  lazy val propensive = "com.propensive" %% "magnolia" % "0.17.0"
  lazy val ScalaReflect = "org.scala-lang" % "scala-reflect" % scalaV % Provided
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "2.2.2"
  lazy val sext = "com.github.nikita-volkov" % "sext" % "0.2.4"
  lazy val monix = "io.monix" %% "monix" % "3.2.2+44-ab4c068c"
  lazy val JacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.11.3"
  lazy val JacksonDatabind = "com.fasterxml.jackson.core" % "jackson-databind" % "2.11.3"
}
