package jsoft.graphql.lib

import scala.language.implicitConversions

trait ImplicitUtils {
  implicit def asOption[A](t: A): Option[A] = Option(t)
}
