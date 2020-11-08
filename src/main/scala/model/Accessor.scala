package model

trait Accessor {
  def apply(key: String): Option[String]

  def query: Option[String]
}
