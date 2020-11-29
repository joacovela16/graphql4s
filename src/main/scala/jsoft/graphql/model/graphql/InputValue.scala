package jsoft.graphql.model.graphql

final case class InputValue(`type`: ()=> Type, name: String, description: Option[String] = None, defaultValue: Option[String] = None)
