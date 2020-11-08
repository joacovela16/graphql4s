package model.graphql

final case class InputValue(_type: Type, name: String, description: Option[String] = None, defaultValue: Option[String] = None)
