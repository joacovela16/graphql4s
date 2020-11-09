package modux.graphql.model.graphql

final case class EnumValue(name: String, description: Option[String] = None, isDeprecated: Boolean = false, deprecationReason: Option[String])
