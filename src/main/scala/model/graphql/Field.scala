package model.graphql

final case class Field(
                        _type: Type,
                        name: String,
                        description: Option[String] = None,
                        args: Seq[InputValue] = Nil,
                        isDeprecated: Boolean = false,
                        deprecationReason: Option[String] = None
                      )
